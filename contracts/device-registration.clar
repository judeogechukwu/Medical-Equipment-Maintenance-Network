;; Device Registration Contract
;; Manages medical equipment registration for hospitals

;; Error codes
(define-constant ERR_UNAUTHORIZED u100)
(define-constant ERR_ALREADY_EXISTS u101)
(define-constant ERR_NOT_FOUND u102)

;; Data structures
(define-map devices
  { device-id: uint }
  {
    name: (string-ascii 50),
    model: (string-ascii 50),
    manufacturer: (string-ascii 50),
    purchase-date: uint,
    warranty-expiry: uint,
    last-maintenance: uint,
    status: uint
  }
)

(define-map hospital-devices
  { hospital-id: uint }
  { device-ids: (list 100 uint) }
)

;; Hospital authorization
(define-map authorized-hospitals
  { hospital-id: uint }
  { authorized: bool }
)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Read-only functions
(define-read-only (get-device (device-id uint))
  (map-get? devices { device-id: device-id })
)

(define-read-only (get-hospital-devices (hospital-id uint))
  (default-to { device-ids: (list) } (map-get? hospital-devices { hospital-id: hospital-id }))
)

(define-read-only (is-hospital-authorized (hospital-id uint))
  (default-to false (get authorized (map-get? authorized-hospitals { hospital-id: hospital-id })))
)

;; Public functions
(define-public (register-device
    (device-id uint)
    (name (string-ascii 50))
    (model (string-ascii 50))
    (manufacturer (string-ascii 50))
    (purchase-date uint)
    (warranty-expiry uint)
    (hospital-id uint)
  )
  (let (
    (caller tx-sender)
    (hospital-authorized (is-hospital-authorized hospital-id))
  )
    (asserts! (or (is-eq caller (var-get contract-owner)) hospital-authorized) (err ERR_UNAUTHORIZED))
    (asserts! (is-none (get-device device-id)) (err ERR_ALREADY_EXISTS))

    ;; Add device to devices map
    (map-set devices
      { device-id: device-id }
      {
        name: name,
        model: model,
        manufacturer: manufacturer,
        purchase-date: purchase-date,
        warranty-expiry: warranty-expiry,
        last-maintenance: u0,
        status: u1
      }
    )

    ;; Add device to hospital's device list
    (let (
      (current-devices (get-hospital-devices hospital-id))
    )
      (map-set hospital-devices
        { hospital-id: hospital-id }
        { device-ids: (unwrap-panic (as-max-len? (append (get device-ids current-devices) device-id) u100)) }
      )
    )

    (ok device-id)
  )
)

(define-public (update-device-status (device-id uint) (new-status uint))
  (let (
    (device (unwrap! (get-device device-id) (err ERR_NOT_FOUND)))
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))

    (map-set devices
      { device-id: device-id }
      (merge device { status: new-status })
    )

    (ok true)
  )
)

(define-public (update-maintenance-date (device-id uint) (maintenance-date uint))
  (let (
    (device (unwrap! (get-device device-id) (err ERR_NOT_FOUND)))
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))

    (map-set devices
      { device-id: device-id }
      (merge device { last-maintenance: maintenance-date })
    )

    (ok true)
  )
)

(define-public (authorize-hospital (hospital-id uint))
  (let (
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))

    (map-set authorized-hospitals
      { hospital-id: hospital-id }
      { authorized: true }
    )

    (ok true)
  )
)

(define-public (revoke-hospital (hospital-id uint))
  (let (
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))

    (map-set authorized-hospitals
      { hospital-id: hospital-id }
      { authorized: false }
    )

    (ok true)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (let (
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))
    (var-set contract-owner new-owner)
    (ok true)
  )
)
