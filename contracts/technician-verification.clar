;; Technician Verification Contract
;; Validates qualifications for specific devices

;; Error codes
(define-constant ERR_UNAUTHORIZED u100)
(define-constant ERR_ALREADY_EXISTS u101)
(define-constant ERR_NOT_FOUND u102)
(define-constant ERR_EXPIRED u103)

;; Data structures
(define-map technicians
  { technician-id: uint }
  {
    name: (string-ascii 50),
    license-number: (string-ascii 20),
    specializations: (list 10 (string-ascii 30)),
    certification-expiry: uint,
    status: uint
  }
)

(define-map device-technicians
  { device-type: (string-ascii 30) }
  { technician-ids: (list 100 uint) }
)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Read-only functions
(define-read-only (get-technician (technician-id uint))
  (map-get? technicians { technician-id: technician-id })
)

(define-read-only (get-device-technicians (device-type (string-ascii 30)))
  (default-to { technician-ids: (list) } (map-get? device-technicians { device-type: device-type }))
)

(define-read-only (is-technician-certified (technician-id uint) (current-time uint))
  (let (
    (technician (get-technician technician-id))
  )
    (and
      (is-some technician)
      (let (
        (tech (unwrap-panic technician))
      )
        (and
          (> (get certification-expiry tech) current-time)
          (is-eq (get status tech) u1)
        )
      )
    )
  )
)

;; Public functions
(define-public (register-technician
    (technician-id uint)
    (name (string-ascii 50))
    (license-number (string-ascii 20))
    (specializations (list 10 (string-ascii 30)))
    (certification-expiry uint)
  )
  (let (
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))
    (asserts! (is-none (get-technician technician-id)) (err ERR_ALREADY_EXISTS))

    ;; Add technician to technicians map
    (map-set technicians
      { technician-id: technician-id }
      {
        name: name,
        license-number: license-number,
        specializations: specializations,
        certification-expiry: certification-expiry,
        status: u1
      }
    )

    ;; Add technician to each device type they specialize in
    (fold add-technician-to-device-type specializations technician-id)

    (ok technician-id)
  )
)

(define-private (add-technician-to-device-type (device-type (string-ascii 30)) (technician-id uint))
  (let (
    (current-technicians (get-device-technicians device-type))
  )
    (map-set device-technicians
      { device-type: device-type }
      { technician-ids: (unwrap-panic (as-max-len? (append (get technician-ids current-technicians) technician-id) u100)) }
    )
    technician-id
  )
)

(define-public (update-technician-status (technician-id uint) (new-status uint))
  (let (
    (technician (unwrap! (get-technician technician-id) (err ERR_NOT_FOUND)))
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))

    (map-set technicians
      { technician-id: technician-id }
      (merge technician { status: new-status })
    )

    (ok true)
  )
)

(define-public (update-certification-expiry (technician-id uint) (new-expiry uint))
  (let (
    (technician (unwrap! (get-technician technician-id) (err ERR_NOT_FOUND)))
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))

    (map-set technicians
      { technician-id: technician-id }
      (merge technician { certification-expiry: new-expiry })
    )

    (ok true)
  )
)

(define-public (verify-technician-for-service (technician-id uint) (device-type (string-ascii 30)) (current-time uint))
  (let (
    (technician (unwrap! (get-technician technician-id) (err ERR_NOT_FOUND)))
  )
    (asserts! (> (get certification-expiry technician) current-time) (err ERR_EXPIRED))
    (asserts! (is-eq (get status technician) u1) (err ERR_UNAUTHORIZED))

    ;; Check if technician specializes in this device type
    (asserts! (is-some (index-of (get specializations technician) device-type)) (err ERR_UNAUTHORIZED))

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
