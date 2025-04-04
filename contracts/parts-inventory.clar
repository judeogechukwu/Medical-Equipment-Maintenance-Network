;; Parts Inventory Contract
;; Tracks availability of replacement components

;; Error codes
(define-constant ERR_UNAUTHORIZED u100)
(define-constant ERR_ALREADY_EXISTS u101)
(define-constant ERR_NOT_FOUND u102)
(define-constant ERR_INSUFFICIENT_QUANTITY u103)

;; Data structures
(define-map parts
  { part-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 100),
    compatible-devices: (list 10 uint),
    quantity: uint,
    reorder-threshold: uint,
    supplier-id: uint
  }
)

(define-map part-usage-history
  { usage-id: uint }
  {
    part-id: uint,
    device-id: uint,
    service-id: uint,
    quantity: uint,
    usage-date: uint
  }
)

(define-map device-parts
  { device-id: uint }
  { part-ids: (list 100 uint) }
)

(define-map supplier-parts
  { supplier-id: uint }
  { part-ids: (list 100 uint) }
)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Read-only functions
(define-read-only (get-part (part-id uint))
  (map-get? parts { part-id: part-id })
)

(define-read-only (get-part-usage (usage-id uint))
  (map-get? part-usage-history { usage-id: usage-id })
)

(define-read-only (get-device-parts (device-id uint))
  (default-to { part-ids: (list) } (map-get? device-parts { device-id: device-id }))
)

(define-read-only (get-supplier-parts (supplier-id uint))
  (default-to { part-ids: (list) } (map-get? supplier-parts { supplier-id: supplier-id }))
)

(define-read-only (check-part-availability (part-id uint) (quantity-needed uint))
  (let (
    (part (get-part part-id))
  )
    (if (is-some part)
      (>= (get quantity (unwrap-panic part)) quantity-needed)
      false
    )
  )
)

;; Public functions
(define-public (register-part
    (part-id uint)
    (name (string-ascii 50))
    (description (string-ascii 100))
    (compatible-devices (list 10 uint))
    (initial-quantity uint)
    (reorder-threshold uint)
    (supplier-id uint)
  )
  (let (
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))
    (asserts! (is-none (get-part part-id)) (err ERR_ALREADY_EXISTS))

    ;; Add part to parts map
    (map-set parts
      { part-id: part-id }
      {
        name: name,
        description: description,
        compatible-devices: compatible-devices,
        quantity: initial-quantity,
        reorder-threshold: reorder-threshold,
        supplier-id: supplier-id
      }
    )

    ;; Add part to each compatible device
    (fold add-part-to-device compatible-devices part-id)

    ;; Add part to supplier
    (let (
      (supplier-part-list (get-supplier-parts supplier-id))
    )
      (map-set supplier-parts
        { supplier-id: supplier-id }
        { part-ids: (unwrap-panic (as-max-len? (append (get part-ids supplier-part-list) part-id) u100)) }
      )
    )

    (ok part-id)
  )
)

(define-private (add-part-to-device (device-id uint) (part-id uint))
  (let (
    (device-part-list (get-device-parts device-id))
  )
    (map-set device-parts
      { device-id: device-id }
      { part-ids: (unwrap-panic (as-max-len? (append (get part-ids device-part-list) part-id) u100)) }
    )
    part-id
  )
)

(define-public (update-part-quantity (part-id uint) (new-quantity uint))
  (let (
    (part (unwrap! (get-part part-id) (err ERR_NOT_FOUND)))
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))

    (map-set parts
      { part-id: part-id }
      (merge part { quantity: new-quantity })
    )

    (ok true)
  )
)

(define-public (use-parts (usage-id uint) (part-id uint) (device-id uint) (service-id uint) (quantity uint))
  (let (
    (part (unwrap! (get-part part-id) (err ERR_NOT_FOUND)))
    (caller tx-sender)
    (current-quantity (get quantity part))
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))
    (asserts! (>= current-quantity quantity) (err ERR_INSUFFICIENT_QUANTITY))

    ;; Record usage
    (map-set part-usage-history
      { usage-id: usage-id }
      {
        part-id: part-id,
        device-id: device-id,
        service-id: service-id,
        quantity: quantity,
        usage-date: (unwrap-panic (get-block-info? time u0))
      }
    )

    ;; Update inventory
    (map-set parts
      { part-id: part-id }
      (merge part { quantity: (- current-quantity quantity) })
    )

    (ok true)
  )
)

(define-public (restock-parts (part-id uint) (quantity-to-add uint))
  (let (
    (part (unwrap! (get-part part-id) (err ERR_NOT_FOUND)))
    (caller tx-sender)
    (current-quantity (get quantity part))
  )
    (asserts! (is-eq caller (var-get contract-owner)) (err ERR_UNAUTHORIZED))

    (map-set parts
      { part-id: part-id }
      (merge part { quantity: (+ current-quantity quantity-to-add) })
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
