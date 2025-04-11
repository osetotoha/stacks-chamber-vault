;; Stacks Chamber Network - Value Storage Protocol

;; Chamber repository data structure
(define-map ChamberRepository
  { chamber-id: uint }
  {
    initiator: principal,
    beneficiary: principal,
    item-id: uint,
    quantity: uint,
    chamber-status: (string-ascii 10),
    creation-block: uint,
    expiration-block: uint
  }
)

;; Registry counter for chambers
(define-data-var next-chamber-id uint u0)

;; Utility functions for validation
(define-private (valid-beneficiary? (beneficiary principal))
  (and 
    (not (is-eq beneficiary tx-sender))
    (not (is-eq beneficiary (as-contract tx-sender)))
  )
)

;; Core system parameters
(define-constant PROTOCOL_GUARDIAN tx-sender)
(define-constant ERROR_PERMISSION_DENIED (err u100))
(define-constant ERROR_NO_CHAMBER (err u101))
(define-constant ERROR_ALREADY_PROCESSED (err u102))
(define-constant ERROR_STX_MOVEMENT_FAILED (err u103))
(define-constant ERROR_INVALID_IDENTIFIER (err u104))
(define-constant ERROR_INVALID_QUANTITY (err u105))
(define-constant ERROR_INVALID_INITIATOR (err u106))
(define-constant ERROR_CHAMBER_TIMEOUT (err u107))
(define-constant CHAMBER_LIFESPAN_BLOCKS u1008)

(define-private (valid-chamber-id? (chamber-id uint))
  (<= chamber-id (var-get next-chamber-id))
)

;; Protocol operational functions

;; Finalize transfer to beneficiary
(define-public (finalize-chamber-transfer (chamber-id uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (beneficiary (get beneficiary chamber-record))
        (quantity (get quantity chamber-record))
        (item (get item-id chamber-record))
      )
      (asserts! (or (is-eq tx-sender PROTOCOL_GUARDIAN) (is-eq tx-sender (get initiator chamber-record))) ERROR_PERMISSION_DENIED)
      (asserts! (is-eq (get chamber-status chamber-record) "pending") ERROR_ALREADY_PROCESSED)
      (asserts! (<= block-height (get expiration-block chamber-record)) ERROR_CHAMBER_TIMEOUT)
      (match (as-contract (stx-transfer? quantity tx-sender beneficiary))
        success
          (begin
            (map-set ChamberRepository
              { chamber-id: chamber-id }
              (merge chamber-record { chamber-status: "completed" })
            )
            (print {action: "chamber_transferred", chamber-id: chamber-id, beneficiary: beneficiary, item-id: item, quantity: quantity})
            (ok true)
          )
        error ERROR_STX_MOVEMENT_FAILED
      )
    )
  )
)

;; Return items to initiator
(define-public (return-chamber-contents (chamber-id uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (quantity (get quantity chamber-record))
      )
      (asserts! (is-eq tx-sender PROTOCOL_GUARDIAN) ERROR_PERMISSION_DENIED)
      (asserts! (is-eq (get chamber-status chamber-record) "pending") ERROR_ALREADY_PROCESSED)
      (match (as-contract (stx-transfer? quantity tx-sender initiator))
        success
          (begin
            (map-set ChamberRepository
              { chamber-id: chamber-id }
              (merge chamber-record { chamber-status: "returned" })
            )
            (print {action: "contents_returned", chamber-id: chamber-id, initiator: initiator, quantity: quantity})
            (ok true)
          )
        error ERROR_STX_MOVEMENT_FAILED
      )
    )
  )
)

;; Initiator requests chamber nullification
(define-public (nullify-chamber (chamber-id uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (quantity (get quantity chamber-record))
      )
      (asserts! (is-eq tx-sender initiator) ERROR_PERMISSION_DENIED)
      (asserts! (is-eq (get chamber-status chamber-record) "pending") ERROR_ALREADY_PROCESSED)
      (asserts! (<= block-height (get expiration-block chamber-record)) ERROR_CHAMBER_TIMEOUT)
      (match (as-contract (stx-transfer? quantity tx-sender initiator))
        success
          (begin
            (map-set ChamberRepository
              { chamber-id: chamber-id }
              (merge chamber-record { chamber-status: "nullified" })
            )
            (print {action: "chamber_nullified", chamber-id: chamber-id, initiator: initiator, quantity: quantity})
            (ok true)
          )
        error ERROR_STX_MOVEMENT_FAILED
      )
    )
  )
)
