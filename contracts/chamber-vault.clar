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

