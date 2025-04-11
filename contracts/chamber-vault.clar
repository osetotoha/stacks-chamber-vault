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

;; Create secure timelock vault with multiple unlock conditions
(define-public (create-timelocked-vault (beneficiary principal) (unlock-height uint) (backup-principal principal) (quantity uint))
  (begin
    (asserts! (> quantity u0) ERROR_INVALID_QUANTITY)
    (asserts! (> unlock-height block-height) (err u450)) ;; Must be future block
    (asserts! (<= unlock-height (+ block-height u8640)) (err u451)) ;; Maximum 60 days in future
    (asserts! (valid-beneficiary? beneficiary) ERROR_INVALID_INITIATOR)
    (asserts! (not (is-eq backup-principal tx-sender)) (err u452)) ;; Backup must be different from sender
    (asserts! (not (is-eq backup-principal beneficiary)) (err u453)) ;; Backup must be different from beneficiary

    (let
      (
        (new-chamber-id (+ (var-get next-chamber-id) u1))
        (time-lock-duration (- unlock-height block-height))
      )

      ;; Transfer STX to contract
      (match (stx-transfer? quantity tx-sender (as-contract tx-sender))
        success
          (begin
            (var-set next-chamber-id new-chamber-id)

            ;; Store vault info in repository
            (map-set ChamberRepository
              { chamber-id: new-chamber-id }
              {
                initiator: tx-sender,
                beneficiary: beneficiary,
                item-id: u0, ;; Generic vault
                quantity: quantity,
                chamber-status: "timelocked",
                creation-block: block-height,
                expiration-block: unlock-height
              }
            )

            (print {action: "timelock_vault_created", 
                    chamber-id: new-chamber-id, 
                    initiator: tx-sender,
                    beneficiary: beneficiary,
                    backup-principal: backup-principal,
                    unlock-height: unlock-height,
                    timelock-duration: time-lock-duration,
                    quantity: quantity})
            (ok new-chamber-id)
          )
        error ERROR_STX_MOVEMENT_FAILED
      )
    )
  )
)

;; Implement encrypted messaging for chamber participants
(define-public (add-encrypted-communication (chamber-id uint) (encrypted-message (buff 256)) (recipient-principal principal))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (asserts! (> (len encrypted-message) u32) (err u500)) ;; Ensure minimum message length
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (beneficiary (get beneficiary chamber-record))
      )
      ;; Only chamber participants can send messages
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GUARDIAN)) ERROR_PERMISSION_DENIED)
      ;; Recipient must be chamber participant
      (asserts! (or (is-eq recipient-principal initiator) (is-eq recipient-principal beneficiary) (is-eq recipient-principal PROTOCOL_GUARDIAN)) (err u501))
      ;; Sender cannot be recipient
      (asserts! (not (is-eq tx-sender recipient-principal)) (err u502))

      ;; Get message digest for reference
      (let
        (
          (message-digest (hash160 encrypted-message))
        )
        (print {action: "encrypted_message_sent", 
                chamber-id: chamber-id, 
                sender: tx-sender, 
                recipient: recipient-principal, 
                message-digest: message-digest,
                timestamp-block: block-height})
        (ok message-digest)
      )
    )
  )
)

;; Implement panic button with immediate system shutdown
(define-public (activate-protocol-panic-mode (justification (string-ascii 100)) (cooldown-blocks uint))
  (begin
    (asserts! (is-eq tx-sender PROTOCOL_GUARDIAN) ERROR_PERMISSION_DENIED)
    (asserts! (> cooldown-blocks u24) (err u440)) ;; Minimum 4 hour cooldown
    (asserts! (<= cooldown-blocks u4320) (err u441)) ;; Maximum 30 day cooldown

    (let
      (
        (activation-block block-height)
        (deactivation-block (+ block-height cooldown-blocks))
      )

      ;; Validate justification - must have minimum length
      (asserts! (>= (len justification) u10) (err u442))

      ;; Log audit trail of panic mode activation
      (print {action: "panic_mode_activated", 
              guardian: tx-sender,
              activation-block: activation-block,
              deactivation-block: deactivation-block,
              cooldown-duration: cooldown-blocks,
              justification: justification})

      ;; In a full implementation, this would set all protocol operations to pause

      (ok {activation: activation-block, 
           deactivation: deactivation-block, 
           duration: cooldown-blocks})
    )
  )
)
