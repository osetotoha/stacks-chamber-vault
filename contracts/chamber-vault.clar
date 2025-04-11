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

;; Implement dynamic fee adjustment based on chamber value
(define-public (adjust-chamber-fees (chamber-id uint) (fee-percentage uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (asserts! (<= fee-percentage u10) (err u520)) ;; Max 10% fee
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (quantity (get quantity chamber-record))
        (status (get chamber-status chamber-record))
      )
      ;; Only guardian can adjust fees
      (asserts! (is-eq tx-sender PROTOCOL_GUARDIAN) ERROR_PERMISSION_DENIED)
      ;; Only for pending chambers
      (asserts! (is-eq status "pending") ERROR_ALREADY_PROCESSED)

      ;; Calculate fee amount
      (let
        (
          (fee-amount (/ (* quantity fee-percentage) u100))
          (adjusted-quantity (- quantity fee-amount))
        )
        ;; Ensure minimum quantity remains
        (asserts! (> adjusted-quantity u0) (err u521))

        ;; Update chamber with new quantity
        (map-set ChamberRepository
          { chamber-id: chamber-id }
          (merge chamber-record { quantity: adjusted-quantity })
        )

        (print {action: "fees_adjusted", 
                chamber-id: chamber-id, 
                original-quantity: quantity, 
                fee-percentage: fee-percentage, 
                fee-amount: fee-amount, 
                adjusted-quantity: adjusted-quantity})
        (ok fee-amount)
      )
    )
  )
)

;; Implement selective disclosure verification
(define-public (verify-selective-disclosure (chamber-id uint) (disclosure-hash (buff 32)) (verification-path (list 10 (buff 32))))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (asserts! (> (len verification-path) u0) (err u530))
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (beneficiary (get beneficiary chamber-record))
        (status (get chamber-status chamber-record))
      )
      ;; Only chamber participants can verify disclosures
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender beneficiary)) ERROR_PERMISSION_DENIED)
      ;; Chamber must be active
      (asserts! (or (is-eq status "pending") (is-eq status "accepted")) ERROR_ALREADY_PROCESSED)

      ;; Perform Merkle path verification (simplified for this example)
      (let
        (
          (root-hash (fold hash-combine verification-path disclosure-hash))
        )
        (print {action: "selective_disclosure_verified", 
                chamber-id: chamber-id, 
                verifier: tx-sender, 
                disclosure-hash: disclosure-hash, 
                merkle-root: root-hash,
                path-length: (len verification-path)})
        (ok root-hash)
      )
    )
  )
)

;; Helper function for Merkle path verification
(define-private (hash-combine (path-element (buff 32)) (current-hash (buff 32)))
  (hash160 (concat current-hash path-element))
)


;; Helper function to process chamber merging
(define-private (process-chamber-merge (source-id uint) (result {quantity: uint, count: uint}))
  (let
    (
      (chamber-record (map-get? ChamberRepository { chamber-id: source-id }))
    )
    (match chamber-record
      source-chamber
        (let
          (
            (source-status (get chamber-status source-chamber))
            (source-quantity (get quantity source-chamber))
          )
          ;; Only merge chambers that are in pending status
          (if (is-eq source-status "pending")
            (begin
              ;; Mark source chamber as merged
              (map-set ChamberRepository
                { chamber-id: source-id }
                (merge source-chamber { chamber-status: "merged", quantity: u0 })
              )
              ;; Add quantity to result
              {quantity: (+ (get quantity result) source-quantity), count: (+ (get count result) u1)}
            )
            ;; Keep existing result if chamber can't be merged
            result
          )
        )
      ;; Chamber doesn't exist, keep existing result
      result
    )
  )
)

 ;; Helper function to create fragments
 (define-private (create-fragment (percentage uint) 
                                 (state {parent-id: uint, 
                                        current-id: uint, 
                                        parent-quantity: uint,
                                        fragments: (list 5 uint)}))
   (let
     (
       (parent-id (get parent-id state))
       (current-id (get current-id state))
       (parent-quantity (get parent-quantity state))
       (fragments (get fragments state))
       (next-id (+ current-id u1))
       (fragment-quantity (/ (* parent-quantity percentage) u100))
       (parent-chamber (unwrap-panic (map-get? ChamberRepository { chamber-id: parent-id })))
     )
     ;; Create new fragment chamber
     (map-set ChamberRepository
       { chamber-id: next-id }
       {
         initiator: (get initiator parent-chamber),
         beneficiary: (get beneficiary parent-chamber),
         item-id: (get item-id parent-chamber),
         quantity: fragment-quantity,
         chamber-status: "pending",
         creation-block: block-height,
         expiration-block: (get expiration-block parent-chamber)
       }
     )

     ;; Return updated state
     {
       parent-id: parent-id,
       current-id: next-id,
       parent-quantity: parent-quantity,
       fragments: (unwrap-panic (as-max-len? (append fragments next-id) u5))
     }
   )
 )

 ;; Implement chamber fragmentation for partial transfers
 (define-public (fragment-chamber (chamber-id uint) (fragment-percentages (list 5 uint)))
   (begin
     (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
     (asserts! (> (len fragment-percentages) u0) (err u600))
     (let
       (
         (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
         (initiator (get initiator chamber-record))
         (quantity (get quantity chamber-record))
         (status (get chamber-status chamber-record))
         (total-percentage (fold + fragment-percentages u0))
       )
       ;; Only initiator can fragment a chamber
       (asserts! (is-eq tx-sender initiator) ERROR_PERMISSION_DENIED)
       ;; Chamber must be pending
       (asserts! (is-eq status "pending") ERROR_ALREADY_PROCESSED)
       ;; Total percentage must equal 100
       (asserts! (is-eq total-percentage u100) (err u601))
       ;; Maximum 5 fragments
       (asserts! (<= (len fragment-percentages) u5) (err u602))

       ;; Generate new fragment chamber IDs
       (let
         (
           (fragment-result (fold create-fragment fragment-percentages 
                               {parent-id: chamber-id, 
                                current-id: (var-get next-chamber-id), 
                                parent-quantity: quantity,
                                fragments: (list )}))
           (new-last-id (get current-id fragment-result))
           (fragment-ids (get fragments fragment-result))
         )
         ;; Update next chamber ID
         (var-set next-chamber-id new-last-id)

         ;; Mark original chamber as fragmented
         (map-set ChamberRepository
           { chamber-id: chamber-id }
           (merge chamber-record { chamber-status: "fragmented", quantity: u0 })
         )

         (print {action: "chamber_fragmented", 
                 parent-chamber: chamber-id, 
                 fragment-percentages: fragment-percentages, 
                 fragment-ids: fragment-ids,
                 total-fragments: (len fragment-ids)})
         (ok fragment-ids)
       )
     )
   )
 )

;; Prolong chamber existence
(define-public (prolong-chamber-lifespan (chamber-id uint) (additional-blocks uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (asserts! (> additional-blocks u0) ERROR_INVALID_QUANTITY)
    (asserts! (<= additional-blocks u1440) ERROR_INVALID_QUANTITY) ;; Max ~10 days extension
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record)) 
        (beneficiary (get beneficiary chamber-record))
        (current-expiration (get expiration-block chamber-record))
        (updated-expiration (+ current-expiration additional-blocks))
      )
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GUARDIAN)) ERROR_PERMISSION_DENIED)
      (asserts! (or (is-eq (get chamber-status chamber-record) "pending") (is-eq (get chamber-status chamber-record) "accepted")) ERROR_ALREADY_PROCESSED)
      (map-set ChamberRepository
        { chamber-id: chamber-id }
        (merge chamber-record { expiration-block: updated-expiration })
      )
      (print {action: "chamber_prolonged", chamber-id: chamber-id, requestor: tx-sender, new-expiration-block: updated-expiration})
      (ok true)
    )
  )
)

;; Collect expired chamber contents
(define-public (collect-expired-chamber (chamber-id uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (quantity (get quantity chamber-record))
        (expiry (get expiration-block chamber-record))
      )
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender PROTOCOL_GUARDIAN)) ERROR_PERMISSION_DENIED)
      (asserts! (or (is-eq (get chamber-status chamber-record) "pending") (is-eq (get chamber-status chamber-record) "accepted")) ERROR_ALREADY_PROCESSED)
      (asserts! (> block-height expiry) (err u108)) ;; Must be expired
      (match (as-contract (stx-transfer? quantity tx-sender initiator))
        success
          (begin
            (map-set ChamberRepository
              { chamber-id: chamber-id }
              (merge chamber-record { chamber-status: "expired" })
            )
            (print {action: "expired_chamber_collected", chamber-id: chamber-id, initiator: initiator, quantity: quantity})
            (ok true)
          )
        error ERROR_STX_MOVEMENT_FAILED
      )
    )
  )
)

;; Initiate chamber challenge
(define-public (challenge-chamber (chamber-id uint) (justification (string-ascii 50)))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (beneficiary (get beneficiary chamber-record))
      )
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender beneficiary)) ERROR_PERMISSION_DENIED)
      (asserts! (or (is-eq (get chamber-status chamber-record) "pending") (is-eq (get chamber-status chamber-record) "accepted")) ERROR_ALREADY_PROCESSED)
      (asserts! (<= block-height (get expiration-block chamber-record)) ERROR_CHAMBER_TIMEOUT)
      (map-set ChamberRepository
        { chamber-id: chamber-id }
        (merge chamber-record { chamber-status: "challenged" })
      )
      (print {action: "chamber_challenged", chamber-id: chamber-id, challenger: tx-sender, justification: justification})
      (ok true)
    )
  )
)

;; Add cryptographic verification
(define-public (add-cryptographic-verification (chamber-id uint) (cryptographic-proof (buff 65)))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (beneficiary (get beneficiary chamber-record))
      )
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender beneficiary)) ERROR_PERMISSION_DENIED)
      (asserts! (or (is-eq (get chamber-status chamber-record) "pending") (is-eq (get chamber-status chamber-record) "accepted")) ERROR_ALREADY_PROCESSED)
      (print {action: "crypto_proof_verified", chamber-id: chamber-id, verifier: tx-sender, proof: cryptographic-proof})
      (ok true)
    )
  )
)

;; Resolve challenge with mediation
(define-public (mediate-challenge (chamber-id uint) (initiator-allocation uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (asserts! (is-eq tx-sender PROTOCOL_GUARDIAN) ERROR_PERMISSION_DENIED)
    (asserts! (<= initiator-allocation u100) ERROR_INVALID_QUANTITY) ;; Percentage must be 0-100
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (beneficiary (get beneficiary chamber-record))
        (quantity (get quantity chamber-record))
        (initiator-portion (/ (* quantity initiator-allocation) u100))
        (beneficiary-portion (- quantity initiator-portion))
      )
      (asserts! (is-eq (get chamber-status chamber-record) "challenged") (err u112)) ;; Must be challenged
      (asserts! (<= block-height (get expiration-block chamber-record)) ERROR_CHAMBER_TIMEOUT)

      ;; Send initiator's portion
      (unwrap! (as-contract (stx-transfer? initiator-portion tx-sender initiator)) ERROR_STX_MOVEMENT_FAILED)

      ;; Send beneficiary's portion
      (unwrap! (as-contract (stx-transfer? beneficiary-portion tx-sender beneficiary)) ERROR_STX_MOVEMENT_FAILED)

      (map-set ChamberRepository
        { chamber-id: chamber-id }
        (merge chamber-record { chamber-status: "mediated" })
      )
      (print {action: "challenge_mediated", chamber-id: chamber-id, initiator: initiator, beneficiary: beneficiary, 
              initiator-portion: initiator-portion, beneficiary-portion: beneficiary-portion, initiator-allocation: initiator-allocation})
      (ok true)
    )
  )
)

;; Register enhanced verification for substantial chambers
(define-public (register-enhanced-verification (chamber-id uint) (verifier principal))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (quantity (get quantity chamber-record))
      )
      ;; Only for substantial chambers (> 1000 STX)
      (asserts! (> quantity u1000) (err u120))
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender PROTOCOL_GUARDIAN)) ERROR_PERMISSION_DENIED)
      (asserts! (is-eq (get chamber-status chamber-record) "pending") ERROR_ALREADY_PROCESSED)
      (print {action: "verification_registered", chamber-id: chamber-id, verifier: verifier, requestor: tx-sender})
      (ok true)
    )
  )
)

;; Create incremental delivery chamber
(define-public (create-incremental-chamber (beneficiary principal) (item-id uint) (quantity uint) (increments uint))
  (let 
    (
      (new-id (+ (var-get next-chamber-id) u1))
      (expiration-date (+ block-height CHAMBER_LIFESPAN_BLOCKS))
      (increment-quantity (/ quantity increments))
    )
    (asserts! (> quantity u0) ERROR_INVALID_QUANTITY)
    (asserts! (> increments u0) ERROR_INVALID_QUANTITY)
    (asserts! (<= increments u5) ERROR_INVALID_QUANTITY) ;; Max 5 increments
    (asserts! (valid-beneficiary? beneficiary) ERROR_INVALID_INITIATOR)
    (asserts! (is-eq (* increment-quantity increments) quantity) (err u121)) ;; Ensure even division
    (match (stx-transfer? quantity tx-sender (as-contract tx-sender))
      success
        (begin
          (var-set next-chamber-id new-id)

          (print {action: "incremental_chamber_created", chamber-id: new-id, initiator: tx-sender, beneficiary: beneficiary, 
                  item-id: item-id, quantity: quantity, increments: increments, increment-quantity: increment-quantity})
          (ok new-id)
        )
      error ERROR_STX_MOVEMENT_FAILED
    )
  )
)

;; Register fallback address
(define-public (register-fallback-address (chamber-id uint) (fallback-address principal))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
      )
      (asserts! (is-eq tx-sender initiator) ERROR_PERMISSION_DENIED)
      (asserts! (not (is-eq fallback-address tx-sender)) (err u111)) ;; Fallback address must be different
      (asserts! (is-eq (get chamber-status chamber-record) "pending") ERROR_ALREADY_PROCESSED)
      (print {action: "fallback_registered", chamber-id: chamber-id, initiator: initiator, fallback: fallback-address})
      (ok true)
    )
  )
)

;; Lock questionable chamber
(define-public (lock-questionable-chamber (chamber-id uint) (justification (string-ascii 100)))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (beneficiary (get beneficiary chamber-record))
      )
      (asserts! (or (is-eq tx-sender PROTOCOL_GUARDIAN) (is-eq tx-sender initiator) (is-eq tx-sender beneficiary)) ERROR_PERMISSION_DENIED)
      (asserts! (or (is-eq (get chamber-status chamber-record) "pending") 
                   (is-eq (get chamber-status chamber-record) "accepted")) 
                ERROR_ALREADY_PROCESSED)
      (map-set ChamberRepository
        { chamber-id: chamber-id }
        (merge chamber-record { chamber-status: "locked" })
      )
      (print {action: "chamber_locked", chamber-id: chamber-id, reporter: tx-sender, justification: justification})
      (ok true)
    )
  )
)

;; Activate advanced verification for substantial chambers
(define-public (activate-advanced-verification (chamber-id uint) (verification-code (buff 32)))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (quantity (get quantity chamber-record))
      )
      ;; Only for chambers above threshold
      (asserts! (> quantity u5000) (err u130))
      (asserts! (is-eq tx-sender initiator) ERROR_PERMISSION_DENIED)
      (asserts! (is-eq (get chamber-status chamber-record) "pending") ERROR_ALREADY_PROCESSED)
      (print {action: "advanced_verification_activated", chamber-id: chamber-id, initiator: initiator, verification-hash: (hash160 verification-code)})
      (ok true)
    )
  )
)

;; Cryptographic transaction verification
(define-public (perform-crypto-verification (chamber-id uint) (message-digest (buff 32)) (crypto-signature (buff 65)) (signatory principal))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (beneficiary (get beneficiary chamber-record))
        (verification-result (unwrap! (secp256k1-recover? message-digest crypto-signature) (err u150)))
      )
      ;; Verify with cryptographic proof
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GUARDIAN)) ERROR_PERMISSION_DENIED)
      (asserts! (or (is-eq signatory initiator) (is-eq signatory beneficiary)) (err u151))
      (asserts! (is-eq (get chamber-status chamber-record) "pending") ERROR_ALREADY_PROCESSED)

      ;; Verify signature matches expected signatory
      (asserts! (is-eq (unwrap! (principal-of? verification-result) (err u152)) signatory) (err u153))

      (print {action: "crypto_verification_completed", chamber-id: chamber-id, verifier: tx-sender, signatory: signatory})
      (ok true)
    )
  )
)


;; Attach chamber information packet
(define-public (attach-information-packet (chamber-id uint) (packet-type (string-ascii 20)) (packet-digest (buff 32)))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (beneficiary (get beneficiary chamber-record))
      )
      ;; Only authorized parties can add information packets
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GUARDIAN)) ERROR_PERMISSION_DENIED)
      (asserts! (not (is-eq (get chamber-status chamber-record) "completed")) (err u160))
      (asserts! (not (is-eq (get chamber-status chamber-record) "returned")) (err u161))
      (asserts! (not (is-eq (get chamber-status chamber-record) "expired")) (err u162))

      ;; Valid packet types
      (asserts! (or (is-eq packet-type "item-specifications") 
                   (is-eq packet-type "transfer-evidence")
                   (is-eq packet-type "quality-assessment")
                   (is-eq packet-type "initiator-preferences")) (err u163))

      (print {action: "information_packet_attached", chamber-id: chamber-id, packet-type: packet-type, 
              packet-digest: packet-digest, submitter: tx-sender})
      (ok true)
    )
  )
)


