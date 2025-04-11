# Chamber Vault Protocol

The **Chamber Vault Protocol** is a Clarity-based smart contract framework for secure value storage, conditional transfers, encrypted communication, and multi-party authorization. Built for the [Stacks blockchain](https://www.stacks.co), it enables time-locked vault creation, STX transfers with customizable unlock logic, emergency shutdown mechanisms, and selective disclosure verification.

---

## 💡 Key Features

- 🔐 **Chambers**: Secure vaults for holding and transferring STX with expiration and approval logic.
- ⏳ **Timelocked Vaults**: Create vaults with defined unlock heights and dual-party recovery.
- 📨 **Encrypted Messaging**: Communication between chamber participants using hashed buffers.
- 🧠 **Selective Disclosure**: On-chain Merkle path verification for zero-knowledge data proofs.
- 🚨 **Panic Mode**: Emergency system shutdown with time-based reactivation.
- ⚖️ **Dynamic Fee Adjustment**: Guardian can apply transaction fees based on chamber value.
- 🧬 **Chamber Merging**: Consolidate multiple chambers into a single destination vault.

---

## 🛠 Contract Overview

### Repository: `ChamberRepository`
Stores chamber records containing:
- `initiator` / `beneficiary`
- `item-id`, `quantity`
- `creation-block`, `expiration-block`
- `chamber-status`

### Core Functions
- `create-timelocked-vault(...)`
- `finalize-chamber-transfer(...)`
- `return-chamber-contents(...)`
- `nullify-chamber(...)`
- `add-encrypted-communication(...)`
- `activate-protocol-panic-mode(...)`
- `verify-selective-disclosure(...)`
- `merge-chambers(...)`
- `adjust-chamber-fees(...)`

---

## 📄 Example Usage

```clarity
;; Create a new time-locked vault
(create-timelocked-vault 
  'SP...Beneficiary 
  u105000 
  'SP...Backup 
  u5000000)
```

---

## 🧪 Testing

1. Clone the repo:
   ```bash
   git clone https://github.com/your-org/chamber-vault-protocol.git
   ```
2. Deploy with [Clarinet](https://docs.stacks.co/write-smart-contracts/clarinet):
   ```bash
   clarinet check
   clarinet test
   ```

---

## 📜 License

MIT License. See `LICENSE` file for details.

---

## 👥 Contributing

Contributions welcome! Please open issues or pull requests. Let's build secure storage protocols together!

