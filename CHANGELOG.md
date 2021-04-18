## 0.8.0

The auto-match config by default now skips all other values that would be prompted.


Let's assume the following `config.yml`
```
...
ledger:
  prompts: 
    - credit_account
    - purpose
  fills:
    - match:
        payee: "Landlord"
      fill:
        credit_account: "expenses:monthly:rent"
```
In version 0.7.0 this will prompt for the purpose. In version 0.8.0 this will skip the "purpose" prompt.
This is useful for fully automating transactions that already have the "purpose" field. The following configs will results in the same behavior.

_0.7.0_
```
    - match:
        payee: "Landlord"
        purpose: "Monthly rent"
      fill:
        credit_account: "expenses:monthly:rent"
        purpose: "Monthly rent"
```
_0.8.0_
```
    - match:
        payee: "Landlord"
        purpose: "Monthly rent"
      fill:
        credit_account: "expenses:monthly:rent"
```

If prompting is needed, e.g. when you want to automatically fill the credit_account but still be prompted for the purpose, use empty values in the `fill` section:
```
    - match:
        payee: "Landlord"
      fill:
        credit_account: "expenses:monthly:rent"
        purpose:
```