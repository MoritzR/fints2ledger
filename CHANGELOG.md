# Changelog

## 1.0.0
* The new `--demo` flag allows one to try out fints2ledger without actually calling a banking API. Dummy transactions are used instead.
* For automatic matching, the amount field now allows values like "<=90" instead of a regex. This is useful if one wants double check on expensive transactions that would normally be automatically matched.
* The `fills` key in the `config.yml` is no longer optional. At least an empty array must be specified.
* `template.txt` in the config directory is no longer in use and can be removed. The previous version did not pick this up correctly and instead used the default template.
* `*.auto` files are no longer in use. Credit and debit account will instead be autocompleted based on the ledger file that is passed in. This means that all existing accounts will be picked up in the autocompletion without having to add them to the `.auto` file first. This also means that other won't have any autocompletion anymore (if this was enabled by the user previously).
* The prompt output is now slightly nicer (less `"` and `{}`)
* The program should run properly on windows now
* The installation process requires more steps now. Most of the program is now run as a binary, while the banking API is still accessed through the fints python library.

## 0.8.2

* pyfints was updated to the current version 4
* The `password` field in the config can now be left empty. The password will then be prompted each time the application is run. Use this if you don't feel comfortable storing your banking password in a file.

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