cinq-cloches-ledger
===================

An tiny Interledger [ledger](https://github.com/interledger/five-bells-ledger) implementation in Haskell.

This implementation passes few basic Interledger tests and is work-in-progress. Long-term, it is hopefully going to be fully Interledger-compliant.

Notable differences with the current Ripple reference implementation:
* Configuration through environment variables differs (variables currently used are `LEDGER_AMOUNT_SCALE`, `LEDGER_BASE_URI`, `LEDGER_PORT`, `LEDGER_DB_CONNECTION_STRING`, `LEDGER_ADMIN_USER`, `LEDGER_ADMIN_PASSWORD`, `LEDGER_MONITOR_INTERVAL`)
* Endpoint `GET /transfers/:id/state` is missing at the moment
* Only database supported is Postgres but support for MySQL and SqlLite should be straightforward to add
* Only crypto condition currently supported is `PreimageSha256`
* For speed, computation on amounts is done internally using 64-bit integers (number are scaled up and down using the `LEDGER_AMOUNT_SCALE` parameter and overflow/underflow will throw errors)
* Response body may not be similar to the reference implementation but that will be fixed shortly
