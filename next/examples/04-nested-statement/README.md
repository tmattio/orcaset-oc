# Nested Income Statement

A hierarchical income statement with three levels of nesting, demonstrating how recurring and non-recurring revenue streams flow through cost of revenue to produce operating income.

## Statement Structure

```text
Operating Income
├── Gross Profit
│   ├── Revenue
│   │   ├── Recurring
│   │   └── Non-Recurring
│   └── Cost of Revenue
│       ├── Recurring
│       └── Non-Recurring
└── Admin Expenses
```

## Line Items

| Line Item                           | Logic                                                                    |
| ----------------------------------- | ------------------------------------------------------------------------ |
| **Recurring Revenue**               | Starts at $10,000/month, grows 5% annually.                              |
| **Non-Recurring Revenue**           | Random walk with $50/period drift and $500 volatility. Starts at $2,000. |
| **Cost of Revenue / Recurring**     | Recurring Revenue x -0.30 (30% margin).                                  |
| **Cost of Revenue / Non-Recurring** | Non-Recurring Revenue x -0.40 (40% margin).                              |
| **Gross Profit**                    | Total Revenue + Total Cost of Revenue.                                   |
| **Admin Expenses**                  | Starts at -$1,500/month, grows 3% annually.                              |
| **Operating Income**                | Gross Profit + Admin Expenses.                                           |

Group totals (Revenue, Cost of Revenue, Gross Profit, Operating Income) are computed automatically by summing their children.

## Run

```
dune exec examples2/04-nested-statement/main.exe
```

## Sample Output

```text
=== INCOME STATEMENT ===

                                2025-01-01  2025-02-01  2025-03-01  2025-04-01  2025-05-01  2025-06-01
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Operating Income
Gross Profit
Revenue
  Recurring                         10,000      10,043      10,081      10,125      10,166      10,209
  Non-Recurring                      2,237       2,005       2,032       1,568       1,781       1,766
  Total Revenue                     12,237      12,048      12,114      11,693      11,948      11,976
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Cost of Revenue
  Recurring                         -3,000      -3,012      -3,024      -3,037      -3,049      -3,062
  Non-Recurring                       -895        -802        -813        -627        -712        -706
  Total Cost of Revenue             -3,895      -3,814      -3,837      -3,665      -3,762      -3,769
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Total Gross Profit                 8,342       8,233       8,276       8,028       8,185       8,206
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Admin Expenses                    -1,500      -1,503      -1,507      -1,511      -1,515      -1,518
  Total Operating Income             6,842       6,729       6,769       6,517       6,670       6,687
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
```
