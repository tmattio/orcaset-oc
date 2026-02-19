# Basic Income Statement

A simple income statement with a dependency between revenue and operating expenses.

## Statement Structure

```text
Income Statement
├── Revenue
│   ├── Software
│   └── Services
├── Operating Expenses
│   ├── COGS
│   └── Admin
└── Income
```

## Line Items

| Line Item            | Logic                                                                           |
| -------------------- | ------------------------------------------------------------------------------- |
| **Software Revenue** | Starts at $1,000/quarter, grows 10% annually.                                   |
| **Services Revenue** | First period $500. Subsequently calculated as prior period's Total OpEx x -0.5. |
| **COGS**             | Software Revenue x -0.30 (30% of software revenue).                             |
| **Admin**            | Starts at -$100/quarter, grows 5% annually.                                     |
| **Income**           | Total Revenue + Total Operating Expenses.                                       |

## Run

```
dune exec examples2/01-basic-statement/main.exe
```

## Sample Output

```text
                                  2025-01-01    2025-04-01    2025-07-01    2025-10-01
--------------------------------------------------------------------------------------
Income Statement
Revenue
  Software                           1000.00       1025.00       1050.28       1075.83
  Services                            500.00        200.00        204.38        208.80
  Total Revenue                      1500.00       1225.00       1254.65       1284.63
--------------------------------------------------------------------------------------
Operating Expenses
  COGS                               -300.00       -307.50       -315.08       -322.75
  Admin                              -100.00       -101.25       -102.51       -103.79
  Total Operating Expenses           -400.00       -408.75       -417.60       -426.54
--------------------------------------------------------------------------------------
  Income                             1100.00        816.25        837.06        858.09
  Total Income Statement             2200.00       1632.50       1674.11       1716.18
--------------------------------------------------------------------------------------
```
