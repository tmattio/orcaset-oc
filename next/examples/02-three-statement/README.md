# Three-Statement Financial Model

A complete three-statement model linking an Income Statement, Cash Flow Statement, and Balance Sheet.

## Statement Structure

```text
Financial Model
├── Income Statement
│   ├── Gross Profit
│   │   ├── Revenue
│   │   └── COGS
│   ├── Opex
│   ├── Depreciation
│   ├── Tax
│   └── Net Income
├── Cash Flow Statement
│   ├── Operations
│   │   ├── Net Income
│   │   └── Depreciation Add Back
│   ├── Investing
│   │   └── Capex
│   ├── CF Financing
│   └── Net Cash Change
└── Balance Sheet
    ├── Assets
    │   ├── Cash
    │   └── PPE Net
    ├── Liabilities & Equity
    │   ├── Common Stock
    │   └── Retained Earnings
    └── Check
        └── Balance Check
```

## Line Items

| Line Item                 | Logic                                                                     |
| ------------------------- | ------------------------------------------------------------------------- |
| **Revenue**               | Starts at $1,000/month, grows 5% annually (Actual/360 day count).         |
| **COGS**                  | Revenue x -0.30 (30% of revenue).                                         |
| **Gross Profit**          | Revenue + COGS.                                                           |
| **Opex**                  | Constant -$200/month.                                                     |
| **Depreciation**          | Prior period's PPE Net x (10% / 12).                                      |
| **Tax**                   | (Gross Profit + Opex + Depreciation) x -0.20 (20% tax rate).              |
| **Net Income**            | Earnings before tax + Tax.                                                |
| **Depreciation Add Back** | Reverses the non-cash depreciation charge.                                |
| **CF Operations**         | Net Income + Depreciation Add Back.                                       |
| **Capex**                 | Revenue x -0.05 (5% of revenue).                                          |
| **CF Financing**          | Constant $0.                                                              |
| **Net Cash Change**       | CF Operations + CF Investing + CF Financing.                              |
| **Cash**                  | Starts at $1,000. Accumulates Net Cash Change.                            |
| **PPE Net**               | Starts at $10,000. Increases by Capex, decreases by Depreciation.         |
| **Common Stock**          | Constant $5,000.                                                          |
| **Retained Earnings**     | Starts at $6,000 (Initial Assets - Common Stock). Accumulates Net Income. |
| **Balance Check**         | Total Assets - Total Liabilities & Equity. Should be $0.                  |

## Run

```
dune exec examples2/02-three-statement/main.exe
```

## Sample Output

```text
============================================================
               SIMPLE 3-STATEMENT FINANCIAL MODEL
============================================================

                                 2025-02   2025-03   2025-04   2025-05   2025-06   2025-07
------------------------------------------------------------------------------------------------------------------------------------------------------
Financial Model
Income Statement
Gross Profit
  Revenue                        1000.00   1004.31   1008.19   1012.50   1016.67   1020.97
  COGS                           -300.00   -301.29   -302.46   -303.75   -305.00   -306.29
  Total Gross Profit              700.00    703.01    705.74    708.75    711.67    714.68
------------------------------------------------------------------------------------------------------------------------------------------------------
  Opex                           -200.00   -200.00   -200.00   -200.00   -200.00   -200.00
  Depreciation                    -83.33    -83.06    -82.78    -82.51    -82.25    -81.98
  Tax                             -83.33    -83.99    -84.59    -85.25    -85.88    -86.54
  Net Income                      333.33    335.97    338.36    340.99    343.54    346.16
  ...

Balance Sheet
Assets
  Cash                           1366.67   1735.47   2106.21   2479.09   2854.04   3231.13
  PPE Net                        9966.67   9933.83   9901.45   9869.57   9838.15   9807.22
  Total Assets                  11333.33  11669.30  12007.66  12348.65  12692.19  13038.35
  ...

Check
  Balance Check                     0.00      0.00      0.00      0.00      0.00      0.00
```
