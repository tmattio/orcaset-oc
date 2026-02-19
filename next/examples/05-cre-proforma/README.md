# CRE Pro Forma — Single Property

A complete commercial real estate pro forma for a fictitious 25,000 SF Class B office building, projecting monthly cash flows from Gross Potential Rent through Cash Flow After Financing.

## Property Assumptions

| Parameter      | Value                |
| -------------- | -------------------- |
| Building Size  | 25,000 SF            |
| Parking Spaces | 50                   |
| Purchase Price | $4,500,000 ($180/SF) |

## Statement Structure

```text
Real Estate Pro Forma
├── Gross Potential Rent
│   ├── Base Rent
│   ├── Parking Income
│   ├── CAM Recoveries
│   └── Other Income
├── Less: Vacancy & Credit Loss
├── Effective Gross Income
├── Operating Expenses
│   ├── Property Taxes
│   ├── Insurance
│   ├── Utilities
│   ├── Repairs & Maintenance
│   ├── Property Management
│   ├── Janitorial
│   ├── Landscaping
│   └── Security
├── Net Operating Income (NOI)
├── Capital Expenditures
│   ├── Capital Reserves
│   ├── Tenant Improvements
│   └── Leasing Commissions
├── Cash Flow Before Financing
├── Debt Service
│   ├── Interest Expense
│   └── Principal Payment
└── Cash Flow After Financing
```

## Revenue

| Line Item                 | Logic                                           |
| ------------------------- | ----------------------------------------------- |
| **Base Rent**             | $22/SF/year, 3% annual growth.                  |
| **Parking Income**        | 50 spaces at $75/month, grows with rent.        |
| **CAM Recoveries**        | 85% of prior period's total operating expenses. |
| **Other Income**          | $1,500/month, grows with rent.                  |
| **Vacancy & Credit Loss** | 7% of Gross Potential Rent.                     |

## Operating Expenses

All expenses grow at 2.5% annually.

| Line Item                 | Logic                         |
| ------------------------- | ----------------------------- |
| **Property Taxes**        | $72,000/year.                 |
| **Insurance**             | $15,000/year.                 |
| **Utilities**             | $6,250/month.                 |
| **Repairs & Maintenance** | $4,167/month.                 |
| **Property Management**   | 4% of Effective Gross Income. |
| **Janitorial**            | $5,208/month.                 |
| **Landscaping**           | $1,250/month.                 |
| **Security**              | $2,083/month.                 |

## Capital Expenditures

| Line Item               | Logic          |
| ----------------------- | -------------- |
| **Capital Reserves**    | 3% of EGI.     |
| **Tenant Improvements** | $1.50/SF/year. |
| **Leasing Commissions** | 2% of EGI.     |

## Debt Service

| Parameter       | Value                |
| --------------- | -------------------- |
| Loan Amount     | $3,150,000 (70% LTV) |
| Interest Rate   | 5.50% fixed          |
| Term            | 25 years             |
| Monthly Payment | $19,343.76           |

## Run

```
dune exec examples2/05-cre-proforma/main.exe
```

## Sample Output

```text
PRO FORMA CASH FLOW PROJECTION
==============================

                                       2023-01-01    2023-02-01    2023-03-01    2023-04-01    2023-05-01    2023-06-01
===========================================================================================================================================================================================================
Real Estate Pro Forma
Gross Potential Rent
  Base Rent                                 45833         45952         46059         46177         46292         46410
  Parking Income                             3750          3760          3768          3778          3788          3797
  CAM Recoveries                             1700         23946         24701         24772         24827         24879
  Other Income                               1500          1504          1507          1511          1515          1519
  Total Gross Potential Rent                52783         75161         76036         76239         76421         76605
===========================================================================================================================================================================================================
  Less: Vacancy & Credit Loss               -3695         -5261         -5323         -5337         -5349         -5362
  Effective Gross Income                    49088         69900         70713         70902         71071         71243
  ...
  Net Operating Income (NOI)                20917         40839         41569         41694         41802         41910
  ...
  Cash Flow Before Financing                15338         34219         34909         35024         35124         35223
  ...
  Cash Flow After Financing                 -4006         14876         15565         15680         15780         15879
```
