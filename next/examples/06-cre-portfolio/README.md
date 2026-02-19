# CRE Portfolio — Multi-Property Aggregation

Aggregates 10,000 identical commercial real estate properties into a portfolio-level pro forma with 120 months (10 years) of monthly projections.

Each property is a full model — revenue, operating expenses, capital expenditures, and debt service — evaluated independently and then summed across the portfolio. Aggregation is parallelized across CPU cores.

## Portfolio Summary

| Parameter            | Value                     |
| -------------------- | ------------------------- |
| Properties           | 10,000                    |
| Total Square Footage | 250,000,000 SF            |
| Total Purchase Price | $45,000,000,000           |
| Total Loan Amount    | $31,500,000,000 (70% LTV) |
| Projection Period    | 120 months                |

## Statement Structure

```text
Portfolio Totals
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

Each property uses the same assumptions as the [single-property pro forma](../05-cre-proforma/) (25,000 SF Class B office, $22/SF base rent, 70% LTV). The per-property line item definitions are identical; only the aggregation across 10,000 properties is new.

## Run

```
dune exec examples2/06-cre-portfolio/main.exe
```

## Sample Output

```text
PORTFOLIO SUMMARY
=================

Properties in Portfolio: 10000
Total Square Footage:    250000000 SF
Total Purchase Price:    $45000000000
Total Loan Amount:       $31500000000

PORTFOLIO PRO FORMA PROJECTIONS
==================================================

                                            2023-01-01    2023-02-01    2023-03-01    2023-04-01
================================================================================================================================================
Portfolio Totals
Gross Potential Rent
  Base Rent                                  458333333     459517361     460586806     461770833
  Parking Income                              37500000      37596875      37684375      37781250
  CAM Recoveries                              17000000     239458090     247013512     247723264
  Other Income                                15000000      15038750      15073750      15112500
  Total Gross Potential Rent                 527833333     751611076     760358443     762387847
================================================================================================================================================
  Less: Vacancy & Credit Loss                -36948333     -52612775     -53225091     -53367149
  Effective Gross Income                     490885000     698998301     707133352     709020698
  ...
  Net Operating Income (NOI)                 209169600     408394169     415694218     416941870
  ...
  Cash Flow After Financing                  -40062210     148756694     155649990     156803275
================================================================================================================================================
```
