# Loan Amortization Schedule

A 30-year fixed-rate amortizing mortgage schedule for a $50M commercial real estate loan.

## Loan Terms

| Parameter            | Value                 |
| -------------------- | --------------------- |
| Loan Amount          | $50,000,000           |
| Annual Interest Rate | 6.500%                |
| Term                 | 360 months (30 years) |
| Day Count Convention | 30/360                |
| Monthly Payment      | $316,034.01           |
| Start Date           | 2025-01-01            |

## Schedule Mechanics

Each monthly period computes:

1. **Interest**: Prior period's balance x annual rate x day count fraction
2. **Principal**: Total payment - Interest
3. **End Balance**: Beginning balance + Principal (negative, reducing the balance)

The schedule also demonstrates mid-period balance queries: given any arbitrary date (even between payment dates), the balance is interpolated by pro-rating the current period's principal.

## Run

```
dune exec examples2/03-loan-schedule/main.exe
```

## Sample Output

```text
=== Fixed-Rate Amortizing Loan Schedule ===
Loan Amount:     $50000000.00
Annual Rate:     6.500%
Term:            360 months (30.0 years)
Day Count:       30/360
Monthly Payment: $316034.01
Start Date:      2025-01-01

=== Confirm Principal Payments ===
Loan Amount:           $50000000.00
Total Repaid Principal: $50000000.00
Difference:            $-0.00

=== Loan Balance Queries ===
Balance on 2025-01-01: $50000000.00
Balance on 2025-01-15: $49979586.79
Balance on 2028-02-04: $48148006.48
Balance on 2040-01-07: $36256437.78

Month        Date     Beg Balance       Payment      Interest     Principal     End Balance
-----------------------------------------------------------------------------------------
    1  2025-02-01     50000000.00     316034.01     270833.33      45200.68     49954799.32
    2  2025-03-01     49954799.32     316034.01     270588.50      45445.52     49909353.81
    3  2025-04-01     49909353.81     316034.01     270342.33      45691.68     49863662.13
    4  2025-05-01     49863662.13     316034.01     270094.84      45939.18     49817722.95
    5  2025-06-01     49817722.95     316034.01     269846.00      46188.01     49771534.94
    ...
```
