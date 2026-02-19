(** Fixed-Rate Amortizing Loan Schedule

    A 30-year fixed-rate mortgage amortization schedule. Interest is computed on the prior period's
    balance using a 30/360 day count convention. Principal payment is the residual (total payment
    minus interest).

    Key API patterns demonstrated:
    - Mutual recursion between balance and interest via [Series.delay] + [Series.prev]
    - Running balance with [Series.cumsum]
    - Day count fractions with [Series.init] and [Daycount]
    - Mid-period balance queries with [Series.Query.balance_at] *)

open Orcaset2

(* Assumptions *)

let loan_amount = 50_000_000.0
let annual_rate = 0.065
let term_months = 360
let start_date = Date.make 2025 1 1

(* Timeline *)

let tl = Timeline.monthly ~start_date ~n:term_months

(* Monthly Payment *)

(* Standard annuity formula: PMT = P * r(1+r)^n / ((1+r)^n - 1) *)
let monthly_payment =
  let r = annual_rate /. 12.0 in
  let n = float_of_int term_months in
  let factor = (1.0 +. r) ** n in
  loan_amount *. (r *. factor) /. (factor -. 1.0)

(* Negative: payments are outflows from the borrower's perspective *)
let total_pmt = Series.const (-.monthly_payment)

(* Year Fractions *)

let year_fracs = Series.year_frac ~name:"Year Fracs" Daycount.thirty_360

(* Loan Amortization *)

(* Interest depends on prior balance, but balance depends on principal which
   depends on interest. [feedback] breaks the cycle: the function receives the
   prior period's balance, and returns the current period's balance definition.

   Recurrence:
     interest.(i)  = -balance.(i-1) * annual_rate * yf.(i)
     principal.(i) = total_pmt.(i) - interest.(i)
     balance.(i)   = balance.(i-1) + principal.(i) *)
let balance, (interest_pmt, principal_pmt) =
  Series.feedback ~name:"Balance" ~default:loan_amount (fun prev_bal ->
      let interest =
        Series.named "Interest" (Series.scale (-.annual_rate) (Series.mul prev_bal year_fracs))
      in
      let principal = Series.named "Principal" (Series.sub total_pmt interest) in
      let balance = Series.cumsum ~init:loan_amount principal in
      (balance, (balance, (interest, principal))))

(* Output *)

let () =
  let balance_values = Series.eval tl balance in
  let interest_values = Series.eval tl interest_pmt in
  let principal_values = Series.eval tl principal_pmt in
  let total_values = Series.eval tl total_pmt in

  Printf.printf "=== Fixed-Rate Amortizing Loan Schedule ===\n";
  Printf.printf "Loan Amount:     $%.2f\n" loan_amount;
  Printf.printf "Annual Rate:     %.3f%%\n" (annual_rate *. 100.0);
  Printf.printf "Term:            %d months (%.1f years)\n" term_months
    (float_of_int term_months /. 12.0);
  Printf.printf "Day Count:       30/360\n";
  Printf.printf "Monthly Payment: $%.2f\n" monthly_payment;
  Printf.printf "Start Date:      %s\n" (Date.to_string start_date);
  Printf.printf "\n";

  (* Sanity check: total principal repaid should equal the original loan *)
  let total_principal = Array.fold_left (fun acc v -> acc +. -.v) 0.0 principal_values in
  Printf.printf "=== Confirm Principal Payments ===\n";
  Printf.printf "Loan Amount:           $%.2f\n" loan_amount;
  Printf.printf "Total Repaid Principal: $%.2f\n" total_principal;
  Printf.printf "Difference:            $%.2f\n" (loan_amount -. total_principal);
  Printf.printf "\n";

  (* Series.Query.balance_at interpolates the balance at any date, even
     mid-period, by pro-rating the current period's principal flow *)
  Printf.printf "=== Loan Balance Queries ===\n";
  let query_dates =
    [ Date.make 2025 1 1; Date.make 2025 1 15; Date.make 2028 2 4; Date.make 2040 1 7 ]
  in
  List.iter
    (fun d ->
      let bal = Series.Query.balance_at tl ~balance:balance_values ~flow:principal_values d in
      Printf.printf "Balance on %s: $%.2f\n" (Date.to_string d) bal)
    query_dates;
  Printf.printf "\n";

  (* Display the first year of the amortization schedule *)
  Printf.printf "%5s  %10s  %14s  %12s  %12s  %12s  %14s\n" "Month" "Date" "Beg Balance" "Payment"
    "Interest" "Principal" "End Balance";
  Printf.printf "%s\n" (String.make 89 '-');
  let display_months = min 12 term_months in
  for i = 0 to display_months - 1 do
    let end_date = Timeline.period_end tl i in
    let beg_bal = if i = 0 then loan_amount else balance_values.(i - 1) in
    Printf.printf "%5d  %10s  %14.2f  %12.2f  %12.2f  %12.2f  %14.2f\n" (i + 1)
      (Date.to_string end_date) beg_bal
      (-.total_values.(i))
      (-.interest_values.(i))
      (-.principal_values.(i))
      balance_values.(i)
  done;

  (* Dependency graph *)
  let oc = open_out "model.dot" in
  let ppf = Format.formatter_of_out_channel oc in
  Series.Deps.pp_dot ppf [ balance; interest_pmt; principal_pmt ];
  Format.pp_print_flush ppf ();
  close_out oc
