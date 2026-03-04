(** Loan with Schedule -- Multi-Periodicity

    A monthly operating model with a quarterly term loan originated on a non-standard date. The loan
    schedule runs at its own periodicity (quarterly from Feb 15) with business day adjustments,
    while the operating model runs monthly from Jan 1.

    Key patterns:
    - {!Schedule.make} with roll and BDC conventions
    - Loan amortization on the schedule's own timeline
    - Interest accrual from unadjusted periods
    - Quarterly payments bridged to the monthly model via {!Schedule.to_events} +
      {!Formula.of_events} *)

open Orcaset2

(* Assumptions *)

let model_start = Date.make 2025 1 1
let model_months = 60
let issuance_date = Date.make 2025 2 15
let maturity_date = Date.make 2030 2 15
let loan_amount = 10_000_000.0
let annual_rate = 0.065
let loan_quarters = 20

(* Model Timeline *)

let model_tl = Timeline.monthly ~start_date:model_start ~n:model_months

(* Loan Schedule *)

let loan_sched =
  Schedule.make ~start_date:issuance_date ~end_date:maturity_date
    ~offset:(Period.make_offset ~quarters:1 ())
    ~bdc:Modified_following ~calendar:Calendar.weekdays ()

let loan_tl = Schedule.to_timeline loan_sched

(* Quarterly Payment *)

let quarterly_payment =
  let r = annual_rate /. 4.0 in
  let n = float_of_int loan_quarters in
  let factor = (1.0 +. r) ** n in
  loan_amount *. (r *. factor) /. (factor -. 1.0)

(* Loan Amortization *)

(* Interest accrual uses unadjusted year fractions: the interest period runs
   from the contractual coupon date to the next, regardless of weekend shifts. *)
let accrual_yf =
  let unadj_periods = Schedule.unadjusted_periods loan_sched in
  Formula.init ~name:"Year Fracs" (fun i _p ->
      let up = unadj_periods.(i) in
      Daycount.actual_360 (Period.start_date up) (Period.end_date up))

let total_pmt = Formula.const ~name:"Quarterly Payment" (-.quarterly_payment)

let loan_balance, (interest_pmt, principal_pmt) =
  Formula.feedback ~name:"Loan Balance" ~default:loan_amount (fun prev_bal ->
      let interest =
        Formula.named "Interest" (Formula.scale (-.annual_rate) (Formula.mul prev_bal accrual_yf))
      in
      let principal = Formula.named "Principal" (Formula.sub total_pmt interest) in
      let balance = Formula.cumsum ~init:loan_amount principal in
      (balance, (balance, (interest, principal))))

(* Bridge quarterly loan events into the monthly model. *)

let bridge_to_monthly name series =
  let vals = Formula.eval loan_tl series in
  Formula.of_events ~name (Schedule.to_events (fun i _p -> vals.(i)) loan_sched)

let monthly_interest = bridge_to_monthly "Loan Interest" interest_pmt
let monthly_principal = bridge_to_monthly "Loan Principal" principal_pmt

let monthly_debt_service =
  Formula.named "Debt Service" (Formula.add monthly_interest monthly_principal)

(* Operating Model *)

let revenue =
  Formula.growth_simple ~name:"Revenue" ~start_date:model_start ~rate:0.08
    ~daycount:Daycount.calendar_monthly 500_000.0

let opex =
  Formula.growth_simple ~name:"Operating Expenses" ~start_date:model_start ~rate:0.03
    ~daycount:Daycount.calendar_monthly (-200_000.0)

let noi = Formula.named "NOI" (Formula.add revenue opex)
let cfaf = Formula.named "CFAF" (Formula.add noi monthly_debt_service)
let cash = Formula.cumsum ~name:"Cash Balance" ~init:2_000_000.0 cfaf

(* Output *)

let () =
  Printf.printf "=== Loan with Schedule: Multi-Periodicity Demo ===\n\n";

  (* Schedule info *)
  Printf.printf "LOAN SCHEDULE\n";
  Printf.printf "=============\n";
  Printf.printf "Issuance:        %s\n" (Date.to_string issuance_date);
  Printf.printf "Maturity:        %s\n" (Date.to_string maturity_date);
  Printf.printf "Amount:          $%.0f\n" loan_amount;
  Printf.printf "Rate:            %.2f%%\n" (annual_rate *. 100.0);
  Printf.printf "Periods:         %d quarters\n" (Schedule.length loan_sched);
  Printf.printf "Quarterly PMT:   $%.2f\n" quarterly_payment;
  Printf.printf "\n";

  (* Show adjusted vs unadjusted boundary dates *)
  Printf.printf "BOUNDARY DATES (first 8)\n";
  Printf.printf "========================\n";
  let unadj = Schedule.unadjusted_dates loan_sched in
  let adj = Schedule.dates loan_sched in
  let show_n = min 9 (Array.length adj) in
  Printf.printf "%3s  %12s  %12s  %s\n" "#" "Unadjusted" "Adjusted" "Shift";
  Printf.printf "%s\n" (String.make 45 '-');
  for i = 0 to show_n - 1 do
    let diff = Date.diff adj.(i) unadj.(i) in
    let shift = if diff = 0 then "" else Printf.sprintf "%+d days" diff in
    Printf.printf "%3d  %12s  %12s  %s\n" i
      (Date.to_string unadj.(i))
      (Date.to_string adj.(i))
      shift
  done;
  Printf.printf "\n";

  (* Loan amortization on loan timeline *)
  Printf.printf "LOAN AMORTIZATION (first 8 quarters)\n";
  Printf.printf "=====================================\n";
  let bal_v = Formula.eval loan_tl loan_balance in
  let int_v = Formula.eval loan_tl interest_pmt in
  let pri_v = Formula.eval loan_tl principal_pmt in
  Printf.printf "%3s  %12s  %14s  %12s  %12s  %14s\n" "Q" "Pay Date" "Beg Balance" "Interest"
    "Principal" "End Balance";
  Printf.printf "%s\n" (String.make 75 '-');
  let show_q = min 8 (Schedule.length loan_sched) in
  for i = 0 to show_q - 1 do
    let beg_bal = if i = 0 then loan_amount else bal_v.(i - 1) in
    Printf.printf "%3d  %12s  %14.2f  %12.2f  %12.2f  %14.2f\n" (i + 1)
      (Date.to_string adj.(i + 1))
      beg_bal
      (-.int_v.(i))
      (-.pri_v.(i))
      bal_v.(i)
  done;
  Printf.printf "\n";

  (* Monthly model statement *)
  Printf.printf "MONTHLY MODEL (first 12 months)\n";
  Printf.printf "===============================\n\n";
  let display_tl = Timeline.monthly ~start_date:model_start ~n:12 in
  let model_statement =
    let open Statement in
    group "Monthly Cash Flow"
      [
        line "Revenue" revenue;
        line "Operating Expenses" opex;
        line "NOI" noi;
        group ~total:monthly_debt_service "Debt Service"
          [ line "Interest" monthly_interest; line "Principal" monthly_principal ];
        line "CFAF" cfaf;
        line "Cash Balance" cash;
      ]
  in
  let results = Statement.eval model_tl model_statement in
  let l =
    Statement.layout ~label_width:25 ~col_width:14
      ~col_header:(fun i -> Date.to_string (Timeline.period_start model_tl i))
      ~pp_num:(fun ppf v -> Format.fprintf ppf "%14.0f" v)
      ~sep:'=' display_tl
  in
  Statement.pp l Format.std_formatter results;

  (* Dependency graph *)
  let oc = open_out "model.dot" in
  let ppf = Format.formatter_of_out_channel oc in
  Formula.Deps.pp_dot ppf [ cfaf; cash ];
  Format.pp_print_flush ppf ();
  close_out oc
