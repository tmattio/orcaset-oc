(** Trailing-window revenue and opening-balance interest

    Demonstrates [Flow.window] and [Balance.sample] on a simple SaaS model:

    - Monthly revenue with 2% growth.
    - Trailing 12-month (TTM) revenue computed declaratively with [Flow.window].
    - Cash balance from cumulative income.
    - Interest accrued on the opening cash balance each period via [Balance.sample]. *)

open Orcaset2

(* Timeline: 3 years of monthly periods *)

let start_date = Date.make 2025 1 1
let tl = Timeline.monthly ~start_date ~n:36

(* Revenue: 2% monthly growth via feedback *)

let revenue =
  Flow.feedback ~default:(100.0 /. 1.02) (fun prev ->
      let cur = Flow.named "Revenue" (Flow.scale 1.02 prev) in
      (cur, cur))

(* TTM revenue: trailing 12-month window over revenue.

   For each period, this accrues revenue over the 12 months ending at the
   current period's end date. The date references shift with the evaluation
   grid, so no manual index arithmetic is needed. Early periods have less
   than 12 months of history; Orcaset clips the window at the timeline start,
   so the warm-up periods show partial trailing totals rather than errors. *)

let ttm_revenue =
  Flow.window ~name:"TTM Revenue"
    ~start:(Date_ref.shift (Period.make_offset ~years:(-1) ()) Date_ref.period_end)
    ~end_:Date_ref.period_end revenue

(* Costs and income *)

let costs = Flow.named "Costs" (Flow.scale (-0.35) revenue)
let income = Flow.named "Income" (Flow.add revenue costs)

(* Cash balance: roll forward income from an initial balance *)

let cash = Balance.roll_forward ~name:"Cash" ~init:50_000.0 income

(* Interest: annual rate applied to the opening cash balance each period.

   [Balance.sample] with [Date_ref.period_start] looks up the cash balance
   in the period that contains the current period's start date. For
   opening-balance semantics (previous period's end value), use
   [Balance.at_period_start] instead. Here we use [at_period_start] to get
   the true opening balance, then multiply by the monthly year fraction. *)

let annual_rate = 0.04

let interest =
  let opening_cash = Balance.at_period_start ~name:"Opening Cash" cash ~default:50_000.0 in
  let year_frac = Flow.year_frac ~name:"Year Frac" Daycount.actual_360 in
  Flow.named "Interest"
    (Flow.scale annual_rate (Flow.mul (Balance.to_flow_approx opening_cash) year_frac))

(* Final cash including interest *)

let cash_with_interest =
  Balance.roll_forward ~name:"Cash (with interest)" ~init:50_000.0 (Flow.add income interest)

(* Statement: tabular output with shared memoization *)

let stmt =
  let open Statement in
  group "SaaS Model"
    [
      flow_line "Revenue" revenue;
      flow_line "TTM Revenue" ttm_revenue;
      flow_line "Costs" costs;
      flow_line "Income" income;
      flow_line "Interest" interest;
      balance_line "Cash" cash_with_interest;
    ]

(* Output *)

let () =
  let ppf = Format.std_formatter in
  let l =
    Statement.layout ~label_width:20 ~col_width:10
      ~col_header:(fun i ->
        let d = Timeline.period_start tl i in
        Printf.sprintf "%d-%02d" (Date.year d) (Date.month d))
      ~pp_num:(fun ppf v -> Format.fprintf ppf "%10.0f" v)
      tl
  in
  Statement.pp l ppf (Statement.eval tl stmt);

  (* Ad-hoc queries: eval individually, query with Materialized *)
  let rev_m = Flow.eval tl revenue in
  let ttm_m = Flow.eval tl ttm_revenue in
  Format.fprintf ppf "@.Year 1 revenue: %12.2f@."
    (Flow.Materialized.accrue rev_m ~start_date:(Date.make 2025 1 1) ~end_date:(Date.make 2026 1 1));
  Format.fprintf ppf "Year 2 revenue: %12.2f@."
    (Flow.Materialized.accrue rev_m ~start_date:(Date.make 2026 1 1) ~end_date:(Date.make 2027 1 1));
  Format.fprintf ppf "TTM at month 12: %11.2f  (should match Year 1)@."
    (Flow.Materialized.get ttm_m 11);
  Format.fprintf ppf "TTM at month 24: %11.2f  (should match Year 2)@."
    (Flow.Materialized.get ttm_m 23)
