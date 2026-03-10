(** Three-Statement Financial Model

    Income Statement, Cash Flow Statement, and Balance Sheet linked together.

    Key patterns demonstrated:
    - [Flow.t] for all income and cash flow line items (revenue, expenses, net income).
    - [Balance.t] for all balance sheet items (cash, PPE, equity).
    - [Balance.feedback] for the PPE/depreciation circular dependency.
    - [Balance.roll_forward] to accumulate flows into balances (cash, retained earnings).
    - [Statement.flow_line] and [Statement.balance_line] for typed statement construction. *)

open Orcaset2

(* Assumptions *)

let start_date = Date.make 2025 1 1
let initial_revenue = 1000.0
let revenue_growth_rate = 0.05
let cogs_pct = 0.30
let opex_monthly = 200.0
let tax_rate = 0.20
let capex_pct = 0.05
let depreciation_rate = 0.10
let initial_cash = 1000.0
let initial_ppe = 10000.0
let common_stock_amount = 5000.0

(* Timeline *)

let tl = Timeline.monthly ~start_date ~n:12

(* Income Statement *)

let revenue =
  Flow.growth_simple ~name:"Revenue" ~start_date ~rate:revenue_growth_rate initial_revenue

let cogs = Flow.named "COGS" (Flow.scale (-.cogs_pct) revenue)
let gross_profit = Flow.named "Gross Profit" (Flow.add revenue cogs)
let opex = Flow.const ~name:"OpEx" (-.opex_monthly)
let capex = Flow.named "CapEx" (Flow.scale (-.capex_pct) revenue)

(* Circular dependency: depreciation depends on PPE net, PPE net depends on
   depreciation. [Balance.feedback] breaks the cycle: the function receives the
   prior period's PPE net (a balance), and returns the current period's
   definition. *)
let ppe_net, depreciation =
  Balance.feedback ~default:initial_ppe (fun prev_ppe ->
      let depreciation =
        Flow.map ~name:"Depreciation"
          (fun ppe -> -.(ppe *. depreciation_rate /. 12.0))
          (Pointwise.to_flow_approx (Pointwise.of_balance prev_ppe))
      in
      let ppe_change = Flow.named "PPE Change" (Flow.add (Flow.neg capex) depreciation) in
      let ppe_net = Balance.roll_forward ~name:"PPE Net" ~init:initial_ppe ppe_change in
      (ppe_net, (ppe_net, depreciation)))

let ebt = Flow.named "EBT" (Flow.sum [ gross_profit; opex; depreciation ])
let tax = Flow.named "Tax" (Flow.scale (-.tax_rate) ebt)
let net_income = Flow.named "Net Income" (Flow.add ebt tax)

(* Cash Flow Statement *)

(* Depreciation is a non-cash charge: add it back to get operating cash flow *)
let cf_net_income = net_income
let cf_depreciation_addback = Flow.named "Depreciation Add-back" (Flow.neg depreciation)
let cf_ops = Flow.named "CF Operations" (Flow.add cf_net_income cf_depreciation_addback)
let cf_invest = capex
let cf_finance = Flow.const ~name:"CF Financing" 0.0
let net_cash_change = Flow.named "Net Cash Change" (Flow.sum [ cf_ops; cf_invest; cf_finance ])

(* Balance Sheet *)

let cash = Balance.roll_forward ~name:"Cash" ~init:initial_cash net_cash_change
let total_assets = Balance.named "Total Assets" (Balance.add cash ppe_net)
let common_stock = Balance.const ~name:"Common Stock" common_stock_amount

(* Derived so the balance sheet balances at t=0: assets - equity = retained earnings *)
let initial_re = initial_cash +. initial_ppe -. common_stock_amount
let retained_earnings = Balance.roll_forward ~name:"Retained Earnings" ~init:initial_re net_income

let total_liabilities_equity =
  Balance.named "Total L&E" (Balance.add common_stock retained_earnings)

let balance_check =
  Balance.named "Balance Check" (Balance.sub total_assets total_liabilities_equity)

(* Statements *)

let financial_statement =
  let open Statement in
  group "Financial Model"
    [
      group "Income Statement"
        [
          group "Gross Profit" [ flow_line "Revenue" revenue; flow_line "COGS" cogs ];
          flow_line "Opex" opex;
          flow_line "Depreciation" depreciation;
          flow_line "Tax" tax;
          flow_line "Net Income" net_income;
        ];
      group "Cash Flow Statement"
        [
          group "Operations"
            [
              flow_line "Net Income" cf_net_income;
              flow_line "Depreciation Add Back" cf_depreciation_addback;
            ];
          group "Investing" [ flow_line "Capex" cf_invest ];
          flow_line "CF Financing" cf_finance;
          flow_line "Net Cash Change" net_cash_change;
        ];
    ]

let balance_sheet_statement =
  let open Statement in
  group "Balance Sheet"
    [
      group "Assets" [ balance_line "Cash" cash; balance_line "PPE Net" ppe_net ];
      group "Liabilities & Equity"
        [
          balance_line "Common Stock" common_stock;
          balance_line "Retained Earnings" retained_earnings;
        ];
      group "Check" [ balance_line "Balance Check" balance_check ];
    ]

(* Output *)

let () =
  let l =
    Statement.layout ~label_width:30 ~col_width:10
      ~col_header:(fun i ->
        let d = Timeline.period_end tl i in
        Printf.sprintf "%d-%02d" (Date.year d) (Date.month d))
      ~pp_num:(fun ppf v -> Format.fprintf ppf "%10.2f" v)
      tl
  in
  let ppf = Format.std_formatter in

  Format.fprintf ppf "@.";
  Format.fprintf ppf "============================================================@.";
  Format.fprintf ppf "               SIMPLE 3-STATEMENT FINANCIAL MODEL@.";
  Format.fprintf ppf "============================================================@.";
  Format.fprintf ppf "@.";

  let results = Statement.eval tl financial_statement in
  Statement.pp l ppf results;

  let bs_results = Statement.eval tl balance_sheet_statement in
  Statement.pp l ppf bs_results;

  (* Dependency graph *)
  let oc = open_out "model.dot" in
  let dot_ppf = Format.formatter_of_out_channel oc in
  Balance.Deps.pp_dot dot_ppf [ balance_check ];
  Format.pp_print_flush dot_ppf ();
  close_out oc
