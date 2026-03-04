(** Three-Statement Financial Model

    Income Statement, Cash Flow Statement, and Balance Sheet linked together. Key patterns: breaking
    circular dependencies with [Formula.delay] + [Formula.prev], running balances with
    [Formula.cumsum], and hierarchical output with [Statement]. *)

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
  Formula.growth_simple ~name:"Revenue" ~start_date ~rate:revenue_growth_rate initial_revenue

let cogs = Formula.named "COGS" (Formula.scale (-.cogs_pct) revenue)
let gross_profit = Formula.named "Gross Profit" (Formula.add revenue cogs)
let opex = Formula.const ~name:"OpEx" (-.opex_monthly)
let capex = Formula.named "CapEx" (Formula.scale (-.capex_pct) revenue)

(* Circular dependency: depreciation depends on PPE net, PPE net depends on
   depreciation. [feedback] breaks the cycle: the function receives the prior
   period's PPE net, and returns the current period's definition. *)
let ppe_net, depreciation =
  Formula.feedback ~default:initial_ppe (fun prev_ppe ->
      let depreciation =
        Formula.map ~name:"Depreciation" (fun ppe -> -.(ppe *. depreciation_rate /. 12.0)) prev_ppe
      in
      let ppe_change = Formula.named "PPE Change" (Formula.add (Formula.neg capex) depreciation) in
      let ppe_net = Formula.cumsum ~name:"PPE Net" ~init:initial_ppe ppe_change in
      (ppe_net, (ppe_net, depreciation)))

let ebt = Formula.named "EBT" (Formula.sum [ gross_profit; opex; depreciation ])
let tax = Formula.named "Tax" (Formula.scale (-.tax_rate) ebt)
let net_income = Formula.named "Net Income" (Formula.add ebt tax)

(* Cash Flow Statement *)

(* Depreciation is a non-cash charge: add it back to get operating cash flow *)
let cf_net_income = net_income
let cf_depreciation_addback = Formula.named "Depreciation Add-back" (Formula.neg depreciation)
let cf_ops = Formula.named "CF Operations" (Formula.add cf_net_income cf_depreciation_addback)
let cf_invest = capex
let cf_finance = Formula.const ~name:"CF Financing" 0.0
let net_cash_change = Formula.named "Net Cash Change" (Formula.sum [ cf_ops; cf_invest; cf_finance ])

(* Balance Sheet *)

let cash = Formula.cumsum ~name:"Cash" ~init:initial_cash net_cash_change
let total_assets = Formula.named "Total Assets" (Formula.add cash ppe_net)
let common_stock = Formula.const ~name:"Common Stock" common_stock_amount

(* Derived so the balance sheet balances at t=0: assets - equity = retained earnings *)
let initial_re = initial_cash +. initial_ppe -. common_stock_amount
let retained_earnings = Formula.cumsum ~name:"Retained Earnings" ~init:initial_re net_income
let total_liabilities_equity = Formula.named "Total L&E" (Formula.add common_stock retained_earnings)
let balance_check = Formula.named "Balance Check" (Formula.sub total_assets total_liabilities_equity)

(* Statements *)

let financial_statement =
  let open Statement in
  group "Financial Model"
    [
      group "Income Statement"
        [
          group "Gross Profit" [ line "Revenue" revenue; line "COGS" cogs ];
          line "Opex" opex;
          line "Depreciation" depreciation;
          line "Tax" tax;
          line "Net Income" net_income;
        ];
      group "Cash Flow Statement"
        [
          group "Operations"
            [
              line "Net Income" cf_net_income; line "Depreciation Add Back" cf_depreciation_addback;
            ];
          group "Investing" [ line "Capex" cf_invest ];
          line "CF Financing" cf_finance;
          line "Net Cash Change" net_cash_change;
        ];
    ]

let balance_sheet_statement =
  let open Statement in
  group "Balance Sheet"
    [
      group "Assets" [ line "Cash" cash; line "PPE Net" ppe_net ];
      group "Liabilities & Equity"
        [ line "Common Stock" common_stock; line "Retained Earnings" retained_earnings ];
      group "Check" [ line "Balance Check" balance_check ];
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
  Formula.Deps.pp_dot dot_ppf [ balance_check ];
  Format.pp_print_flush dot_ppf ();
  close_out oc
