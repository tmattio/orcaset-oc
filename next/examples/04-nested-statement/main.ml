(** Nested Statement

    A hierarchical income statement with three levels of nesting:

    {v
    Operating Income
      Gross Profit
        Revenue  (Recurring + Non-Recurring)
        Cost of Revenue  (Recurring + Non-Recurring)
      Admin Expenses
    v}

    Non-recurring revenue follows a random walk with drift and volatility, demonstrating
    [Formula.scan] as a replacement for the old [Seq.unfold] pattern.

    Group totals are synthesized automatically by [Statement.auto_total] (called implicitly by
    [Statement.eval]) -- no explicit totals needed.

    Demonstrates: [Formula.init], [Formula.scan], [Formula.scale], [Statement.group] (auto_total),
    [Statement.eval], [Statement.pp], [Statement.lines]. *)

open Orcaset2

(* Assumptions *)

let start_date = Date.make 2025 1 1
let recurring_first = 10000.0
let recurring_growth = 0.05
let non_recurring_first = 2000.0
let non_recurring_drift = 50.0
let non_recurring_volatility = 500.0
let seed = 42
let recurring_cost_pct = -0.30
let non_recurring_cost_pct = -0.40
let admin_first = -1500.0
let admin_rate = 0.03

(* Timeline *)

let tl = Timeline.monthly ~start_date ~n:12

(* Revenue *)

let recurring_revenue =
  Formula.growth_simple ~name:"Recurring Revenue" ~start_date ~rate:recurring_growth recurring_first

(* Non-recurring revenue: random walk with drift and volatility.
   Formula.scan accumulates over a shock series -- each period's value
   depends on the previous period's output, drift, and a random shock. *)
let rng = Random.State.make [| seed |]

(* Approximate normal shocks via sum of 12 uniforms (central limit theorem). *)
let shocks =
  Formula.init ~name:"Shocks" (fun _i _p ->
      let sum_uniforms =
        List.init 12 (fun _ -> Random.State.float rng 1.0) |> List.fold_left ( +. ) 0.0
      in
      (sum_uniforms -. 6.0) *. non_recurring_volatility)

(* scan ~init feeds the previous output back as ~acc, building a running walk. *)
let non_recurring_revenue =
  Formula.scan ~name:"Non-Recurring Revenue" ~init:non_recurring_first
    (fun ~acc ~x -> acc +. non_recurring_drift +. x)
    shocks

(* Cost of Revenue *)

let recurring_cost =
  Formula.named "Recurring Cost" (Formula.scale recurring_cost_pct recurring_revenue)

let non_recurring_cost =
  Formula.named "Non-Recurring Cost" (Formula.scale non_recurring_cost_pct non_recurring_revenue)

(* Admin Expenses *)

let admin = Formula.growth_simple ~name:"Admin" ~start_date ~rate:admin_rate admin_first

(* Statement *)

(* Nested groups mirror the hierarchy from the docstring above.
   No explicit totals are needed -- Statement.eval calls auto_total,
   which synthesizes a total for each group by summing its children. *)
let income_statement =
  let open Statement in
  group "Operating Income"
    [
      group "Gross Profit"
        [
          group "Revenue"
            [ line "Recurring" recurring_revenue; line "Non-Recurring" non_recurring_revenue ];
          group "Cost of Revenue"
            [ line "Recurring" recurring_cost; line "Non-Recurring" non_recurring_cost ];
        ];
      line "Admin Expenses" admin;
    ]

(* Output *)

let format_float ?(width = 12) value =
  let abs_value = Float.abs value in
  let sign = if value < 0.0 then "-" else "" in
  let int_part = Int64.of_float abs_value in
  let rec add_commas n acc =
    if Int64.compare n 1000L < 0 then Int64.to_string n :: acc
    else
      let remainder = Int64.rem n 1000L in
      let quotient = Int64.div n 1000L in
      add_commas quotient (Printf.sprintf "%03Ld" remainder :: acc)
  in
  let formatted =
    if Int64.compare int_part 0L = 0 then "0" else String.concat "," (add_commas int_part [])
  in
  let s = sign ^ formatted in
  let pad = max 0 (width - String.length s) in
  String.make pad ' ' ^ s

let () =
  let results = Statement.eval tl income_statement in
  let l =
    Statement.layout ~label_width:30 ~col_width:12
      ~col_header:(fun i -> Date.to_string (Timeline.period_start tl i))
      ~pp_num:(fun ppf v -> Format.pp_print_string ppf (format_float v))
      tl
  in
  let ppf = Format.std_formatter in

  Format.fprintf ppf "=== INCOME STATEMENT ===@.@.";
  Statement.pp l ppf results;

  (* Statement.lines flattens the tree into leaf (label, data) pairs,
     skipping group totals -- useful for extracting individual line items. *)
  Format.fprintf ppf "@.=== LINE ITEMS ===@.";
  let lines = Statement.lines results in
  List.iter (fun (label, _) -> Format.fprintf ppf "- %s@." label) lines;

  (* Dependency graph *)
  let oc = open_out "model.dot" in
  let dot_ppf = Format.formatter_of_out_channel oc in
  Formula.Deps.pp_dot dot_ppf
    [ recurring_revenue; non_recurring_revenue; recurring_cost; non_recurring_cost; admin ];
  Format.pp_print_flush dot_ppf ();
  close_out oc
