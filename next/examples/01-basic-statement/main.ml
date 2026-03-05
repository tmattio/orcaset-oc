(** Basic Income Statement

    Builds on the Coffee Shop example by introducing cross-period dependencies with [Flow.feedback]
    and custom timeline construction via [Timeline.make] + [Period.make_offset].

    The dependency graph is naturally acyclic except for services revenue, which looks at the
    *prior* period's OpEx via [Flow.feedback].

    Key patterns demonstrated:
    - [Flow.t] for all revenue and expense line items.
    - [Flow.feedback] for cross-period dependency (services reads prior OpEx).
    - [Statement.flow_line] for typed statement construction. *)

open Orcaset2

(* Assumptions *)

let start_date = Date.make 2025 1 1
let software_first = 1000.
let software_growth = 0.1
let services_multiple_opex = -0.5
let cogs_pct = -0.3
let admin_first = -100.
let admin_rate = 0.05

(* Timeline *)

(* 4 quarterly periods starting Jan 2025. Timeline.make accepts a
   Period.offset for custom step sizes; for standard calendars prefer
   Timeline.quarterly or Timeline.monthly. *)

let tl = Timeline.make ~start_date ~offset:(Period.make_offset ~quarters:1 ()) ~n:4

(* Revenue *)

let software =
  Flow.growth_simple ~name:"Software" ~start_date ~rate:software_growth software_first

(* Operating Expenses *)

let cogs = Flow.named "COGS" (Flow.scale cogs_pct software)
let admin = Flow.growth_simple ~name:"Admin" ~start_date ~rate:admin_rate admin_first
let opex_total = Flow.sum ~name:"OpEx Total" [ cogs; admin ]

(* Revenue (Continued) *)

(* Flow.feedback ties a cross-period dependency: services revenue depends on
   the *prior* period's total OpEx. The feedback function receives a flow
   representing the previous period's value (default 0.0 at period 0).
   We feed back opex_total so that services can read the prior period. *)
let _opex_fed_back, services =
  Flow.feedback ~default:0.0 (fun prev_opex ->
      let services =
        Flow.map ~name:"Services"
          (fun prev -> if prev = 0.0 then 500.0 else prev *. services_multiple_opex)
          prev_opex
      in
      (opex_total, (opex_total, services)))

let revenue_total = Flow.named "Revenue Total" (Flow.add software services)

(* Income *)

let income = Flow.named "Income" (Flow.add revenue_total opex_total)

(* Statement *)

let income_statement =
  let open Statement in
  group "Income Statement"
    [
      group "Revenue" [ flow_line "Software" software; flow_line "Services" services ];
      group "Operating Expenses" [ flow_line "COGS" cogs; flow_line "Admin" admin ];
      flow_line "Income" income;
    ]

(* Output *)

let () =
  let results = Statement.eval tl income_statement in
  let l =
    Statement.layout ~label_width:30 ~col_width:14
      ~col_header:(fun i -> Date.to_string (Timeline.period_start tl i))
      ~pp_num:(fun ppf v -> Format.fprintf ppf "%14.2f" v)
      tl
  in
  Statement.pp l Format.std_formatter results;

  (* Dependency graph *)
  let oc = open_out "model.dot" in
  let ppf = Format.formatter_of_out_channel oc in
  Flow.Deps.pp_dot ppf [ income ];
  Format.pp_print_flush ppf ();
  close_out oc
