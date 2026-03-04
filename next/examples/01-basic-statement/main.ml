(** Basic Income Statement

    Builds on the Coffee Shop example by introducing cross-period dependencies with [Formula.prev]
    and custom timeline construction via [Timeline.make] + [Period.make_offset].

    The dependency graph is naturally acyclic -- no delay or lazy needed -- except for services
    revenue, which looks at the *prior* period's OpEx:

    software --> cogs ---> opex_total ---> services --> revenue_total software
    ---------------------------> --> revenue_total admin --> opex_total *)

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
  Formula.growth_simple ~name:"Software" ~start_date ~rate:software_growth software_first

(* Operating Expenses *)

let cogs = Formula.named "COGS" (Formula.scale cogs_pct software)
let admin = Formula.growth_simple ~name:"Admin" ~start_date ~rate:admin_rate admin_first
let opex_total = Formula.sum ~name:"OpEx Total" [ cogs; admin ]

(* Revenue (Continued) *)

(* Formula.prev shifts a series forward by one period, producing ~default
   at period 0. Combined with Formula.map, this lets services revenue
   depend on the *prior* period's OpEx without introducing a same-period
   cycle. Period 0 falls back to a fixed estimate since there is no
   prior OpEx yet. *)
let services =
  Formula.map ~name:"Services"
    (fun prev_opex -> if prev_opex = 0.0 then 500.0 else prev_opex *. services_multiple_opex)
    (Formula.prev opex_total ~default:0.0)

let revenue_total = Formula.named "Revenue Total" (Formula.add software services)

(* Income *)

let income = Formula.named "Income" (Formula.add revenue_total opex_total)

(* Statement *)

let income_statement =
  let open Statement in
  group "Income Statement"
    [
      group "Revenue" [ line "Software" software; line "Services" services ];
      group "Operating Expenses" [ line "COGS" cogs; line "Admin" admin ];
      line "Income" income;
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
  Formula.Deps.pp_dot ppf [ income ];
  Format.pp_print_flush ppf ();
  close_out oc
