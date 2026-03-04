(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Types And Constructors *)

type 'a item =
  | Line of { label : string; data : 'a }
  | Group of { label : string; items : 'a item list; total : 'a option }

let line label data = Line { label; data }
let group ?total label items = Group { label; items; total }
let flow_line label f = Line { label; data = Flow.formula f }
let balance_line label b = Line { label; data = Balance.formula b }

(* Traversal *)

let rec fold ~line_fn ~group_fn item =
  match item with
  | Line { label; data } -> line_fn label data
  | Group { label; items; total } ->
      let folded = List.map (fold ~line_fn ~group_fn) items in
      group_fn label folded total

let rec iter ~line_fn ~group_fn item =
  match item with
  | Line { label; data } -> line_fn label data
  | Group { label; items; total } ->
      group_fn label total `Enter;
      List.iter (iter ~line_fn ~group_fn) items;
      group_fn label total `Exit

let rec map f item =
  match item with
  | Line { label; data } -> Line { label; data = f data }
  | Group { label; items; total } ->
      Group { label; items = List.map (map f) items; total = Option.map f total }

let lines item =
  fold item
    ~line_fn:(fun label data -> [ (label, data) ])
    ~group_fn:(fun _label children _total -> List.concat children)

(* Auto-totaling *)

let direct_data = function Line { data; _ } -> Some data | Group { total; _ } -> total

(* Bottom-up: recurse into children first so nested groups have their
   totals filled in before the parent tries to aggregate them. Only
   synthesizes a total when one is missing; explicit totals are preserved
   so callers can provide custom calculations (e.g. net income that
   subtracts rather than sums). *)
let rec auto_total item =
  match item with
  | Line _ -> item
  | Group { label; items; total } ->
      let items = List.map auto_total items in
      let total =
        match total with
        | Some _ -> total
        | None -> (
            match List.filter_map direct_data items with
            | [] -> None
            | child_series -> Some (Formula.sum ~name:("Total " ^ label) child_series))
      in
      Group { label; items; total }

(* Evaluation *)

let rec collect_series item =
  match item with
  | Line { data; _ } -> [ data ]
  | Group { items; total; _ } -> (
      let child_series = List.concat_map collect_series items in
      match total with Some s -> s :: child_series | None -> child_series)

(* Two-pass evaluation strategy:
   1. Collect every Formula.t from the tree into a flat list
   2. Evaluate them all at once via eval_many (shared memoization context)
   3. Map results back into the tree using physical equality (List.assq)
   This avoids evaluating shared sub-series multiple times and ensures
   the entire statement is consistent against a single evaluation pass. *)
let eval tl item =
  let item = auto_total item in
  let all_series = collect_series item in
  let results = Formula.eval_many tl all_series in
  let pairs = List.combine all_series results in
  map (fun s -> List.assq s pairs) item

let eval_materialized tl item =
  let item = auto_total item in
  let all_series = collect_series item in
  let results = Formula.eval_many tl all_series in
  let pairs = List.combine all_series results in
  map (fun s ->
    let vs = List.assq s pairs in
    Formula.Materialized.make tl vs
  ) item

(* Pretty-printing *)

type layout = {
  n : int;
  label_width : int;
  col_width : int;
  col_header : int -> string;
  pp_num : Format.formatter -> float -> unit;
  sep : char;
}

let default_col_header tl i =
  let month_name = function
    | 1 -> "Jan"
    | 2 -> "Feb"
    | 3 -> "Mar"
    | 4 -> "Apr"
    | 5 -> "May"
    | 6 -> "Jun"
    | 7 -> "Jul"
    | 8 -> "Aug"
    | 9 -> "Sep"
    | 10 -> "Oct"
    | 11 -> "Nov"
    | 12 -> "Dec"
    | _ -> "???"
  in
  month_name (Date.month (Timeline.period_start tl i))

let default_pp_num col_width ppf v = Format.fprintf ppf "%*.*f" col_width 0 v

let layout ?(label_width = 25) ?(col_width = 10) ?col_header ?pp_num ?(sep = '-') tl =
  let n = Timeline.length tl in
  let col_header = match col_header with Some f -> f | None -> default_col_header tl in
  let pp_num = match pp_num with Some f -> f | None -> default_pp_num col_width in
  { n; label_width; col_width; col_header; pp_num; sep }

let pp_separator l ppf =
  let total_width = l.label_width + (l.n * l.col_width) in
  Format.fprintf ppf "%s@." (String.make total_width l.sep)

let pp_row l ppf label values =
  Format.fprintf ppf "%-*s" l.label_width label;
  for i = 0 to l.n - 1 do
    l.pp_num ppf values.(i)
  done;
  Format.fprintf ppf "@."

let pp l ppf item =
  Format.fprintf ppf "%-*s" l.label_width "";
  for i = 0 to l.n - 1 do
    Format.fprintf ppf "%*s" l.col_width (l.col_header i)
  done;
  Format.fprintf ppf "@.";
  pp_separator l ppf;
  iter item
    ~line_fn:(fun label values -> pp_row l ppf (Printf.sprintf "  %s" label) values)
    ~group_fn:(fun label total phase ->
      match phase with
      | `Enter -> Format.fprintf ppf "%-*s@." l.label_width label
      | `Exit ->
          Option.iter (fun values -> pp_row l ppf (Printf.sprintf "  Total %s" label) values) total;
          pp_separator l ppf)
