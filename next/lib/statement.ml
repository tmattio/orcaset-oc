(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Types And Constructors *)

type 'a item =
  | Line of { label : string; data : 'a }
  | Group of { label : string; items : 'a item list; total : 'a option }

type 'c series = Flow : 'c Flow.t -> 'c series | Balance : 'c Balance.t -> 'c series

let line label data = Line { label; data }
let group ?total label items = Group { label; items; total }
let flow f = Flow f
let balance b = Balance b

let to_formula : type c. c series -> c Formula.t = function
  | Flow f -> Flow.unsafe_to_formula f
  | Balance b -> Balance.unsafe_to_formula b

let flow_line label f = Line { label; data = Flow f }
let balance_line label b = Line { label; data = Balance b }
let flow_group ?total label items = group ?total:(Option.map (fun t -> Flow t) total) label items

let balance_group ?total label items =
  group ?total:(Option.map (fun t -> Balance t) total) label items

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

(* Classify direct children to determine whether auto-summing is safe.
   Mixed flow/balance groups skip auto-total since the sum would be
   semantically meaningless. *)
type kind = All_flows | All_balances | Mixed

let classify_children items =
  let tags =
    List.filter_map
      (fun item -> match item with Line { data; _ } -> Some data | Group { total; _ } -> total)
      items
  in
  match tags with
  | [] -> Mixed
  | first :: rest ->
      let tag = function Flow _ -> `F | Balance _ -> `B in
      let t = tag first in
      if List.for_all (fun s -> tag s = t) rest then
        match t with `F -> All_flows | `B -> All_balances
      else Mixed

let direct_data_formula = function Line { data; _ } -> Some data | Group { total; _ } -> total

let rec auto_total : type c. c series item -> c Formula.t item = function
  | Line { label; data } -> Line { label; data = to_formula data }
  | Group { label; items; total } ->
      let kind = classify_children items in
      let converted = List.map auto_total items in
      let total =
        match total with
        | Some t -> Some (to_formula t)
        | None when kind = Mixed -> None
        | None -> (
            match List.filter_map direct_data_formula converted with
            | [] -> None
            | child_formulas -> Some (Formula.sum ~name:("Total " ^ label) child_formulas))
      in
      Group { label; items = converted; total }

(* Evaluation *)

let rec collect_series item =
  match item with
  | Line { data; _ } -> [ data ]
  | Group { items; total; _ } -> (
      let child_series = List.concat_map collect_series items in
      match total with Some s -> s :: child_series | None -> child_series)

let eval tl item =
  let item = auto_total item in
  let all_series = collect_series item in
  let results = Formula.eval_many tl all_series in
  let pairs = List.combine all_series results in
  map (fun s -> List.assq s pairs) item

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
