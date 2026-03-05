(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Flow: interval quantities *)

type query_hint =
  | H_events of (Date.t * float) list
  | H_source_periods of { pairs : (Period.t * float) list; split_fn : Formula.Query.split_fn }
  | H_sum of query_hint list
  | H_scale of float * query_hint
  | H_neg of query_hint
  | H_no_hint

type 'c t = { formula : 'c Formula.t; hint : query_hint }
type split_fn = Formula.Query.split_fn

let default_split_fn = Formula.Query.default_split_fn

exception Cycle_error = Formula.Cycle_error
exception Convergence_error = Formula.Convergence_error

let mk ?hint formula = { formula; hint = (match hint with Some h -> h | None -> H_no_hint) }

(* Constructors *)

let const ?name v = mk (Formula.const ?name v)
let init ?name f = mk (Formula.init_flow ?name f)
let init_indexed ?name f = mk (Formula.init ?name f)
let of_events ?name events = mk ~hint:(H_events events) (Formula.of_events ?name events)

let of_periods ?name ?(split_fn = Formula.Query.default_split_fn) pairs =
  let formula =
    Formula.init_tl ?name (fun _tl _i period ->
        List.fold_left
          (fun acc (src_p, v) ->
            let ov_start = Date.max (Period.start_date src_p) (Period.start_date period) in
            let ov_end = Date.min (Period.end_date src_p) (Period.end_date period) in
            if Date.compare ov_start ov_end >= 0 then acc
            else
              let _, after =
                split_fn ~start_date:(Period.start_date src_p) ~end_date:(Period.end_date src_p)
                  ~split_date:ov_start ~value:v
              in
              let contribution, _ =
                split_fn ~start_date:ov_start ~end_date:(Period.end_date src_p) ~split_date:ov_end
                  ~value:after
              in
              acc +. contribution)
          0.0 pairs)
  in
  { formula; hint = H_source_periods { pairs; split_fn } }

let growth_simple ?name ?daycount ~start_date ~rate initial =
  mk (Formula.growth_simple ?name ?daycount ~start_date ~rate initial)

let growth_compound ?name ?daycount ~start_date ~rate initial =
  mk (Formula.growth_compound ?name ?daycount ~start_date ~rate initial)

let year_frac ?name daycount = mk (Formula.year_frac ?name daycount)

(* Naming *)

let named name f = { formula = Formula.named name f.formula; hint = f.hint }

(* Exact algebra *)

let add a b = { formula = Formula.add a.formula b.formula; hint = H_sum [ a.hint; b.hint ] }
let sub a b = { formula = Formula.sub a.formula b.formula; hint = H_sum [ a.hint; H_neg b.hint ] }
let scale k s = { formula = Formula.scale k s.formula; hint = H_scale (k, s.hint) }
let neg s = { formula = Formula.neg s.formula; hint = H_neg s.hint }

let sum ?name fs =
  {
    formula = Formula.sum ?name (List.map (fun f -> f.formula) fs);
    hint = H_sum (List.map (fun f -> f.hint) fs);
  }

(* Cell-local combinators *)

let map ?name f s = mk (Formula.map ?name f s.formula)
let map2 ?name f a b = mk (Formula.map2 ?name f a.formula b.formula)
let mul a b = mk (Formula.mul a.formula b.formula)
let div a b = mk (Formula.div a.formula b.formula)
let abs s = mk (Formula.abs s.formula)
let min a b = mk (Formula.min a.formula b.formula)
let max a b = mk (Formula.max a.formula b.formula)
let clamp ~lo ~hi s = mk (Formula.clamp ~lo ~hi s.formula)
let round digits s = mk (Formula.round digits s.formula)

(* Conditional *)

let where ~cond ~then_ ~else_ =
  mk (Formula.where ~cond:cond.formula ~then_:then_.formula ~else_:else_.formula)

(* Cross-period *)

let prev ?name src ~default = mk (Formula.prev ?name src.formula ~default)
let scan ?name ~init f flow = mk (Formula.scan ?name ~init f flow.formula)

(* Feedback *)

let feedback ?name ~default f =
  Formula.feedback ?name ~default (fun prev_formula ->
      let prev_flow = { formula = prev_formula; hint = H_no_hint } in
      let def, exposed = f prev_flow in
      (def.formula, exposed))

let fixpoint ?name ?tol ?max_iter ~guess f =
  mk
    (Formula.fixpoint ?name ?tol ?max_iter ~guess (fun var ->
         let body = f { formula = var; hint = H_no_hint } in
         body.formula))

(* Currency conversion *)

let convert ~rate s = { formula = Formula.convert ~rate s.formula; hint = H_scale (rate, s.hint) }

(* Unsafe escape hatches *)

let unsafe_to_formula f = f.formula
let unsafe_of_formula s = { formula = s; hint = H_no_hint }
let unsafe_of_array ?name arr = mk (Formula.of_array ?name arr)
let of_array = unsafe_of_array

(* Materialized *)

module Materialized = struct
  type query_ctx =
    | Q_events of (Date.t * float) list
    | Q_source_periods of { pairs : (Period.t * float) list; split_fn : Formula.Query.split_fn }
    | Q_sum of query_ctx list
    | Q_scale of float * query_ctx
    | Q_neg of query_ctx
    | Q_cell_based

  type 'c t = { timeline : Timeline.t; values : float array; query : query_ctx }

  let make tl values =
    if Array.length values <> Timeline.length tl then
      invalid_arg "Flow.Materialized.make: array length does not match timeline length";
    { timeline = tl; values; query = Q_cell_based }

  let query m = m.query
  let timeline m = m.timeline
  let to_array m = Array.copy m.values
  let unsafe_values m = m.values
  let length m = Array.length m.values
  let get m i = m.values.(i)
  let period m i = Timeline.get m.timeline i

  let to_list m =
    let n = length m in
    let rec loop acc i = if i < 0 then acc else loop ((period m i, m.values.(i)) :: acc) (i - 1) in
    loop [] (n - 1)

  let fold f init m =
    let acc = ref init in
    for i = 0 to Array.length m.values - 1 do
      acc := f !acc (Timeline.get m.timeline i) m.values.(i)
    done;
    !acc

  let iter f m =
    for i = 0 to Array.length m.values - 1 do
      f (Timeline.get m.timeline i) m.values.(i)
    done

  let rec accrue_via_ctx ctx ~start_date ~end_date =
    match ctx with
    | Q_events events ->
        List.fold_left
          (fun acc (d, v) ->
            if Date.compare d start_date >= 0 && Date.compare d end_date < 0 then acc +. v else acc)
          0.0 events
    | Q_source_periods { pairs; split_fn } ->
        List.fold_left
          (fun acc (src_p, v) ->
            let ov_start = Date.max (Period.start_date src_p) start_date in
            let ov_end = Date.min (Period.end_date src_p) end_date in
            if Date.compare ov_start ov_end >= 0 then acc
            else
              let _, after =
                split_fn ~start_date:(Period.start_date src_p) ~end_date:(Period.end_date src_p)
                  ~split_date:ov_start ~value:v
              in
              let contribution, _ =
                split_fn ~start_date:ov_start ~end_date:(Period.end_date src_p) ~split_date:ov_end
                  ~value:after
              in
              acc +. contribution)
          0.0 pairs
    | Q_sum ctxs ->
        List.fold_left (fun acc c -> acc +. accrue_via_ctx c ~start_date ~end_date) 0.0 ctxs
    | Q_scale (k, c) -> k *. accrue_via_ctx c ~start_date ~end_date
    | Q_neg c -> -.accrue_via_ctx c ~start_date ~end_date
    | Q_cell_based -> raise_notrace Exit

  let accrue ?split_fn m ~start_date ~end_date =
    match accrue_via_ctx m.query ~start_date ~end_date with
    | v -> v
    | exception Exit -> Formula.Query.accrue ?split_fn m.timeline m.values ~start_date ~end_date
end

(* Dependency graph *)

module Deps = struct
  type node = Formula.Deps.node = { id : int; name : string option; kind : string }
  type edge = Formula.Deps.edge = { src : int; dst : int }

  let graph ?named_only flows = Formula.Deps.graph ?named_only (List.map (fun f -> f.formula) flows)

  let pp_dot ?named_only ppf flows =
    Formula.Deps.pp_dot ?named_only ppf (List.map (fun f -> f.formula) flows)
end

(* Infix syntax *)

module Syntax = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( *$ ) k s = scale k s
end

(* Evaluation *)

let rec hint_to_ctx = function
  | H_events events -> Materialized.Q_events events
  | H_source_periods { pairs; split_fn } -> Materialized.Q_source_periods { pairs; split_fn }
  | H_sum hs -> Materialized.Q_sum (List.map hint_to_ctx hs)
  | H_scale (k, h) -> Materialized.Q_scale (k, hint_to_ctx h)
  | H_neg h -> Materialized.Q_neg (hint_to_ctx h)
  | H_no_hint -> Materialized.Q_cell_based

let eval tl f =
  let values = Formula.eval tl f.formula in
  { Materialized.timeline = tl; values; query = hint_to_ctx f.hint }

let eval_values tl f = Formula.eval tl f.formula

let eval_many tl flows =
  let formulas = List.map (fun f -> f.formula) flows in
  let value_arrays = Formula.eval_many tl formulas in
  List.map2
    (fun f values -> { Materialized.timeline = tl; values; query = hint_to_ctx f.hint })
    flows value_arrays
