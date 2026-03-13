(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type 'c t = (Formula.flow_kind, 'c) Formula.t

exception Cycle_error = Formula.Cycle_error
exception Convergence_error = Formula.Convergence_error

(* Constructors *)

let const ?name v = Formula.const ?name Formula.Flow_k v
let init ?name f = Formula.init ?name Formula.Flow_k (fun _i p -> f p)
let init_indexed ?name f = Formula.init ?name Formula.Flow_k f
let of_array ?name arr = Formula.of_array ?name Formula.Flow_k arr
let unsafe_of_array ?name arr = of_array ?name arr
let of_events ?name events = Formula.of_events ?name events
let of_periods ?name ?prorater pairs = Formula.of_periods ?name ?prorater pairs

let growth_simple ?name ?daycount ~start_date ~rate initial =
  Formula.growth_simple ?name ?daycount ~start_date ~rate initial

let growth_compound ?name ?daycount ~start_date ~rate initial =
  Formula.growth_compound ?name ?daycount ~start_date ~rate initial

let year_frac ?name daycount = Formula.year_frac ?name daycount

(* Naming / algebra *)

let named = Formula.named
let add = Formula.add
let sub = Formula.sub
let scale = Formula.scale
let neg = Formula.neg
let sum ?name fs = Formula.sum ?name Formula.Flow_k fs

(* Cell-local *)

let map = Formula.map
let map2 = Formula.map2
let mul = Formula.mul
let div = Formula.div
let abs = Formula.abs
let min = Formula.min
let max = Formula.max
let clamp = Formula.clamp
let round = Formula.round
let where ~cond ~then_ ~else_ = Formula.where ~cond ~then_ ~else_

(* Cross-period / feedback *)

let prev ?name src ~default = Formula.prev ?name src ~default
let scan ?name ~init f flow = Formula.scan ?name ~init f flow

let window ?name ?prorater ~start ~end_ flow =
  Formula.flow_window ?name ?prorater ~start_ref:start ~end_ref:end_ flow

let feedback ?name ~default f = Formula.feedback ?name ~kind:Formula.Flow_k ~default f

let fixpoint ?name ?tol ?max_iter ~guess f =
  Formula.fixpoint ?name ~kind:Formula.Flow_k ?tol ?max_iter ~guess f

(* Conversion *)

let convert ~rate s = Formula.convert ~rate s

(* Escape hatches *)

let unsafe_to_formula f = f
let unsafe_of_formula f = f

(* Materialized *)

module Materialized = struct
  type query_mode = Formula.query_mode = Exact | Approx
  type 'c t = { timeline : Timeline.t; values : float array; query : Formula.flow_query }

  let make timeline values =
    if Array.length values <> Timeline.length timeline then
      invalid_arg "Flow.Materialized.make: array length does not match timeline length";
    let query = Formula.flow_query timeline values (Formula.of_array Formula.Flow_k values) in
    { timeline; values; query = { query with mode = Approx } }

  let query_mode m = m.query.mode
  let timeline m = m.timeline
  let to_array m = Array.copy m.values
  let unsafe_values m = m.values
  let length m = Array.length m.values
  let get m i = m.values.(i)
  let period m i = Timeline.get m.timeline i
  let to_list m = List.init (length m) (fun i -> (period m i, m.values.(i)))

  let iter f m =
    for i = 0 to length m - 1 do
      f (period m i) m.values.(i)
    done

  let fold f init m =
    let acc = ref init in
    for i = 0 to length m - 1 do
      acc := f !acc (period m i) m.values.(i)
    done;
    !acc

  let accrue ?(prorater = Formula.default_prorater) m ~start_date ~end_date =
    m.query.accrue ~prorater ~start_date ~end_date
end

let eval tl f =
  let values = Formula.eval tl f in
  let query = Formula.flow_query tl values f in
  { Materialized.timeline = tl; values; query }

let eval_values tl f = Formula.eval tl f

let eval_many tl flows =
  let value_arrays = Formula.eval_many tl flows in
  List.map2
    (fun flow values ->
      let query = Formula.flow_query tl values flow in
      { Materialized.timeline = tl; values; query })
    flows value_arrays

module Deps = struct
  type node = Formula.Deps.node = { id : int; name : string option; kind : string }
  type edge = Formula.Deps.edge = { src : int; dst : int }

  let graph ?named_only flows = Formula.Deps.graph ?named_only flows
  let pp_dot ?named_only ppf flows = Formula.Deps.pp_dot ?named_only ppf flows
end

module Syntax = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( *$ ) = scale
end
