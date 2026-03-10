(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type 'c t = (Formula.balance_kind, 'c) Formula.t

(* Constructors *)

let const ?name v = Formula.const ?name Formula.Balance_k v
let init ?name f = Formula.init ?name Formula.Balance_k (fun _i p -> f p)
let of_array ?name arr = Formula.of_array ?name Formula.Balance_k arr
let unsafe_of_array ?name arr = of_array ?name arr

let of_dates ?name ?before_first observations =
  Formula.of_observations ?name ?before_first observations

let of_observations = of_dates

(* Bridge from Flow *)

let roll_forward ?name ~init flow = Formula.roll_forward ?name ~init (Flow.unsafe_to_formula flow)

let roll_forward_with ?name ~init f flow =
  Formula.accumulate ?name ~init f (Flow.unsafe_to_formula flow)

(* Naming / algebra *)

let named = Formula.named
let add = Formula.add
let sub = Formula.sub
let scale = Formula.scale
let neg = Formula.neg
let map = Formula.map
let map2 = Formula.map2
let mul = Formula.mul
let div = Formula.div
let abs = Formula.abs
let min = Formula.min
let max = Formula.max
let clamp = Formula.clamp
let round = Formula.round
let where ~cond ~then_ ~else_ = Formula.where ~cond:(Flow.unsafe_to_formula cond) ~then_ ~else_

(* Cross-period / bridges *)

let prev ?name src ~default = Formula.prev ?name src ~default
let at_period_start ?name b ~default = prev ?name b ~default
let at_period_end b = b
let sample ?name b = Flow.unsafe_of_formula (Formula.sample ?name b)

let change b ~default =
  let cur = sample b in
  let prev = Flow.prev cur ~default in
  Flow.sub cur prev

(* Feedback / fixpoint *)

let feedback ?name ~default f = Formula.feedback ?name ~kind:Formula.Balance_k ~default f

let fixpoint ?name ?tol ?max_iter ~guess f =
  Formula.fixpoint ?name ~kind:Formula.Balance_k ?tol ?max_iter ~guess f

(* Conversion *)

let convert ~rate s = Formula.convert ~rate s

(* Escape hatches *)

let unsafe_to_formula b = b
let unsafe_of_formula b = b

(* Materialized *)

module Materialized = struct
  type query_mode = Exact | Approx

  type 'c t = {
    timeline : Timeline.t;
    values : float array;
    exact_at : (prorater:Formula.prorater -> Date.t -> float) option;
  }

  let make timeline values =
    if Array.length values <> Timeline.length timeline then
      invalid_arg "Balance.Materialized.make: array length does not match timeline length";
    { timeline; values; exact_at = None }

  let query_mode m = match m.exact_at with Some _ -> Exact | None -> Approx
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

  let at ?(prorater = Formula.default_prorater) m date =
    match Timeline.find_index m.timeline date with
    | None ->
        invalid_arg
          (Printf.sprintf "Balance.Materialized.at: date %s is outside the timeline"
             (Date.to_string date))
    | Some i -> ( match m.exact_at with Some f -> f ~prorater date | None -> m.values.(i))
end

let eval tl b =
  let values = Formula.eval tl b in
  let exact_at = Formula.exact_at tl b in
  { Materialized.timeline = tl; values; exact_at }

let eval_values tl b = Formula.eval tl b

module Deps = struct
  type node = Formula.Deps.node = { id : int; name : string option; kind : string }
  type edge = Formula.Deps.edge = { src : int; dst : int }

  let graph ?named_only balances = Formula.Deps.graph ?named_only balances
  let pp_dot ?named_only ppf balances = Formula.Deps.pp_dot ?named_only ppf balances
end
