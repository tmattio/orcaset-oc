(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Balance: point-in-time quantities *)

type 'c t = B of 'c Formula.t [@@unboxed]

(* Constructors *)

let const ?name v = B (Formula.const ?name v)
let init ?name f = B (Formula.init_flow ?name f)

(* Bridge from Flow *)

let roll_forward ?name ~init flow =
  B (Formula.cumsum ?name ~init (Flow.formula flow))

let roll_forward_with ?name ~init f flow =
  B (Formula.scan ?name ~init f (Flow.formula flow))

(* Naming *)

let named name (B s) = B (Formula.named name s)

(* Algebra *)

let add (B a) (B b) = B (Formula.add a b)
let sub (B a) (B b) = B (Formula.sub a b)
let scale k (B s) = B (Formula.scale k s)
let map ?name f (B s) = B (Formula.map ?name f s)
let map2 ?name f (B a) (B b) = B (Formula.map2 ?name f a b)

(* Cross-period *)

let prev ?name (B src) ~default = B (Formula.prev ?name src ~default)
let at_period_start ?name b ~default = prev ?name b ~default
let at_period_end b = b

(* Bridge to Flow *)

let change (B s) ~default =
  let prev_s = Formula.prev s ~default in
  Flow.of_formula (Formula.sub s prev_s)

(* Feedback *)

let feedback ?name ~default f =
  Formula.feedback ?name ~default (fun prev_formula ->
    let (B def), exposed = f (B prev_formula) in
    (def, exposed))

let fixpoint ?name ?tol ?max_iter ~guess f =
  B (Formula.fixpoint ?name ?tol ?max_iter ~guess (fun var ->
    let (B body) = f (B var) in
    body))

(* Escape hatches *)

let formula (B s) = s
let of_formula s = B s
let of_array ?name arr = B (Formula.of_array ?name arr)

(* Materialized *)

module Materialized = struct
  type 'c t = { timeline : Timeline.t; values : float array }
  type interp = Step | Linear

  let make tl values =
    if Array.length values <> Timeline.length tl then
      invalid_arg "Balance.Materialized.make: array length does not match timeline length";
    { timeline = tl; values }

  let timeline m = m.timeline
  let to_array m = Array.copy m.values
  let unsafe_values m = m.values
  let length m = Array.length m.values
  let get m i = m.values.(i)
  let period m i = Timeline.get m.timeline i

  let to_list m =
    let n = length m in
    let rec loop acc i =
      if i < 0 then acc
      else loop ((period m i, m.values.(i)) :: acc) (i - 1)
    in
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

  let at ?(interp = Linear) ?split_fn m ~flow date =
    let tl = m.timeline in
    match interp with
    | Step ->
        Formula.Query.interpolate ?split_fn tl m.values date
    | Linear ->
        Formula.Query.balance_at ?split_fn tl
          ~balance:m.values
          ~flow:(Flow.Materialized.unsafe_values flow)
          date
end

(* Evaluation *)

let eval tl (B s) =
  let values = Formula.eval tl s in
  { Materialized.timeline = tl; values }

let eval_with_flow tl (B s) ~flow =
  let flow_formula = Flow.formula flow in
  match Formula.eval_many tl [ s; flow_formula ] with
  | [ bal_arr; flow_arr ] ->
      let bal_mat = Materialized.make tl bal_arr in
      let flow_mat = Flow.Materialized.make tl flow_arr in
      (bal_mat, flow_mat)
  | _ -> assert false

let eval_values tl (B s) = Formula.eval tl s
