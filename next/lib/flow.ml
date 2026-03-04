(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Flow: interval quantities *)

type 'c t = F of 'c Formula.t [@@unboxed]

(* Constructors *)

let const ?name v = F (Formula.const ?name v)
let init ?name f = F (Formula.init_flow ?name f)
let of_events ?name events = F (Formula.of_events ?name events)

let of_periods ?name pairs =
  F (Formula.init_flow ?name (fun period ->
    List.fold_left (fun acc (p, v) ->
      if Period.equal p period then acc +. v else acc)
    0.0 pairs))

let growth_simple ?name ?daycount ~start_date ~rate initial =
  F (Formula.growth_simple ?name ?daycount ~start_date ~rate initial)

let growth_compound ?name ?daycount ~start_date ~rate initial =
  F (Formula.growth_compound ?name ?daycount ~start_date ~rate initial)

let year_frac ?name daycount = F (Formula.year_frac ?name daycount)

(* Naming *)

let named name (F s) = F (Formula.named name s)

(* Exact algebra *)

let add (F a) (F b) = F (Formula.add a b)
let sub (F a) (F b) = F (Formula.sub a b)
let scale k (F s) = F (Formula.scale k s)
let neg (F s) = F (Formula.neg s)
let sum ?name fs = F (Formula.sum ?name (List.map (fun (F s) -> s) fs))

(* Escape hatches *)

let formula (F s) = s
let of_formula s = F s
let of_array ?name arr = F (Formula.of_array ?name arr)

(* Materialized *)

module Materialized = struct
  type 'c t = { timeline : Timeline.t; values : float array }

  let make tl values =
    if Array.length values <> Timeline.length tl then
      invalid_arg "Flow.Materialized.make: array length does not match timeline length";
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

  let accrue ?split_fn m ~start_date ~end_date =
    Formula.Query.accrue ?split_fn m.timeline m.values ~start_date ~end_date
end

(* Evaluation *)

let eval tl (F s) =
  let values = Formula.eval tl s in
  { Materialized.timeline = tl; values }

let eval_values tl (F s) = Formula.eval tl s
