(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Flow: interval quantities *)

type 'c t = F of 'c Formula.t [@@unboxed]

(* Constructors *)

let const ?name v = F (Formula.const ?name v)

let of_array ?name arr = F (Formula.of_array ?name arr)

let init ?name f = F (Formula.init_flow ?name f)

let of_events ?name events = F (Formula.of_events ?name events)

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

(* Evaluation *)

let eval tl (F s) = Formula.eval tl s
let eval_materialized tl (F s) = Formula.eval_materialized tl s
