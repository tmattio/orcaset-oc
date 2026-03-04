(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Balance: point-in-time quantities *)

type 'c t = B of 'c Formula.t [@@unboxed]

(* Constructors *)

let const ?name v = B (Formula.const ?name v)
let of_array ?name arr = B (Formula.of_array ?name arr)
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
let at_period_end _ b = b

(* Escape hatches *)

let formula (B s) = s
let of_formula s = B s

(* Evaluation *)

let eval tl (B s) = Formula.eval tl s
let eval_materialized tl (B s) = Formula.eval_materialized tl s
