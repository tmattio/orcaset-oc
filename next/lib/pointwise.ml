(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type 'c t = (Formula.pointwise_kind, 'c) Formula.t

let of_flow ?name flow = Formula.pointwise_of_flow ?name (Flow.unsafe_to_formula flow)

let of_balance ?name balance =
  Formula.pointwise_of_balance ?name (Balance.unsafe_to_formula balance)

let named = Formula.named
let add = Formula.add
let sub = Formula.sub
let mul = Formula.mul
let div = Formula.div
let scale = Formula.scale
let neg = Formula.neg
let abs = Formula.abs
let min = Formula.min
let max = Formula.max
let clamp = Formula.clamp
let round = Formula.round
let map = Formula.map
let map2 = Formula.map2
let where ~cond ~then_ ~else_ = Formula.where ~cond ~then_ ~else_

let to_flow_approx ?name pointwise =
  Flow.unsafe_of_formula (Formula.flow_of_pointwise ?name pointwise)

module Materialized = struct
  type 'c t = (Formula.pointwise_kind, 'c) Formula.Materialized.t

  let timeline = Formula.Materialized.timeline
  let to_array = Formula.Materialized.to_array
  let unsafe_values = Formula.Materialized.unsafe_values
  let length = Formula.Materialized.length
  let get = Formula.Materialized.get
  let period = Formula.Materialized.period
end

let eval tl pointwise = Formula.eval_materialized tl pointwise
