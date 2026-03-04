(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Interval quantities (flows).

    A {!type-t} represents a quantity that accrues over intervals:
    revenue, expenses, cash movements, principal amortization.
    The natural query is "how much over [{!Period.t}]?", not "what
    is the value at a date?".

    The algebra is restricted to operations that compose correctly
    through partial-period accrual: {!add}, {!sub}, {!scale},
    {!neg}, {!sum}. For pointwise operations ({!Formula.map},
    {!Formula.mul}, etc.) drop to {!formula} and work at the
    {!Formula} level.

    {1:constructors Constructors} *)

type 'c t
(** The type for flows tagged with currency or unit ['c]. *)

val const : ?name:string -> float -> 'c t
(** [const v] is a flow that produces [v] at every period. *)

val of_array : ?name:string -> float array -> 'c t
(** [of_array arr] is a flow that produces [arr.(i)] at period [i].
    Periods beyond [Array.length arr] produce [0.0]. The array is
    captured by reference and must not be mutated after the call. *)

val init : ?name:string -> (Period.t -> float) -> 'c t
(** [init f] is a flow that produces [f period] at each period. *)

val of_events : ?name:string -> (Date.t * float) list -> 'c t
(** [of_events events] distributes sparse [(date, value)] pairs
    into periods. See {!Formula.of_events}. *)

val growth_simple :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_simple ~start_date ~rate initial] is a flow with
    simple (linear) growth. See {!Formula.growth_simple}. *)

val growth_compound :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_compound ~start_date ~rate initial] is a flow with
    compound growth. See {!Formula.growth_compound}. *)

val year_frac : ?name:string -> (Date.t -> Date.t -> float) -> 'c t
(** [year_frac daycount] is a flow that produces the year fraction
    of each period. See {!Formula.year_frac}. *)

(** {1:naming Naming} *)

val named : string -> 'c t -> 'c t
(** [named name f] attaches [name] to [f] for diagnostics. *)

(** {1:algebra Exact algebra}

    These operations compose correctly through partial-period
    accrual. *)

val add : 'c t -> 'c t -> 'c t
(** [add a b] is the pointwise sum of [a] and [b]. *)

val sub : 'c t -> 'c t -> 'c t
(** [sub a b] is the pointwise difference [a - b]. *)

val scale : float -> 'c t -> 'c t
(** [scale k f] multiplies every value of [f] by [k]. *)

val neg : 'c t -> 'c t
(** [neg f] negates every value of [f]. *)

val sum : ?name:string -> 'c t list -> 'c t
(** [sum fs] is the pointwise sum of all flows in [fs]. *)

(** {1:escape Escape hatches} *)

val formula : 'c t -> 'c Formula.t
(** [formula f] is the underlying {!Formula.t}. Use this to drop
    to the grid-local formula layer for operations like
    {!Formula.map}, {!Formula.mul}, or {!Formula.prev}. *)

val of_formula : 'c Formula.t -> 'c t
(** [of_formula s] wraps [s] as a flow. The caller asserts that
    [s] has flow semantics (interval quantities). *)

(** {1:eval Evaluation} *)

val eval : Timeline.t -> 'c t -> float array
(** [eval tl f] materializes [f] against [tl]. *)

val eval_materialized : Timeline.t -> 'c t -> 'c Formula.Materialized.t
(** [eval_materialized tl f] is like {!eval} but returns a
    {!Formula.Materialized.t} with period bindings attached. *)
