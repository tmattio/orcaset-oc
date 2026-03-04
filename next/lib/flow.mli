(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Interval quantities (flows).

    A {!type-t} represents a quantity that accrues over intervals:
    revenue, expenses, cash movements, principal amortization.
    The natural query is "how much over a date range?", answered by
    {!Materialized.accrue}.

    {2 Algebra}

    The {!section:algebra} ({!add}, {!sub}, {!scale}, {!neg},
    {!sum}) composes correctly through partial-period accrual.

    The {!section:pointwise} ({!map}, {!map2}, {!mul}) applies
    per-period and does {b not} commute with {!Materialized.accrue}
    in general. Use for percentage calculations, conditional logic,
    and cross-type operations (e.g. balance {e ×} year fraction).

    {1:constructors Constructors} *)

type 'c t
(** The type for flows tagged with currency or unit ['c]. *)

val const : ?name:string -> float -> 'c t
(** [const v] is a flow that produces [v] at every period. *)

val init : ?name:string -> (Period.t -> float) -> 'c t
(** [init f] is a flow that produces [f period] at each period. *)

val of_events : ?name:string -> (Date.t * float) list -> 'c t
(** [of_events events] distributes sparse [(date, value)] pairs
    into periods. Each event is placed in the period found by
    {!Timeline.find_index} (start-inclusive, end-exclusive; last
    period end-inclusive). Multiple events in the same period are
    summed. Periods with no events produce [0.0].

    Raises [Invalid_argument] if any event date falls outside the
    timeline. *)

val of_periods : ?name:string -> (Period.t * float) list -> 'c t
(** [of_periods pairs] distributes period-keyed values by exact
    period match. Multiple values for the same period are summed.
    Unmatched periods produce [0.0]. *)

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

(** {1:pointwise Pointwise combinators}

    These operations apply per-period and do {b not} compose
    through partial-period accrual: in general,
    [f(accrue(flow))] {e !=} [accrue(map(f, flow))]. Use them
    for percentage calculations, conditional logic, and
    cross-type operations where period-level semantics suffice. *)

val map : ?name:string -> (float -> float) -> 'c t -> 'c t
(** [map f flow] applies [f] to each period's value. *)

val map2 :
  ?name:string ->
  (float -> float -> float) ->
  'c t ->
  'c t ->
  'c t
(** [map2 f a b] applies [f] to the values of [a] and [b] at
    each period. *)

val mul : 'c t -> 'c t -> 'c t
(** [mul a b] is the pointwise product of [a] and [b]. *)

(** {1:cross_period Cross-period} *)

val prev : ?name:string -> 'c t -> default:float -> 'c t
(** [prev f ~default] produces [default] at period 0 and
    [f.(i-1)] at period [i > 0]. *)

val scan :
  ?name:string ->
  init:float ->
  (acc:float -> x:float -> float) ->
  'c t ->
  'c t
(** [scan ~init f flow] produces a running accumulation:
    - Period 0: [f ~acc:init ~x:flow.(0)]
    - Period i: [f ~acc:result.(i-1) ~x:flow.(i)] *)

(** {1:convert Currency conversion} *)

val convert : rate:float -> 'c1 t -> 'c2 t
(** [convert ~rate f] scales [f] by [rate] and changes the
    currency tag. *)

(** {1:unsafe Unsafe escape hatches}

    These operations drop to the untyped {!Formula.t} layer.
    Prefer the safe API above for new code. *)

val unsafe_to_formula : 'c t -> 'c Formula.t
(** [unsafe_to_formula f] is the underlying {!Formula.t}. *)

val unsafe_of_formula : 'c Formula.t -> 'c t
(** [unsafe_of_formula s] wraps [s] as a flow. The caller asserts
    that [s] has flow semantics (interval quantities). *)

val unsafe_of_array : ?name:string -> float array -> 'c t
(** [unsafe_of_array arr] is a flow that produces [arr.(i)] at
    period [i]. Periods beyond [Array.length arr] produce [0.0].
    The array is captured by reference and must not be mutated
    after the call.

    Prefer {!init} or {!of_events} for new code. *)

(** {1:eval Evaluation} *)

(** {2:materialized Materialized results}

    Evaluation results that keep period bindings attached,
    preventing accidental misalignment between values and their
    periods. *)

module Materialized : sig
  type 'c t
  (** The type for materialized flow results. Each value is bound
      to its period. *)

  val make : Timeline.t -> float array -> 'c t
  (** [make tl values] binds [values] to the periods of [tl].

      Raises [Invalid_argument] if
      [Array.length values <> Timeline.length tl]. *)

  val timeline : _ t -> Timeline.t
  (** [timeline m] is the timeline [m] was evaluated against. *)

  val to_array : _ t -> float array
  (** [to_array m] is a fresh copy of the values array. *)

  val unsafe_values : _ t -> float array
  (** [unsafe_values m] is the backing values array. The caller
      must not mutate it. *)

  val length : _ t -> int
  (** [length m] is the number of values. *)

  val get : _ t -> int -> float
  (** [get m i] is the value at period [i]. *)

  val period : _ t -> int -> Period.t
  (** [period m i] is the period at index [i]. *)

  val to_list : _ t -> (Period.t * float) list
  (** [to_list m] is the list of [(period, value)] pairs. *)

  val iter : (Period.t -> float -> unit) -> _ t -> unit
  (** [iter f m] applies [f period value] to each element. *)

  val fold : ('a -> Period.t -> float -> 'a) -> 'a -> _ t -> 'a
  (** [fold f init m] folds [f] over each [(period, value)]
      pair. *)

  val accrue :
    ?split_fn:Formula.Query.split_fn ->
    _ t ->
    start_date:Date.t ->
    end_date:Date.t ->
    float
  (** [accrue m ~start_date ~end_date] sums the flow over the date
      range. Periods fully contained in the range contribute their
      whole value. Boundary periods are split by [split_fn]
      (default: pro-rata by day count). Returns [0.0] if the date
      range does not overlap the timeline. *)
end

val eval : Timeline.t -> 'c t -> 'c Materialized.t
(** [eval tl f] materializes [f] against [tl], returning a
    {!Materialized.t} with period bindings attached. *)

val eval_values : Timeline.t -> 'c t -> float array
(** [eval_values tl f] materializes [f] against [tl] as a raw
    [float array]. Expert use; prefer {!eval}. *)
