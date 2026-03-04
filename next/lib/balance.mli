(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Point-in-time quantities (balances).

    A {!type-t} represents a snapshot value at a date: cash balance,
    debt outstanding, inventory level. The natural query is "what is
    the value at date [d]?", not "how much over an interval?".

    Unlike {!Flow.t}, balances support {!map} and {!map2} since
    pointwise transforms on snapshots preserve the "value at a date"
    semantics. The fundamental bridge from flow to balance is
    {!roll_forward}: a running sum of a flow starting from an
    initial value.

    {1:constructors Constructors} *)

type 'c t
(** The type for balances tagged with currency or unit ['c]. *)

val const : ?name:string -> float -> 'c t
(** [const v] is a balance that produces [v] at every period. *)

val of_array : ?name:string -> float array -> 'c t
(** [of_array arr] is a balance that produces [arr.(i)] at
    period [i]. Periods beyond [Array.length arr] produce [0.0]. *)

val init : ?name:string -> (Period.t -> float) -> 'c t
(** [init f] is a balance that produces [f period] at each period. *)

(** {1:bridge Bridge from Flow} *)

val roll_forward : ?name:string -> init:float -> 'c Flow.t -> 'c t
(** [roll_forward ~init flow] is a balance where each period's
    value is the running sum of [flow] starting from [init]:
    - Period 0: [init + flow.(0)]
    - Period i: [result.(i-1) + flow.(i)]

    This is the fundamental flow-to-balance bridge. *)

val roll_forward_with :
  ?name:string ->
  init:float ->
  (acc:float -> x:float -> float) ->
  'c Flow.t ->
  'c t
(** [roll_forward_with ~init f flow] is like {!roll_forward} but
    uses [f ~acc ~x] instead of addition. *)

(** {1:naming Naming} *)

val named : string -> 'c t -> 'c t
(** [named name b] attaches [name] to [b] for diagnostics. *)

(** {1:algebra Algebra} *)

val add : 'c t -> 'c t -> 'c t
(** [add a b] is the pointwise sum of [a] and [b]. *)

val sub : 'c t -> 'c t -> 'c t
(** [sub a b] is the pointwise difference [a - b]. *)

val scale : float -> 'c t -> 'c t
(** [scale k b] multiplies every value of [b] by [k]. *)

val map : ?name:string -> (float -> float) -> 'c t -> 'c t
(** [map f b] applies [f] to each value of [b]. *)

val map2 :
  ?name:string -> (float -> float -> float) -> 'c t -> 'c t -> 'c t
(** [map2 f a b] applies [f] to values of [a] and [b] at each
    period. *)

(** {1:cross_period Cross-period} *)

val prev : ?name:string -> 'c t -> default:float -> 'c t
(** [prev b ~default] produces [default] at period 0 and
    [b.(i-1)] at period [i > 0]. *)

val at_period_start : ?name:string -> 'c t -> default:float -> 'c t
(** [at_period_start b ~default] is {!prev}[ b ~default]. The
    balance at the start of a period is the end-of-previous-period
    value. *)

val at_period_end : 'a -> 'c t -> 'c t
(** [at_period_end _name b] is [b] (identity). A balance naturally
    represents the end-of-period value. The first argument is
    ignored; it exists for symmetry with {!at_period_start}. *)

(** {1:escape Escape hatches} *)

val formula : 'c t -> 'c Formula.t
(** [formula b] is the underlying {!Formula.t}. *)

val of_formula : 'c Formula.t -> 'c t
(** [of_formula s] wraps [s] as a balance. The caller asserts
    that [s] has balance semantics (point-in-time quantities). *)

(** {1:eval Evaluation} *)

val eval : Timeline.t -> 'c t -> float array
(** [eval tl b] materializes [b] against [tl]. *)

val eval_materialized :
  Timeline.t -> 'c t -> 'c Formula.Materialized.t
(** [eval_materialized tl b] is like {!eval} but returns a
    {!Formula.Materialized.t} with period bindings attached. *)
