(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Per-cell values.

    A `Pointwise.t` is one value per evaluation cell with no interval or point-in-time query
    semantics. Use it for per-period arithmetic such as "balance times year fraction" when the
    result should not pretend to be an intrinsically queryable flow.

    Convert flows or balances into pointwise values with {!of_flow} and {!of_balance}. Convert back
    to a flow with {!to_flow_approx} when you need to roll the result forward or combine it with
    other flows. The resulting flow is always approximate for date-range accrual. *)

type 'c t
(** The type for pointwise values tagged with currency or unit ['c]. *)

val of_flow : ?name:string -> 'c Flow.t -> 'c t
(** [of_flow flow] reads [flow]'s materialized cell values as pointwise data. *)

val of_balance : ?name:string -> 'c Balance.t -> 'c t
(** [of_balance balance] reads [balance]'s materialized cell values as pointwise data. *)

val named : string -> 'c t -> 'c t
(** [named name p] attaches [name] for diagnostics and dependency graphs. *)

val add : 'c t -> 'c t -> 'c t
(** [add a b] is the pointwise sum of [a] and [b]. *)

val sub : 'c t -> 'c t -> 'c t
(** [sub a b] is the pointwise difference [a - b]. *)

val mul : 'c t -> 'c t -> 'c t
(** [mul a b] is the pointwise product of [a] and [b]. *)

val div : 'c t -> 'c t -> 'c t
(** [div a b] is the pointwise quotient [a /. b]. *)

val scale : float -> 'c t -> 'c t
(** [scale k p] multiplies every cell of [p] by [k]. *)

val neg : 'c t -> 'c t
(** [neg p] negates every cell of [p]. *)

val abs : 'c t -> 'c t
(** [abs p] is the pointwise absolute value of [p]. *)

val min : 'c t -> 'c t -> 'c t
(** [min a b] is the pointwise minimum of [a] and [b]. *)

val max : 'c t -> 'c t -> 'c t
(** [max a b] is the pointwise maximum of [a] and [b]. *)

val clamp : lo:float -> hi:float -> 'c t -> 'c t
(** [clamp ~lo ~hi p] clamps each cell to [[lo, hi]]. *)

val round : int -> 'c t -> 'c t
(** [round digits p] rounds each cell to [digits] decimal places. *)

val map : ?name:string -> (float -> float) -> 'c t -> 'c t
(** [map f p] applies [f] to each cell. *)

val map2 : ?name:string -> (float -> float -> float) -> 'c t -> 'c t -> 'c t
(** [map2 f a b] applies [f] to the cells of [a] and [b]. *)

val where : cond:'a t -> then_:'c t -> else_:'c t -> 'c t
(** [where ~cond ~then_ ~else_] selects [then_] when [cond.(i) <> 0.0] and [else_] otherwise. *)

val to_flow_approx : ?name:string -> 'c t -> 'c Flow.t
(** [to_flow_approx p] converts [p] to a flow that preserves the same per-cell values.

    The resulting flow is always approximate for date-range accrual. *)

module Materialized : sig
  type 'c t
  (** Materialized pointwise values paired with their evaluation timeline. *)

  val timeline : 'c t -> Timeline.t
  (** [timeline m] is the timeline [m] was evaluated against. *)

  val to_array : 'c t -> float array
  (** [to_array m] copies the materialized values. *)

  val unsafe_values : 'c t -> float array
  (** [unsafe_values m] exposes the backing array. The caller must not mutate it. *)

  val length : 'c t -> int
  (** [length m] is the number of periods. *)

  val get : 'c t -> int -> float
  (** [get m i] is the value at period [i]. *)

  val period : 'c t -> int -> Period.t
  (** [period m i] is the period at index [i]. *)
end

val eval : Timeline.t -> 'c t -> 'c Materialized.t
(** [eval tl p] materializes [p] against [tl]. *)
