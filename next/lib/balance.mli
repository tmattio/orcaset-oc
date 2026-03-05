(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** {b Internal} -- Point-in-time quantities.

    Wraps {!Formula.t} with provenance tracking for date-based
    interpolation via {!Materialized.at}. Public API is constrained
    by [orcaset2.mli]. *)

type 'c t
(** The type for balances. Pairs a {!Formula.t} with provenance
    metadata for {!Materialized.at} interpolation. *)

(** {1 Constructors} *)

val const : ?name:string -> float -> 'c t
(** [const v] is a balance that produces [v] at every period. *)

val of_array : ?name:string -> float array -> 'c t
(** [of_array arr] is a balance that produces [arr.(i)] at period
    [i]. Periods beyond [Array.length arr] produce [0.0].

    {b Warning.} The array is captured by reference and must not
    be mutated after the call. *)

val of_dates :
  ?name:string ->
  ?before_first:float ->
  (Date.t * float) list ->
  'c t
(** [of_dates observations] distributes date-keyed observations
    into periods using last-observation-wins semantics: each
    period takes the value of the most recent observation on or
    before the period's end date. Periods before any observation
    produce [before_first] (default [0.0]).

    Raises [Invalid_argument] if any observation date falls
    outside the timeline. *)

val of_observations :
  ?name:string ->
  ?before_first:float ->
  (Date.t * float) list ->
  'c t
(** [of_observations] is {!of_dates}. *)

val init : ?name:string -> (Period.t -> float) -> 'c t
(** [init f] is a balance that produces [f period] at each
    period. Low-level; prefer {!const} or {!of_array}. *)

(** {1 Bridge from Flow} *)

val roll_forward :
  ?name:string -> init:float -> 'c Flow.t -> 'c t
(** [roll_forward ~init flow] is a balance where:
    {ul
    {- Period 0: [init + flow.(0)]}
    {- Period i: [result.(i-1) + flow.(i)]}}

    Stores the companion flow for provenance-aware
    {!Materialized.at} interpolation. *)

val roll_forward_with :
  ?name:string ->
  init:float ->
  (acc:float -> x:float -> float) ->
  'c Flow.t ->
  'c t
(** [roll_forward_with ~init f flow] is like {!roll_forward}
    with a custom accumulation function [f] instead of
    addition. *)

(** {1 Naming} *)

val named : string -> 'c t -> 'c t
(** [named name b] attaches [name] to [b] for diagnostics. *)

(** {1 Algebra} *)

val add : 'c t -> 'c t -> 'c t
(** [add a b] is the pointwise sum of [a] and [b]. *)

val sub : 'c t -> 'c t -> 'c t
(** [sub a b] is the pointwise difference [a - b]. *)

val scale : float -> 'c t -> 'c t
(** [scale k b] multiplies every value of [b] by [k]. *)

val neg : 'c t -> 'c t
(** [neg b] negates every value of [b]. *)

val map : ?name:string -> (float -> float) -> 'c t -> 'c t
(** [map f b] applies [f] to each value of [b]. *)

val map2 :
  ?name:string ->
  (float -> float -> float) ->
  'c t ->
  'c t ->
  'c t
(** [map2 f a b] applies [f] to values of [a] and [b] at each
    period. *)

val mul : 'c t -> 'c t -> 'c t
(** [mul a b] is the pointwise product of [a] and [b]. *)

val div : 'c t -> 'c t -> 'c t
(** [div a b] is the pointwise quotient [a /. b]. *)

val abs : 'c t -> 'c t
(** [abs b] is the pointwise absolute value of [b]. *)

val min : 'c t -> 'c t -> 'c t
(** [min a b] is the pointwise minimum of [a] and [b]. *)

val max : 'c t -> 'c t -> 'c t
(** [max a b] is the pointwise maximum of [a] and [b]. *)

val clamp : lo:float -> hi:float -> 'c t -> 'c t
(** [clamp ~lo ~hi b] clamps each value to
    \[[lo]; [hi]\]. *)

val round : int -> 'c t -> 'c t
(** [round digits b] rounds each value to [digits] decimal
    places. *)

val where :
  cond:'a Flow.t -> then_:'c t -> else_:'c t -> 'c t
(** [where ~cond ~then_ ~else_] selects [then_] when
    [cond.(i) <> 0.0] and [else_] otherwise. *)

(** {1 Cross-period} *)

val at_period_start :
  ?name:string -> 'c t -> default:float -> 'c t
(** [at_period_start b ~default] produces [default] at period 0
    and [b.(i-1)] at period [i > 0]. *)

val at_period_end : 'c t -> 'c t
(** [at_period_end b] is [b] (identity). A balance naturally
    represents the end-of-period value. *)

val prev : ?name:string -> 'c t -> default:float -> 'c t
(** [prev b ~default] shifts by one period. Implementation
    primitive for {!at_period_start}; delegates to
    {!Flow.prev}. *)

(** {1 Bridge to Flow} *)

val sample : ?name:string -> 'c t -> 'c Flow.t
(** [sample b] reads the end-of-period balance at each period
    as a flow. The resulting flow has
    {!Flow.Materialized.Q_cell_based} provenance. *)

val change : 'c t -> default:float -> 'c Flow.t
(** [change b ~default] is the per-period delta of [b]: at
    period 0 the change is [b.(0) - default], at period
    [i > 0] the change is [b.(i) - b.(i-1)]. *)

(** {1 Feedback and fixpoint} *)

val feedback :
  ?name:string ->
  default:float ->
  ('c t -> 'c t * 'a) ->
  'a
(** [feedback ~default f] ties a self-referential knot. Calls
    [f] with a balance representing the {e previous period's}
    value ([default] at period 0). [f] returns
    [(definition, result)] where [definition] is the balance
    fed back.

    Raises {!Flow.Cycle_error} if the definition contains a
    same-period cycle. *)

val fixpoint :
  ?name:string ->
  ?tol:float ->
  ?max_iter:int ->
  guess:float ->
  ('c t -> 'c t) ->
  'c t
(** [fixpoint ~guess f] iterates within each period to find
    convergence.

    [tol] defaults to [1e-10]. [max_iter] defaults to [100].

    Raises {!Flow.Convergence_error} if [max_iter] iterations
    are exhausted. *)

(** {1 Currency conversion} *)

val convert : rate:float -> 'c1 t -> 'c2 t
(** [convert ~rate b] scales [b] by [rate] and changes the
    currency tag. *)

(** {1 Evaluation} *)

module Materialized : sig
  type 'c t
  (** The type for materialized balance results. Each value is
      bound to its period, with provenance for {!at}. *)

  val make : Timeline.t -> float array -> 'c t
  (** [make tl values] binds [values] to [tl] without
      provenance. {!at} falls back to end-of-period. *)

  val timeline : _ t -> Timeline.t
  (** [timeline m] is the timeline [m] was evaluated against. *)

  val to_array : _ t -> float array
  (** [to_array m] is a fresh copy of the values. *)

  val unsafe_values : _ t -> float array
  (** [unsafe_values m] is the backing array. The caller must
      not mutate it. *)

  val length : _ t -> int
  (** [length m] is the number of periods. *)

  val get : _ t -> int -> float
  (** [get m i] is the value at period [i]. *)

  val period : _ t -> int -> Period.t
  (** [period m i] is the period at index [i]. *)

  val to_list : _ t -> (Period.t * float) list
  (** [to_list m] is the [(period, value)] pairs. *)

  val iter : (Period.t -> float -> unit) -> _ t -> unit
  (** [iter f m] applies [f] to each [(period, value)]. *)

  val fold :
    ('a -> Period.t -> float -> 'a) -> 'a -> _ t -> 'a
  (** [fold f init m] folds over each [(period, value)]. *)

  val at :
    ?split_fn:Flow.split_fn -> _ t -> Date.t -> float
  (** [at m date] is the interpolated balance at [date].

      When provenance is available (from {!roll_forward},
      {!of_dates}, or composed via {!add}/{!sub}/etc.),
      computes the exact value by accruing the companion flow
      up to [date].

      When provenance is unavailable (cell-local combinators
      or {!make}), returns the end-of-period value for the
      enclosing period.

      [split_fn] defaults to {!Flow.default_split_fn}.

      Raises [Invalid_argument] if [date] is outside the
      timeline. *)
end

module Deps : sig
  type node = Flow.Deps.node
  (** A node in the computation DAG. *)

  type edge = Flow.Deps.edge
  (** A directed edge from dependency to consumer. *)

  val graph :
    ?named_only:bool -> _ t list -> node list * edge list
  (** [graph roots] returns all reachable nodes and edges.
      When [named_only] is [true] (default [false]), unnamed
      intermediate nodes are collapsed. *)

  val pp_dot :
    ?named_only:bool ->
    Format.formatter ->
    _ t list ->
    unit
  (** [pp_dot ppf roots] formats a Graphviz DOT
      representation. *)
end

val eval : Timeline.t -> 'c t -> 'c Materialized.t
(** [eval tl b] materializes [b] against [tl]. When [b] was
    created with {!roll_forward}, the companion flow is
    co-evaluated for provenance-aware {!Materialized.at}.

    Raises {!Flow.Cycle_error} or
    {!Flow.Convergence_error}. *)

val eval_values : Timeline.t -> 'c t -> float array
(** [eval_values tl b] is like {!eval} but returns the raw
    array without provenance. *)

(** {1 Escape hatches}

    Used by {!Statement} for cross-module interop. *)

val unsafe_to_formula : 'c t -> 'c Formula.t
(** [unsafe_to_formula b] is the underlying formula.
    Provenance is lost. *)

val unsafe_of_formula : 'c Formula.t -> 'c t
(** [unsafe_of_formula f] wraps [f] without provenance. *)

val unsafe_of_array : ?name:string -> float array -> 'c t
(** [unsafe_of_array arr] wraps a raw array without provenance.
    The array is captured by reference and must not be
    mutated. *)
