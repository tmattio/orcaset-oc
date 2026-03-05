(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** {b Internal} -- Interval quantities.

    Wraps {!Formula.t} with a {!Materialized.query_ctx} provenance
    hint so that {!Materialized.accrue} can use source boundaries
    instead of pro-rata splitting. Public API is constrained by
    [orcaset2.mli]. *)

type 'c t
(** The type for flows. Pairs a {!Formula.t} with a
    {!Materialized.query_ctx} describing data provenance. *)

type split_fn =
  start_date:Date.t ->
  end_date:Date.t ->
  split_date:Date.t ->
  value:float ->
  float * float
(** The type for functions that split a period's value at
    [split_date]. Returns [(before, after)] where
    [before +. after = value]. *)

val default_split_fn : split_fn
(** [default_split_fn] distributes proportionally by day count.
    Returns [(0., 0.)] for zero-day periods. *)

exception Cycle_error of
  { formula_name : string option; period_index : int }
(** Raised when evaluation detects a same-period dependency
    cycle. *)

exception Convergence_error of
  { formula_name : string option;
    period_index : int;
    iterations : int }
(** Raised when {!fixpoint} does not converge within
    [max_iter] iterations. *)

(** {1 Constructors}

    Provenance-aware constructors ({!of_events}, {!of_periods})
    record their source data so {!Materialized.accrue} can use
    exact boundaries. All others produce
    {!Materialized.Q_cell_based} provenance. *)

val const : ?name:string -> float -> 'c t
(** [const v] is a flow that produces [v] at every period. *)

val init : ?name:string -> (Period.t -> float) -> 'c t
(** [init f] is a flow that produces [f period] at each
    period. *)

val init_indexed :
  ?name:string -> (int -> Period.t -> float) -> 'c t
(** [init_indexed f] is a flow that produces [f i period] at
    period [i]. Prefer {!init} unless the zero-based index is
    genuinely needed. *)

val of_array : ?name:string -> float array -> 'c t
(** [of_array arr] is a flow that produces [arr.(i)] at period
    [i]. Periods beyond [Array.length arr] produce [0.0].

    {b Warning.} The array is captured by reference and must not
    be mutated after the call. *)

val of_events :
  ?name:string -> (Date.t * float) list -> 'c t
(** [of_events events] bins [(date, value)] pairs into periods.
    Multiple events in the same period are summed. Periods with
    no events produce [0.0]. Records {!Materialized.Q_events}
    provenance.

    Raises [Invalid_argument] if any date falls outside the
    timeline. *)

val of_periods :
  ?name:string ->
  ?split_fn:split_fn ->
  (Period.t * float) list ->
  'c t
(** [of_periods pairs] distributes period-keyed values into the
    evaluation timeline by overlap proportion. Records
    {!Materialized.Q_source_periods} provenance.

    [split_fn] defaults to {!default_split_fn}. *)

val growth_simple :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_simple ~start_date ~rate initial] is a flow with
    simple (linear) growth. [daycount] defaults to
    {!Daycount.actual_360}. *)

val growth_compound :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_compound ~start_date ~rate initial] is a flow with
    compound growth. [daycount] defaults to
    {!Daycount.actual_360}. *)

val year_frac :
  ?name:string -> (Date.t -> Date.t -> float) -> 'c t
(** [year_frac daycount] is a flow that produces the year
    fraction of each period. *)

(** {1 Naming} *)

val named : string -> 'c t -> 'c t
(** [named name f] attaches [name] to [f] for diagnostics and
    {!Deps.pp_dot}. *)

(** {1 Algebra (provenance-preserving)}

    These compose provenance through {!Materialized.Q_sum},
    {!Materialized.Q_scale}, and {!Materialized.Q_neg}, so
    {!Materialized.accrue} can still use source boundaries. *)

val add : 'c t -> 'c t -> 'c t
(** [add a b] is the pointwise sum of [a] and [b]. *)

val sub : 'c t -> 'c t -> 'c t
(** [sub a b] is the pointwise difference [a - b]. *)

val scale : float -> 'c t -> 'c t
(** [scale k f] multiplies every value of [f] by [k]. *)

val neg : 'c t -> 'c t
(** [neg f] negates every value of [f]. *)

val sum : ?name:string -> 'c t list -> 'c t
(** [sum fs] is the pointwise sum of all flows in [fs]. The
    empty list produces [0.0] at every period. *)

(** {1 Cell-local (no provenance)}

    These produce {!Materialized.Q_cell_based} provenance
    because pointwise transforms do not compose with date-range
    accrual in general. *)

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

val div : 'c t -> 'c t -> 'c t
(** [div a b] is the pointwise quotient [a /. b]. Division by
    zero produces [infinity] or [nan] per IEEE 754. *)

val abs : 'c t -> 'c t
(** [abs f] is the pointwise absolute value of [f]. *)

val min : 'c t -> 'c t -> 'c t
(** [min a b] is the pointwise minimum of [a] and [b]. *)

val max : 'c t -> 'c t -> 'c t
(** [max a b] is the pointwise maximum of [a] and [b]. *)

val clamp : lo:float -> hi:float -> 'c t -> 'c t
(** [clamp ~lo ~hi f] clamps each value to
    \[[lo]; [hi]\]. *)

val round : int -> 'c t -> 'c t
(** [round digits f] rounds each value to [digits] decimal
    places. *)

val where : cond:'a t -> then_:'c t -> else_:'c t -> 'c t
(** [where ~cond ~then_ ~else_] selects [then_] when
    [cond.(i) <> 0.0] and [else_] otherwise. *)

(** {1 Cross-period and feedback} *)

val prev : ?name:string -> 'c t -> default:float -> 'c t
(** [prev f ~default] produces [default] at period 0 and
    [f.(i-1)] at period [i > 0]. Used by
    {!Balance.at_period_start}. *)

val scan :
  ?name:string ->
  init:float ->
  (acc:float -> x:float -> float) ->
  'c t ->
  'c t
(** [scan ~init f flow] is a running accumulation where [acc]
    is the output of [scan] at the prior period (not the input
    flow). Used by {!Balance.roll_forward}.
    {ul
    {- Period 0: [f ~acc:init ~x:flow.(0)]}
    {- Period i: [f ~acc:result.(i-1) ~x:flow.(i)]}} *)

val feedback :
  ?name:string ->
  default:float ->
  ('c t -> 'c t * 'a) ->
  'a
(** [feedback ~default f] ties a self-referential knot via
    {!prev}. Breaks same-period cycles by shifting one period.

    Raises {!Cycle_error} if the definition contains a
    same-period cycle. *)

val fixpoint :
  ?name:string ->
  ?tol:float ->
  ?max_iter:int ->
  guess:float ->
  ('c t -> 'c t) ->
  'c t
(** [fixpoint ~guess f] iterates within each period to find [x]
    such that [f (const x)] converges to [x].

    [tol] defaults to [1e-10]. [max_iter] defaults to [100].

    Raises {!Convergence_error} if [max_iter] iterations are
    exhausted. *)

val convert : rate:float -> 'c1 t -> 'c2 t
(** [convert ~rate f] scales [f] by [rate] and changes the
    currency tag. *)

(** {1 Evaluation} *)

module Materialized : sig
  type query_ctx =
    | Q_events of (Date.t * float) list
        (** Original event dates. *)
    | Q_source_periods of {
        pairs : (Period.t * float) list;
        split_fn : Formula.Query.split_fn;
      }
        (** Original source periods and their split function. *)
    | Q_sum of query_ctx list
        (** Sum of sub-provenances. *)
    | Q_scale of float * query_ctx
        (** Scaled sub-provenance. *)
    | Q_neg of query_ctx
        (** Negated sub-provenance. *)
    | Q_cell_based
        (** No provenance; accrual falls back to [split_fn]. *)
  (** Provenance ADT for exact partial-period accrual. Tracks
      how the flow was constructed so {!accrue} can use source
      boundaries instead of pro-rata splitting. Used by
      {!Balance.Materialized.at} via {!accrue_via_ctx}. *)

  type 'c t
  (** The type for materialized flow results. Each value is
      bound to its period, with provenance attached. *)

  val make : Timeline.t -> float array -> 'c t
  (** [make tl values] binds [values] to [tl] with
      {!Q_cell_based} provenance. *)

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

  val accrue :
    ?split_fn:split_fn ->
    _ t ->
    start_date:Date.t ->
    end_date:Date.t ->
    float
  (** [accrue m ~start_date ~end_date] sums the flow over the
      date range. Uses {!query_ctx} provenance when available,
      falls back to [split_fn] (default {!default_split_fn})
      on {!Q_cell_based}. Returns [0.0] if the date range does
      not overlap the timeline. *)

  val accrue_via_ctx :
    query_ctx ->
    start_date:Date.t ->
    end_date:Date.t ->
    float
  (** [accrue_via_ctx ctx ~start_date ~end_date] is low-level
      accrual on a bare provenance context.

      Raises [Exit] on {!Q_cell_based} so the caller can fall
      back to pro-rata. Used by {!Balance.Materialized.at}. *)

  val query : _ t -> query_ctx
  (** [query m] is [m]'s provenance context. *)
end

val eval : Timeline.t -> 'c t -> 'c Materialized.t
(** [eval tl f] materializes [f] against [tl].

    Raises {!Cycle_error} or {!Convergence_error}. *)

val eval_many :
  Timeline.t -> 'c t list -> 'c Materialized.t list
(** [eval_many tl fs] materializes each flow in [fs], sharing
    a single memoization context.

    Raises {!Cycle_error} or {!Convergence_error}. *)

val eval_values : Timeline.t -> 'c t -> float array
(** [eval_values tl f] is like {!eval} but returns the raw
    array without provenance. Used by {!Statement}. *)

(** {1 Escape hatches}

    Used by {!Balance} and {!Statement} for cross-module
    interop. These bypass the provenance layer. *)

val unsafe_to_formula : 'c t -> 'c Formula.t
(** [unsafe_to_formula f] is the underlying formula.
    Provenance is lost. *)

val unsafe_of_formula : 'c Formula.t -> 'c t
(** [unsafe_of_formula f] wraps [f] with {!Materialized.Q_cell_based}
    provenance. *)

val unsafe_of_array : ?name:string -> float array -> 'c t
(** [unsafe_of_array arr] is like {!of_array} but named
    [unsafe] to signal the {!Materialized.Q_cell_based}
    provenance. The array is captured by reference and must not
    be mutated. *)

(** {1 Dependency graph} *)

module Deps : sig
  type node = Formula.Deps.node =
    { id : int; name : string option; kind : string }
  (** A node in the computation DAG. *)

  type edge = Formula.Deps.edge = { src : int; dst : int }
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

(** {1 Infix syntax} *)

module Syntax : sig
  val ( + ) : 'c t -> 'c t -> 'c t
  val ( - ) : 'c t -> 'c t -> 'c t
  val ( * ) : 'c t -> 'c t -> 'c t
  val ( / ) : 'c t -> 'c t -> 'c t
  val ( *$ ) : float -> 'c t -> 'c t
end

