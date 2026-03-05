(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** {b Internal} -- Declarative computation DAG over timelines.

    The core computation engine. {!Flow} and {!Balance} wrap this
    module with provenance tracking for partial-period accrual and
    date-based interpolation. Not exposed to library users (private
    module via dune).

    {b Thread safety.} Formula values are pure DAG descriptions,
    but evaluation mutates internal memoization caches. Do not
    evaluate the same formula concurrently from multiple
    domains. *)

type +'c t
(** The type for formulas. A node in a DAG that is evaluated
    against a {!Timeline.t} to produce one [float] per period.
    Covariant in ['c] (the currency/unit phantom tag). *)

(** {1 Constructors} *)

val const : ?name:string -> float -> 'c t
(** [const v] is a formula that produces [v] at every period. *)

val of_array : ?name:string -> float array -> 'c t
(** [of_array arr] is a formula that produces [arr.(i)] at period
    [i]. Periods beyond [Array.length arr] produce [0.0].

    {b Warning.} The array is captured by reference and must not
    be mutated after the call. *)

val init_flow : ?name:string -> (Period.t -> float) -> 'c t
(** [init_flow f] is a formula that produces [f period] at each
    period. Wrapped by {!Flow.init}. *)

val init :
  ?name:string -> (int -> Period.t -> float) -> 'c t
(** [init f] is a formula that produces [f i period] at period
    [i]. Wrapped by {!Flow.init_indexed}. *)

val init_tl :
  ?name:string ->
  (Timeline.t -> int -> Period.t -> float) ->
  'c t
(** [init_tl f] is like {!init} but [f] also receives the
    {!Timeline.t}. Use when the computation needs timeline-level
    information (e.g. total number of periods). *)

val growth_simple :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_simple ~start_date ~rate initial] is simple (linear)
    growth. [daycount] defaults to {!Daycount.actual_360}. *)

val growth_compound :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_compound ~start_date ~rate initial] is compound
    growth. [daycount] defaults to {!Daycount.actual_360}. *)

val year_frac :
  ?name:string -> (Date.t -> Date.t -> float) -> 'c t
(** [year_frac daycount] produces [daycount start end] for each
    period. *)

val of_events :
  ?name:string -> (Date.t * float) list -> 'c t
(** [of_events events] bins [(date, value)] pairs into periods.
    Multiple events in the same period are summed. Periods with
    no events produce [0.0].

    Raises [Invalid_argument] if any date falls outside the
    timeline. *)

(** {1 Naming} *)

val named : string -> 'c t -> 'c t
(** [named name s] attaches [name] for {!Cycle_error},
    {!Convergence_error} diagnostics and {!Deps.pp_dot} labels.
    Shares memoization state with [s]. *)

(** {1 Pointwise combinators} *)

val map : ?name:string -> (float -> float) -> 'c t -> 'c t
(** [map f s] applies [f] to each period's value. *)

val map2 :
  ?name:string ->
  (float -> float -> float) ->
  'c t ->
  'c t ->
  'c t
(** [map2 f a b] applies [f] to the values of [a] and [b] at
    each period. *)

val add : 'c t -> 'c t -> 'c t
(** [add a b] is the pointwise sum of [a] and [b]. *)

val sub : 'c t -> 'c t -> 'c t
(** [sub a b] is the pointwise difference [a - b]. *)

val mul : 'c t -> 'c t -> 'c t
(** [mul a b] is the pointwise product of [a] and [b]. *)

val scale : float -> 'c t -> 'c t
(** [scale k s] multiplies every value of [s] by [k]. *)

val neg : 'c t -> 'c t
(** [neg s] negates every value of [s]. *)

val div : 'c t -> 'c t -> 'c t
(** [div a b] is the pointwise quotient [a /. b]. Division by
    zero produces [infinity] or [nan] per IEEE 754. *)

val abs : 'c t -> 'c t
(** [abs s] is the pointwise absolute value of [s]. *)

val min : 'c t -> 'c t -> 'c t
(** [min a b] is the pointwise minimum of [a] and [b]. *)

val max : 'c t -> 'c t -> 'c t
(** [max a b] is the pointwise maximum of [a] and [b]. *)

val clamp : lo:float -> hi:float -> 'c t -> 'c t
(** [clamp ~lo ~hi s] clamps each value to
    \[[lo]; [hi]\]. *)

val round : int -> 'c t -> 'c t
(** [round digits s] rounds each value to [digits] decimal
    places. *)

val sum : ?name:string -> 'c t list -> 'c t
(** [sum ss] is the pointwise sum. The empty list produces
    [0.0]. *)

val where : cond:'a t -> then_:'c t -> else_:'c t -> 'c t
(** [where ~cond ~then_ ~else_] selects [then_] when
    [cond.(i) <> 0.0] and [else_] otherwise. Only the selected
    branch is evaluated. *)

(** {1 Cross-period operators}

    The only way to express temporal relationships. Same-period
    cycles are detected at evaluation time ({!Cycle_error}).
    Cross-period cycles are structurally impossible: {!prev}
    shifts by one period and {!scan} only reads its own earlier
    output. *)

val prev : ?name:string -> 'c t -> default:float -> 'c t
(** [prev s ~default] produces [default] at period 0 and
    [s.(i-1)] at period [i > 0]. The primitive for cross-period
    dependencies. *)

val scan :
  ?name:string ->
  init:float ->
  (acc:float -> x:float -> float) ->
  'c t ->
  'c t
(** [scan ~init f flow] is a running accumulation:
    {ul
    {- Period 0: [f ~acc:init ~x:flow.(0)]}
    {- Period i: [f ~acc:result.(i-1) ~x:flow.(i)]}}

    [acc] is the output of [scan] itself at the prior period,
    not the input [flow]. *)

val cumsum : ?name:string -> init:float -> 'c t -> 'c t
(** [cumsum ~init flow] is
    [scan ~init (fun ~acc ~x -> acc +. x) flow]. *)

val feedback :
  ?name:string ->
  default:float ->
  ('c t -> 'c t * 'a) ->
  'a
(** [feedback ~default f] ties a self-referential knot via
    {!prev}. Calls [f] with a formula representing the
    {e previous period's} value ([default] at period 0). [f]
    returns [(definition, result)] where [definition] is the
    formula fed back.

    Raises {!Cycle_error} if [definition] contains a same-period
    cycle. *)

val fixpoint :
  ?name:string ->
  ?tol:float ->
  ?max_iter:int ->
  guess:float ->
  ('c t -> 'c t) ->
  'c t
(** [fixpoint ~guess f] iterates within each period to find [x]
    such that [f (const x)] converges to [x]. At period 0
    iteration starts from [guess]; subsequent periods warm-start
    from the prior converged value.

    [tol] defaults to [1e-10]. [max_iter] defaults to [100].

    Raises {!Convergence_error} if [max_iter] iterations are
    exhausted. *)

(** {1 Currency conversion} *)

val convert : rate:float -> 'c1 t -> 'c2 t
(** [convert ~rate s] scales [s] by [rate] and changes the
    currency tag. *)

(** {1 Evaluation} *)

exception Cycle_error of
  { formula_name : string option; period_index : int }
(** Raised when evaluation detects a same-period dependency
    cycle. [formula_name] is present when the formula was
    given a name with {!named} or a [?name] parameter. *)

exception Convergence_error of
  { formula_name : string option;
    period_index : int;
    iterations : int }
(** Raised when {!fixpoint} does not converge. [iterations] is
    the number attempted. *)

val eval : Timeline.t -> 'c t -> float array
(** [eval tl s] materializes [s] against [tl], returning one
    [float] per period. Each (formula, period) cell is computed
    at most once.

    Raises {!Cycle_error} or {!Convergence_error}. *)

val eval_many : Timeline.t -> 'c t list -> float array list
(** [eval_many tl ss] materializes each formula, sharing a
    single memoization context to avoid redundant computation
    of shared subexpressions.

    Raises {!Cycle_error} or {!Convergence_error}. *)

module Materialized : sig
  type 'c t
  (** The type for materialized formula results. Each value is
      bound to its period. *)

  val make : Timeline.t -> float array -> 'c t
  (** [make tl values] binds [values] to the periods of [tl].

      Raises [Invalid_argument] if
      [Array.length values <> Timeline.length tl]. *)

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
end

val eval_materialized : Timeline.t -> 'c t -> 'c Materialized.t
(** [eval_materialized tl s] is like {!eval} but returns a
    {!Materialized.t} with the timeline bound to the values.

    Raises {!Cycle_error} or {!Convergence_error}. *)

(** {1 Date queries}

    Low-level timeline probing used by {!Flow} and {!Balance}
    for provenance-aware accrual and interpolation. *)

module Query : sig
  type split_fn =
    start_date:Date.t ->
    end_date:Date.t ->
    split_date:Date.t ->
    value:float ->
    float * float
  (** The type for functions that split a period's value at a
      date. Returns [(before, after)] where
      [before +. after = value]. *)

  val default_split_fn : split_fn
  (** [default_split_fn] distributes proportionally by day
      count. Returns [(0., 0.)] for zero-day periods. *)

  val interpolate :
    ?split_fn:split_fn ->
    Timeline.t ->
    float array ->
    Date.t ->
    float
  (** [interpolate tl values date] is the portion of the
      enclosing period's value that falls before [date].
      [split_fn] defaults to {!default_split_fn}.

      Raises [Invalid_argument] if [date] is outside the
      timeline. *)

  val accrue :
    ?split_fn:split_fn ->
    Timeline.t ->
    float array ->
    start_date:Date.t ->
    end_date:Date.t ->
    float
  (** [accrue tl values ~start_date ~end_date] sums values over
      the date range. Boundary periods are split: first period
      contributes its {e after} portion, last period its
      {e before} portion. Returns [0.0] if the range does not
      overlap the timeline. *)

  val balance_at :
    ?split_fn:split_fn ->
    Timeline.t ->
    balance:float array ->
    flow:float array ->
    Date.t ->
    float
  (** [balance_at tl ~balance ~flow date] is the interpolated
      balance at [date]: the prior period's ending balance plus
      the portion of the current period's flow that falls before
      [date]. Used by {!Balance.Materialized.at}.

      Raises [Invalid_argument] if [date] is outside the
      timeline. *)
end

(** {1 Dependency graph} *)

module Deps : sig
  type node =
    { id : int; name : string option; kind : string }
  (** A node in the computation DAG. [kind] is one of:
      ["const"], ["init"], ["map"], ["map2"], ["sum"],
      ["prev"], ["scan"], ["where"], ["fixpoint"],
      ["var"]. *)

  type edge = { src : int; dst : int }
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
