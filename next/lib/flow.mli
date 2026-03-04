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

    The {!section:cell_local} ({!map}, {!map2}, {!mul}) applies
    per-period and does {b not} commute with {!Materialized.accrue}
    in general. Use for percentage calculations, conditional logic,
    and cross-type operations (e.g. balance {e ×} year fraction).

    {1:types Types} *)

type 'c t
(** The type for flows tagged with currency or unit ['c]. *)

type split_fn =
  start_date:Date.t ->
  end_date:Date.t ->
  split_date:Date.t ->
  value:float ->
  float * float
(** The type for functions that split a period's value at a date.
    Given the period's [start_date], [end_date], and a [split_date]
    within the period, returns [(before, after)] where
    [before +. after = value]. *)

val default_split_fn : split_fn
(** [default_split_fn] distributes the value proportionally by day
    count. *)

(** {1:exceptions Exceptions} *)

exception Cycle_error of { formula_name : string option; period_index : int }
(** Raised when evaluation detects a same-period dependency cycle. *)

exception
  Convergence_error of {
    formula_name : string option;
    period_index : int;
    iterations : int;
  }
(** Raised when {!fixpoint} does not converge. *)

(** {1:constructors Constructors} *)

val const : ?name:string -> float -> 'c t
(** [const v] is a flow that produces [v] at every period. *)

val init : ?name:string -> (Period.t -> float) -> 'c t
(** [init f] is a flow that produces [f period] at each period. *)

val init_indexed : ?name:string -> (int -> Period.t -> float) -> 'c t
(** [init_indexed f] is a flow that produces [f i period] at period
    [i]. Prefer {!init} unless the index is genuinely needed (e.g.
    indexing into an external array). *)

val of_events : ?name:string -> (Date.t * float) list -> 'c t
(** [of_events events] distributes sparse [(date, value)] pairs
    into periods. Each event is placed in the period found by
    {!Timeline.find_index} (start-inclusive, end-exclusive; last
    period end-inclusive). Multiple events in the same period are
    summed. Periods with no events produce [0.0].

    Raises [Invalid_argument] if any event date falls outside the
    timeline. *)

val of_periods :
  ?name:string ->
  ?split_fn:split_fn ->
  (Period.t * float) list ->
  'c t
(** [of_periods pairs] distributes period-keyed values into the
    evaluation timeline using overlap-based splitting. Each source
    period's value is allocated to evaluation periods proportionally
    to the overlap, using [split_fn] (default: pro-rata by day
    count).

    Source periods that partially overlap an evaluation period
    contribute only the overlapping portion. Multiple source periods
    overlapping the same evaluation period are summed. Evaluation
    periods with no overlap produce [0.0]. *)

val growth_simple :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_simple ~start_date ~rate initial] is a flow with
    simple (linear) growth. *)

val growth_compound :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_compound ~start_date ~rate initial] is a flow with
    compound growth. *)

val year_frac : ?name:string -> (Date.t -> Date.t -> float) -> 'c t
(** [year_frac daycount] is a flow that produces the year fraction
    of each period. *)

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

(** {1:cell_local Cell-local combinators}

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
(** [clamp ~lo ~hi f] clamps each value to [[lo, hi]]. *)

val round : int -> 'c t -> 'c t
(** [round digits f] rounds each value to [digits] decimal
    places. *)

val where : cond:'a t -> then_:'c t -> else_:'c t -> 'c t
(** [where ~cond ~then_ ~else_] selects [then_] when
    [cond.(i) <> 0.0] and [else_] otherwise. Only the selected
    branch is evaluated at each period. *)

(** {1:feedback Feedback} *)

val feedback :
  ?name:string ->
  default:float ->
  ('c t -> 'c t * 'a) ->
  'a
(** [feedback ~default f] ties a self-referential knot for flows.
    It calls [f] with a flow representing the {e previous period's}
    value ([default] at period 0). [f] returns
    [(definition, result)] where [definition] is the flow fed back
    and [result] is returned to the caller.

    @raise Cycle_error if [definition] contains a same-period
    cycle. *)

val fixpoint :
  ?name:string ->
  ?tol:float ->
  ?max_iter:int ->
  guess:float ->
  ('c t -> 'c t) ->
  'c t
(** [fixpoint ~guess f] finds the value [x] at each period such
    that [f (const x)] converges to [x] (within [tol]).

    [tol] defaults to [1e-10]. [max_iter] defaults to [100].

    @raise Convergence_error if [max_iter] iterations are
    exhausted. *)

(** {1:convert Currency conversion} *)

val convert : rate:float -> 'c1 t -> 'c2 t
(** [convert ~rate f] scales [f] by [rate] and changes the
    currency tag. *)

(** {1:eval Evaluation} *)

(** Materialized results that keep period bindings attached,
    preventing accidental misalignment between values and their
    periods. *)
module Materialized : sig
  type 'c t
  (** The type for materialized flow results. Each value is bound
      to its period. *)

  val timeline : _ t -> Timeline.t
  (** [timeline m] is the timeline [m] was evaluated against. *)

  val to_array : _ t -> float array
  (** [to_array m] is a fresh copy of the values array. *)

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
    ?split_fn:split_fn ->
    _ t ->
    start_date:Date.t ->
    end_date:Date.t ->
    float
  (** [accrue m ~start_date ~end_date] sums the flow over the date
      range. When source provenance is available (from {!val:of_events}
      or {!val:of_periods}), accrual uses the original source
      boundaries for exact splitting. Provenance composes through
      {!val:add}, {!val:sub}, {!val:scale}, {!val:neg}, and {!val:sum}.

      When provenance is unavailable (cell-local combinators), falls
      back to [split_fn] (default: pro-rata by day count). Returns
      [0.0] if the date range does not overlap the timeline. *)

  (**/**)

  val make : Timeline.t -> float array -> 'c t
  val unsafe_values : _ t -> float array
end

val eval : Timeline.t -> 'c t -> 'c Materialized.t
(** [eval tl f] materializes [f] against [tl], returning a
    {!Materialized.t} with period bindings attached. *)

val eval_many : Timeline.t -> 'c t list -> 'c Materialized.t list
(** [eval_many tl fs] materializes each flow in [fs] against [tl],
    sharing a single memoization context. Use this when evaluating
    multiple flows that share subexpressions to avoid redundant
    computation. *)

(** {1:deps Dependency graph} *)

module Deps : sig
  type node = { id : int; name : string option; kind : string }
  (** A node in the dependency graph. *)

  type edge = { src : int; dst : int }
  (** A directed edge from a dependency to a consumer. *)

  val graph : ?named_only:bool -> _ t list -> node list * edge list
  (** [graph roots] returns all nodes and edges reachable from
      [roots]. When [named_only] is [true], unnamed intermediate
      nodes are collapsed. Default is [false]. *)

  val pp_dot :
    ?named_only:bool -> Format.formatter -> _ t list -> unit
  (** [pp_dot ppf roots] formats a Graphviz DOT representation. *)
end

(** {1:syntax Infix syntax}

    Open this module to use arithmetic operators on flows.

    {b Warning.} This shadows the [Stdlib] integer operators
    [( + )], [( - )], [( * )] and [( / )]. *)

module Syntax : sig
  val ( + ) : 'c t -> 'c t -> 'c t
  (** [a + b] is {!add}[ a b]. *)

  val ( - ) : 'c t -> 'c t -> 'c t
  (** [a - b] is {!sub}[ a b]. *)

  val ( * ) : 'c t -> 'c t -> 'c t
  (** [a * b] is {!mul}[ a b]. *)

  val ( / ) : 'c t -> 'c t -> 'c t
  (** [a / b] is {!div}[ a b]. *)

  val ( *$ ) : float -> 'c t -> 'c t
  (** [k *$ f] is {!scale}[ k f]. *)
end

(**/**)

(** Internal: used by {!Balance} for cross-module provenance. *)

val unsafe_to_formula : 'c t -> 'c Formula.t
val unsafe_of_formula : 'c Formula.t -> 'c t
val unsafe_of_array : ?name:string -> float array -> 'c t
val eval_values : Timeline.t -> 'c t -> float array

val prev : ?name:string -> 'c t -> default:float -> 'c t
val scan :
  ?name:string ->
  init:float ->
  (acc:float -> x:float -> float) ->
  'c t ->
  'c t

module Materialized_internal : sig
  type query_ctx =
    | Q_events of (Date.t * float) list
    | Q_source_periods of {
        pairs : (Period.t * float) list;
        split_fn : Formula.Query.split_fn;
      }
    | Q_sum of query_ctx list
    | Q_scale of float * query_ctx
    | Q_neg of query_ctx
    | Q_cell_based

  val accrue_via_ctx :
    query_ctx -> start_date:Date.t -> end_date:Date.t -> float

  val query : _ Materialized.t -> query_ctx
  val unsafe_values : _ Materialized.t -> float array
end
