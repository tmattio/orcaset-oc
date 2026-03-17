(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Interval quantities.

    A `Flow.t` is a **flow**: a quantity that accrues over a period. Revenue, rent, interest
    expense, and principal amortization are all flows.

    You materialize a flow against a {!Timeline.t} with {!eval}. You then call
    {!Materialized.accrue} to ask "how much accrued over this date range?".

    Exact accrual is derived from the flow AST itself. Exact algebra (`add`, `sub`, `scale`, `neg`,
    `sum`) preserves exact accrual. Cell-local transforms (`map`, `map2`, `mul`, `div`, and related
    operators) and {!Balance.to_flow_approx} fall back to approximate prorating. *)

type 'c t
(** The type for flows tagged with currency or unit ['c]. *)

exception Cycle_error of { formula_name : string option; period_index : int }
(** Raised when evaluation detects a same-period dependency cycle. *)

exception
  Convergence_error of { formula_name : string option; period_index : int; iterations : int }
(** Raised when {!fixpoint} exhausts [max_iter] without converging. *)

(** {1 Constructors} *)

val const : ?name:string -> float -> 'c t
(** [const v] is a flow that produces [v] in every period. *)

val init : ?name:string -> (Period.t -> float) -> 'c t
(** [init f] is a flow that produces [f period] for each period. *)

val init_indexed : ?name:string -> (int -> Period.t -> float) -> 'c t
(** [init_indexed f] is like {!init} but also passes the zero-based period index to [f].

    Prefer {!init} unless you genuinely need the index. *)

val of_array : ?name:string -> float array -> 'c t
(** [of_array arr] is a flow that produces [arr.(i)] at period [i]. Periods beyond the end of the
    array produce [0.0].

    {b Warning.} The array is captured by reference and must not be mutated after the call. *)

val of_events : ?name:string -> (Date.t * float) list -> 'c t
(** [of_events events] bins sparse dated events into the evaluation timeline. Multiple events in the
    same period are summed.

    Materialized accrual stays exact because the original event dates are preserved in the AST.

    Raises [Invalid_argument] if any event date falls outside the timeline. *)

val of_periods : ?name:string -> ?prorater:Prorater.t -> (Period.t * float) list -> 'c t
(** [of_periods pairs] distributes period-keyed source values into the evaluation timeline by
    overlap. Source periods may be wider or narrower than the target timeline.

    Materialized accrual stays exact because the original source periods are preserved in the AST.
*)

val growth_simple :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_simple ~start_date ~rate initial] is simple (linear) growth. *)

val growth_compound :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_compound ~start_date ~rate initial] is compound growth. *)

val year_frac : ?name:string -> (Date.t -> Date.t -> float) -> 'c t
(** [year_frac daycount] produces the year fraction of each evaluation period. *)

(** {1 Naming} *)

val named : string -> 'c t -> 'c t
(** [named name f] attaches [name] to [f] for diagnostics and dependency graphs. *)

(** {1 Exact algebra}

    These operations preserve exact accrual when their inputs are exact. *)

val add : 'c t -> 'c t -> 'c t
(** [add a b] is the pointwise sum of [a] and [b]. Exact range-query capability composes through
    {!add}. *)

val sub : 'c t -> 'c t -> 'c t
(** [sub a b] is the pointwise difference [a - b]. Exact range-query capability composes through
    {!sub}. *)

val scale : float -> 'c t -> 'c t
(** [scale k f] multiplies every period of [f] by [k]. Exact range-query capability composes through
    {!scale}. *)

val neg : 'c t -> 'c t
(** [neg f] negates every period of [f]. Exact range-query capability composes through {!neg}. *)

val sum : ?name:string -> 'c t list -> 'c t
(** [sum fs] is the pointwise sum of all flows in [fs]. The empty list produces [0.0]. *)

(** {1 Cell-local combinators} *)

val map : ?name:string -> (float -> float) -> 'c t -> 'c t
(** [map f flow] applies [f] to each period's value.

    This is cell-local: exact accrual usually degrades to approximate prorating. *)

val map2 : ?name:string -> (float -> float -> float) -> 'c t -> 'c t -> 'c t
(** [map2 f a b] applies [f] to the values of [a] and [b] at each period. *)

val mul : 'c t -> 'c t -> 'c t
(** [mul a b] is the pointwise product of [a] and [b]. *)

val div : 'c t -> 'c t -> 'c t
(** [div a b] is the pointwise quotient [a /. b]. *)

val abs : 'c t -> 'c t
(** [abs f] is the pointwise absolute value of [f]. *)

val min : 'c t -> 'c t -> 'c t
(** [min a b] is the pointwise minimum of [a] and [b]. *)

val max : 'c t -> 'c t -> 'c t
(** [max a b] is the pointwise maximum of [a] and [b]. *)

val clamp : lo:float -> hi:float -> 'c t -> 'c t
(** [clamp ~lo ~hi f] clamps each period's value to [[lo, hi]]. *)

val round : int -> 'c t -> 'c t
(** [round digits f] rounds each value to [digits] decimal places. *)

val where : cond:'a t -> then_:'c t -> else_:'c t -> 'c t
(** [where ~cond ~then_ ~else_] selects [then_] when [cond.(i) <> 0.0] and [else_] otherwise. *)

(** {1 Cross-period} *)

val prev : ?name:string -> 'c t -> default:float -> 'c t
(** [prev f ~default] produces [default] at period 0 and [f.(i-1)] at later periods. *)

val scan : ?name:string -> init:float -> (acc:float -> x:float -> float) -> 'c t -> 'c t
(** [scan ~init f flow] is a running accumulation of [flow] where [acc] is the previous output of
    the scan itself. *)

val window :
  ?name:string -> ?prorater:Prorater.t -> start:Date_ref.t -> end_:Date_ref.t -> 'c t -> 'c t
(** [window ~start ~end_ flow] is a flow where each period's value is [flow] accrued over
    [\[Date_ref.resolve period start, Date_ref.resolve period end_)].

    The date references are resolved against each evaluation period, enabling cross-period windowing
    such as trailing sums. Returns [0.0] when the resolved range is empty or inverted. If the range
    starts before the evaluation timeline, Orcaset clips it to {!Timeline.start_date}. If the range
    ends after {!Timeline.end_date}, Orcaset raises [Invalid_argument]. Query mode is always
    {!Materialized.Approx}. *)

(** {1 Feedback and fixpoint} *)

val feedback : ?name:string -> default:float -> ('c t -> 'c t * 'a) -> 'a
(** [feedback ~default f] ties a self-referential knot by passing [f] a flow representing the prior
    period's value.

    Use this when a flow depends on its own previous-period output. *)

val fixpoint : ?name:string -> ?tol:float -> ?max_iter:int -> guess:float -> ('c t -> 'c t) -> 'c t
(** [fixpoint ~guess f] solves a same-period fixed point in each period. *)

(** {1 Conversion} *)

val convert : rate:float -> 'c1 t -> 'c2 t
(** [convert ~rate f] scales [f] by [rate] and changes the phantom currency tag. *)

(** {1 Evaluation} *)

module Materialized : sig
  type query_mode =
    | Exact
    | Approx
        (** Whether {!accrue} is semantically exact or approximate.

            [Exact] is reserved for provenance-preserving flows such as {!of_events}, {!of_periods},
            and their exact linear compositions. [Approx] means Orcaset answers using timeline-cell
            semantics. *)

  type 'c t
  (** Materialized flow values paired with their evaluation timeline. *)

  val make : Timeline.t -> float array -> 'c t
  (** [make tl values] binds [values] to [tl] with approximate query mode.

      This is a low-level constructor. {!eval} is usually what you want. *)

  val query_mode : 'c t -> query_mode
  (** [query_mode m] reports whether date-range accrual is safe to trust as exact. *)

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

  val to_list : 'c t -> (Period.t * float) list
  (** [to_list m] is the [(period, value)] pairs. *)

  val iter : (Period.t -> float -> unit) -> 'c t -> unit
  (** [iter f m] iterates over [(period, value)] pairs. *)

  val fold : ('a -> Period.t -> float -> 'a) -> 'a -> 'c t -> 'a
  (** [fold f init m] folds over [(period, value)] pairs. *)

  val accrue : ?prorater:Prorater.t -> 'c t -> start_date:Date.t -> end_date:Date.t -> float
  (** [accrue m ~start_date ~end_date] sums the flow over [[start_date, end_date)].

      When [query_mode m = Exact], Orcaset accrues from exact AST semantics (events,
      source-period overlap, and exact linear compositions). Otherwise it falls back to prorating
      materialized period values with [prorater].

      Range semantics are half-open: [start_date] is included and [end_date] is excluded. If the
      range starts before the evaluation timeline, Orcaset clips it to {!Timeline.start_date}. If
      the range ends after {!Timeline.end_date}, Orcaset raises [Invalid_argument]. *)
end

val eval : Timeline.t -> 'c t -> 'c Materialized.t
(** [eval tl f] materializes [f] against [tl]. *)

val eval_many : Timeline.t -> 'c t list -> 'c Materialized.t list
(** [eval_many tl fs] materializes [fs] sharing one memoization context. *)

val eval_values : Timeline.t -> 'c t -> float array
(** [eval_values tl f] is like {!eval} but returns the raw array only. *)

(** {1 Escape hatches} *)

val unsafe_to_formula : 'c t -> (Formula.flow_kind, 'c) Formula.t
(** Exposes the underlying typed AST node. Internal escape hatch for {!Balance} and {!Statement}. *)

val unsafe_of_formula : (Formula.flow_kind, 'c) Formula.t -> 'c t
(** Wraps a raw flow formula without changing semantics. *)

val unsafe_of_array : ?name:string -> float array -> 'c t
(** Alias of {!of_array}; named [unsafe] to signal that you are taking responsibility for the
    array's lifetime. *)

(** {1 Dependency graph} *)

module Deps : sig
  type node = Formula.Deps.node = { id : int; name : string option; kind : string }
  (** A node in the computation DAG. *)

  type edge = Formula.Deps.edge = { src : int; dst : int }
  (** A directed edge from a dependency to a consumer. *)

  val graph : ?named_only:bool -> 'c t list -> node list * edge list
  (** [graph roots] returns all nodes and edges reachable from [roots]. When [named_only] is [true],
      unnamed intermediate nodes are collapsed. *)

  val pp_dot : ?named_only:bool -> Format.formatter -> 'c t list -> unit
  (** [pp_dot ppf roots] formats a Graphviz DOT representation. *)
end

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
