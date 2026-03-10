(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** {b Internal} -- Shared typed AST for flows and balances.

    This version removes the separate provenance shadow trees used by the old design. The AST itself
    is the single source of truth for both evaluation and exact-query capability detection.

    The public {!Flow} and {!Balance} modules are thin wrappers around the two semantic kinds
    exposed here. This module remains private via dune. *)

type flow_kind
(** Semantic kind for interval quantities. *)

type balance_kind
(** Semantic kind for point-in-time quantities. *)

type pointwise_kind
(** Semantic kind for per-cell values without interval or point-in-time query semantics. *)

type _ kind =
  | Flow_k : flow_kind kind
  | Balance_k : balance_kind kind
  | Pointwise_k : pointwise_kind kind

type prorater = Prorater.t
(** A prorater allocates a full-period value to a sub-range of that period.

    It returns the fraction of the value that belongs to [[sub_start, sub_end)].
    Callers multiply the full-period value by the returned fraction.

    The intended law is additivity over partitions of the full period. *)

val default_prorater : prorater
(** Default prorater. Uses {!Prorater.actual_days}. *)

type ('k, 'c) t = private { id : int; name : string option; kind : 'k kind; node : ('k, 'c) node }
and any_series = Any_series : ('k, 'c) t -> any_series
and ('k, 'c) delay = private { mutable resolved : ('k, 'c) t option; thunk : unit -> ('k, 'c) t }

and ('k, 'c) node =
  | Const of float
  | Of_array of float array
  | Init of (Timeline.t -> int -> Period.t -> float)
  | Growth_simple of {
      start_date : Date.t;
      daycount : Date.t -> Date.t -> float;
      rate : float;
      initial : float;
    }
  | Growth_compound of {
      start_date : Date.t;
      daycount : Date.t -> Date.t -> float;
      rate : float;
      initial : float;
    }
  | Year_frac of (Date.t -> Date.t -> float)
  | Events of { events : (Date.t * float) list; cache : (Timeline.t * float array) option ref }
  | Source_periods of {
      pairs : (Period.t * float) list;
      prorater : prorater;
      cache : (Timeline.t * float array) option ref;
    }
  | Observations of {
      sorted_obs : (Date.t * float) array;
      before_first : float;
      cache : (Timeline.t * float array) option ref;
    }
  | Add of ('k, 'c) t * ('k, 'c) t
  | Sub of ('k, 'c) t * ('k, 'c) t
  | Scale of float * ('k, 'c) t
  | Neg of ('k, 'c) t
  | Map of (float -> float) * ('k, 'c) t
  | Map2 of (float -> float -> float) * ('k, 'c) t * ('k, 'c) t
  | Sum of ('k, 'c) t list
  | Where of { cond : any_series; then_ : ('k, 'c) t; else_ : ('k, 'c) t }
  | Prev of { src : ('k, 'c) t; default : float }
  | Scan_flow of { init : float; f : acc:float -> x:float -> float; flow : (flow_kind, 'c) t }
  | Accumulate of { init : float; f : acc:float -> x:float -> float; flow : (flow_kind, 'c) t }
  | Roll_forward of { init : float; flow : (flow_kind, 'c) t }
  | Pointwise_of_flow of (flow_kind, 'c) t
  | Pointwise_of_balance of (balance_kind, 'c) t
  | Flow_of_pointwise of (pointwise_kind, 'c) t
  | Change_balance of { balance : (balance_kind, 'c) t; default : float }
  | Delay of ('k, 'c) delay
  | Var of float ref
  | Fixpoint of { var : ('k, 'c) t; body : ('k, 'c) t; tol : float; max_iter : int; guess : float }

type packed = Pack : ('k, 'c) t -> packed

val kind : ('k, 'c) t -> 'k kind
(** Returns the semantic kind carried by the formula. *)

val named : string -> ('k, 'c) t -> ('k, 'c) t
(** Attaches a name used for diagnostics and dependency graphs. *)

val resolve : ('k, 'c) t -> ('k, 'c) node
(** Forces a delayed node, if present, and returns the resolved AST node. *)

(** {1 Constructors} *)

val const : ?name:string -> 'k kind -> float -> ('k, 'c) t
(** A constant formula. *)

val of_array : ?name:string -> 'k kind -> float array -> ('k, 'c) t
(** A formula backed by a raw array. *)

val init : ?name:string -> 'k kind -> (int -> Period.t -> float) -> ('k, 'c) t
(** Per-period constructor from index and period. *)

val init_tl : ?name:string -> 'k kind -> (Timeline.t -> int -> Period.t -> float) -> ('k, 'c) t
(** Per-period constructor that also receives the full evaluation timeline. *)

val growth_simple :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  (flow_kind, 'c) t
(** A simple-growth flow constructor. *)

val growth_compound :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  (flow_kind, 'c) t
(** A compound-growth flow constructor. *)

val year_frac : ?name:string -> (Date.t -> Date.t -> float) -> (flow_kind, 'c) t
(** A flow that produces the year fraction of each evaluation period. *)

val of_events : ?name:string -> (Date.t * float) list -> (flow_kind, 'c) t
(** A flow built from sparse dated events. *)

val of_periods : ?name:string -> ?prorater:prorater -> (Period.t * float) list -> (flow_kind, 'c) t
(** A flow built from source periods and values. *)

val of_observations :
  ?name:string -> ?before_first:float -> (Date.t * float) list -> (balance_kind, 'c) t
(** A balance built from dated observations with last-observation-wins semantics. *)

(** {1 Pointwise combinators} *)

val map : ?name:string -> (float -> float) -> ('k, 'c) t -> ('k, 'c) t
(** Cell-local unary transform. *)

val map2 : ?name:string -> (float -> float -> float) -> ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
(** Cell-local binary transform. *)

val add : ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
(** Pointwise addition. *)

val sub : ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
(** Pointwise subtraction. *)

val mul : ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
(** Pointwise multiplication. *)

val div : ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
(** Pointwise division. *)

val neg : ('k, 'c) t -> ('k, 'c) t
(** Pointwise negation. *)

val scale : float -> ('k, 'c) t -> ('k, 'c) t
(** Pointwise scalar multiplication. *)

val abs : ('k, 'c) t -> ('k, 'c) t
(** Pointwise absolute value. *)

val min : ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
(** Pointwise minimum. *)

val max : ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
(** Pointwise maximum. *)

val clamp : lo:float -> hi:float -> ('k, 'c) t -> ('k, 'c) t
(** Pointwise clamp to [[lo, hi]]. *)

val round : int -> ('k, 'c) t -> ('k, 'c) t
(** Pointwise rounding to a fixed number of decimal places. *)

val sum : ?name:string -> 'k kind -> ('k, 'c) t list -> ('k, 'c) t
(** Pointwise sum. The empty list produces [0.0]. *)

val where : cond:('cond, 'x) t -> then_:('k, 'c) t -> else_:('k, 'c) t -> ('k, 'c) t
(** Pointwise conditional. *)

(** {1 Cross-period and bridges} *)

val prev : ?name:string -> ('k, 'c) t -> default:float -> ('k, 'c) t
(** Previous-period value with a default for period 0. *)

val scan :
  ?name:string ->
  init:float ->
  (acc:float -> x:float -> float) ->
  (flow_kind, 'c) t ->
  (flow_kind, 'c) t
(** Running accumulation that stays in flow semantics. *)

val accumulate :
  ?name:string ->
  init:float ->
  (acc:float -> x:float -> float) ->
  (flow_kind, 'c) t ->
  (balance_kind, 'c) t
(** Running accumulation that yields balance semantics. *)

val roll_forward : ?name:string -> init:float -> (flow_kind, 'c) t -> (balance_kind, 'c) t
(** Specialized accumulation for balance roll-forwards: prior balance plus current flow. *)

val pointwise_of_flow : ?name:string -> (flow_kind, 'c) t -> (pointwise_kind, 'c) t
(** Converts a flow to per-cell semantics. *)

val pointwise_of_balance : ?name:string -> (balance_kind, 'c) t -> (pointwise_kind, 'c) t
(** Converts a balance to per-cell semantics. *)

val flow_of_pointwise : ?name:string -> (pointwise_kind, 'c) t -> (flow_kind, 'c) t
(** Converts a pointwise series back to an approximate flow. *)

val change_balance : ?name:string -> (balance_kind, 'c) t -> default:float -> (flow_kind, 'c) t
(** Per-period delta of a balance, represented as a flow. *)

val convert : rate:float -> ('k, 'c1) t -> ('k, 'c2) t
(** Currency-tag-changing scalar multiplication. *)

(** {1 Feedback and fixpoint} *)

exception Cycle_error of { formula_name : string option; period_index : int }
(** Raised when evaluation detects a same-period dependency cycle. *)

exception
  Convergence_error of { formula_name : string option; period_index : int; iterations : int }
(** Raised when {!fixpoint} exhausts [max_iter] without converging. *)

val feedback :
  ?name:string -> kind:'k kind -> default:float -> (('k, 'c) t -> ('k, 'c) t * 'a) -> 'a
(** Internal feedback primitive used by {!Flow.feedback} and {!Balance.feedback}. *)

val fixpoint :
  ?name:string ->
  kind:'k kind ->
  ?tol:float ->
  ?max_iter:int ->
  guess:float ->
  (('k, 'c) t -> ('k, 'c) t) ->
  ('k, 'c) t
(** Internal fixpoint primitive used by {!Flow.fixpoint} and {!Balance.fixpoint}. *)

(** {1 Evaluation} *)

val eval : Timeline.t -> ('k, 'c) t -> float array
(** Evaluates a formula to one float per period. *)

val eval_many : Timeline.t -> ('k, 'c) t list -> float array list
(** Evaluates multiple same-kind formulas sharing one memoization context. *)

val eval_many_packed : Timeline.t -> packed list -> float array list
(** Evaluates mixed packed formulas sharing one memoization context. *)

module Materialized : sig
  type ('k, 'c) t
  (** Materialized raw formula values paired with their timeline. *)

  val make : Timeline.t -> float array -> ('k, 'c) t
  (** Binds raw values to a timeline. *)

  val timeline : ('k, 'c) t -> Timeline.t
  (** Returns the bound timeline. *)

  val to_array : ('k, 'c) t -> float array
  (** Copies the materialized values. *)

  val unsafe_values : ('k, 'c) t -> float array
  (** Exposes the backing array. The caller must not mutate it. *)

  val length : ('k, 'c) t -> int
  (** Returns the number of periods. *)

  val get : ('k, 'c) t -> int -> float
  (** Returns the value at period [i]. *)

  val period : ('k, 'c) t -> int -> Period.t
  (** Returns the period at index [i]. *)

  val to_list : ('k, 'c) t -> (Period.t * float) list
  (** Returns [(period, value)] pairs. *)

  val iter : (Period.t -> float -> unit) -> ('k, 'c) t -> unit
  (** Iterates over [(period, value)] pairs. *)

  val fold : ('a -> Period.t -> float -> 'a) -> 'a -> ('k, 'c) t -> 'a
  (** Folds over [(period, value)] pairs. *)
end

val eval_materialized : Timeline.t -> ('k, 'c) t -> ('k, 'c) Materialized.t
(** Evaluates a formula and re-attaches the timeline to the result. *)

(** {1 Query helpers used by Flow and Balance} *)

module Query : sig
  type nonrec prorater = prorater

  val default_prorater : prorater
  (** Default prorater used by {!before_date} and {!accrue}. *)

  val before_date : ?prorater:prorater -> Timeline.t -> float array -> Date.t -> float
  (** Portion of the enclosing period's value that falls before [date]. *)

  val accrue :
    ?prorater:prorater -> Timeline.t -> float array -> start_date:Date.t -> end_date:Date.t -> float
  (** Sum of period values over [[start_date, end_date)]. *)
end

type query_mode =
  | Exact
  | Approx  (** Whether a materialized query is semantically exact or approximate. *)

type flow_query = {
  mode : query_mode;
  accrue : prorater:prorater -> start_date:Date.t -> end_date:Date.t -> float;
}
(** Materialized flow query capability. *)

type balance_query = { mode : query_mode; at : prorater:prorater -> Date.t -> float }
(** Materialized balance query capability. *)

val flow_query : Timeline.t -> float array -> (flow_kind, 'c) t -> flow_query
(** Conservative flow query builder. Exact mode is reserved for provenance-preserving flow ASTs. *)

val balance_query : Timeline.t -> float array -> (balance_kind, 'c) t -> balance_query
(** Conservative balance query builder. Exact mode is reserved for balances with trustworthy
    point-in-time semantics. *)

(** {1 Dependency graph} *)

module Deps : sig
  type node = { id : int; name : string option; kind : string }
  (** A node in the internal computation DAG. *)

  type edge = { src : int; dst : int }
  (** A directed edge from a dependency to a consumer. *)

  val graph : ?named_only:bool -> ('k, 'c) t list -> node list * edge list
  (** Returns the subgraph reachable from [roots]. *)

  val pp_dot : ?named_only:bool -> Format.formatter -> ('k, 'c) t list -> unit
  (** Formats the reachable subgraph as Graphviz DOT. *)
end

module Syntax : sig
  val ( + ) : ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
  (** Infix alias for {!add}. *)

  val ( - ) : ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
  (** Infix alias for {!sub}. *)

  val ( * ) : ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
  (** Infix alias for {!mul}. *)

  val ( / ) : ('k, 'c) t -> ('k, 'c) t -> ('k, 'c) t
  (** Infix alias for {!div}. *)

  val ( *$ ) : float -> ('k, 'c) t -> ('k, 'c) t
  (** Infix alias for {!scale}. *)
end
