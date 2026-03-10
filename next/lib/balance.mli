(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Point-in-time quantities.

    A `Balance.t` is a **balance**: a quantity defined at a date. Cash, debt outstanding, and
    retained earnings are all balances.

    You materialize a balance against a {!Timeline.t} with {!eval}. You then call {!Materialized.at}
    to interpolate the balance at a specific date.

    Exact interpolation is derived from the balance AST itself. A balance built from {!roll_forward}
    or {!of_dates} can stay exact through pointwise balance operations such as {!add}, {!sub},
    {!map}, and {!map2}. *)

type 'c t
(** The type for balances tagged with currency or unit ['c]. *)

type prorater = Flow.prorater
(** Fractional allocator used when interpolation falls back to prorating the current period's flow.
*)

(** {1 Constructors} *)

val const : ?name:string -> float -> 'c t
(** [const v] is a balance that produces [v] at every period. *)

val init : ?name:string -> (Period.t -> float) -> 'c t
(** [init f] is a balance that produces [f period] for each period. *)

val of_array : ?name:string -> float array -> 'c t
(** [of_array arr] is a balance that produces [arr.(i)] at period [i]. Periods beyond the array
    produce [0.0].

    {b Warning.} The array is captured by reference and must not be mutated after the call. *)

val of_dates : ?name:string -> ?before_first:float -> (Date.t * float) list -> 'c t
(** [of_dates observations] builds a balance from dated observations using last-observation-wins
    semantics: each period takes the latest observation on or before the period end date.

    Materialized point queries stay exact because the original observation dates are preserved in
    the AST.

    Raises [Invalid_argument] if any observation date falls outside the timeline. *)

val of_observations : ?name:string -> ?before_first:float -> (Date.t * float) list -> 'c t
(** Alias of {!of_dates}. *)

(** {1 Bridge from Flow} *)

val roll_forward : ?name:string -> init:float -> 'c Flow.t -> 'c t
(** [roll_forward ~init flow] accumulates [flow] into a running balance starting from [init].

    This is the fundamental flow-to-balance bridge. Materialized interpolation stays exact because
    the balance keeps the roll-forward semantics in the AST itself. *)

val roll_forward_with :
  ?name:string -> init:float -> (acc:float -> x:float -> float) -> 'c Flow.t -> 'c t
(** [roll_forward_with ~init f flow] is like {!roll_forward} but uses a custom accumulation function
    [f]. Exact point-query capability generally downgrades to approximate mode. *)

(** {1 Naming} *)

val named : string -> 'c t -> 'c t
(** [named name b] attaches [name] for diagnostics and dependency graphs. *)

(** {1 Algebra}

    These operations preserve exact interpolation when their inputs are exact. *)

val add : 'c t -> 'c t -> 'c t
(** [add a b] is the pointwise sum of [a] and [b]. Exact point-query capability composes when both
    inputs remain exact. *)

val sub : 'c t -> 'c t -> 'c t
(** [sub a b] is the pointwise difference [a - b]. *)

val scale : float -> 'c t -> 'c t
(** [scale k b] multiplies every period of [b] by [k]. *)

val neg : 'c t -> 'c t
(** [neg b] negates every period of [b]. *)

val map : ?name:string -> (float -> float) -> 'c t -> 'c t
(** [map f b] applies [f] to each period's value. If [b] has exact point semantics, the mapped
    balance stays exact. *)

val map2 : ?name:string -> (float -> float -> float) -> 'c t -> 'c t -> 'c t
(** [map2 f a b] applies [f] to the values of [a] and [b] at each period. *)

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
(** [clamp ~lo ~hi b] clamps each value to [[lo, hi]]. *)

val round : int -> 'c t -> 'c t
(** [round digits b] rounds each value to [digits] decimal places. *)

val where : cond:'a Flow.t -> then_:'c t -> else_:'c t -> 'c t
(** [where ~cond ~then_ ~else_] selects [then_] when [cond.(i) <> 0.0] and [else_] otherwise. *)

(** {1 Cross-period} *)

val prev : ?name:string -> 'c t -> default:float -> 'c t
(** [prev b ~default] produces [default] at period 0 and [b.(i-1)] thereafter. *)

val at_period_start : ?name:string -> 'c t -> default:float -> 'c t
(** [at_period_start b ~default] is {!prev}[ b ~default]. *)

val at_period_end : 'c t -> 'c t
(** [at_period_end b] is [b]. A balance naturally denotes the end-of-period value. *)

(** {1 Bridge to Flow} *)

val sample : ?name:string -> 'c t -> 'c Flow.t
(** [sample b] reads the end-of-period balance value at each period as a flow.

    This is a pointwise bridge: it is suitable for per-period arithmetic such as interest
    calculations, but date-range accrual on the resulting flow is only approximate. *)

val change : 'c t -> default:float -> 'c Flow.t
(** [change b ~default] is the per-period delta of [b]. *)

(** {1 Feedback / fixpoint} *)

val feedback : ?name:string -> default:float -> ('c t -> 'c t * 'a) -> 'a
(** [feedback ~default f] ties a self-referential balance knot by passing [f] the previous period's
    value.

    Use this when a balance depends on its own previous-period output. *)

val fixpoint : ?name:string -> ?tol:float -> ?max_iter:int -> guess:float -> ('c t -> 'c t) -> 'c t
(** [fixpoint ~guess f] solves a same-period fixed point in each period. *)

(** {1 Conversion} *)

val convert : rate:float -> 'c1 t -> 'c2 t
(** [convert ~rate b] scales [b] by [rate] and changes the phantom currency tag. *)

(** {1 Evaluation} *)

module Materialized : sig
  type query_mode =
    | Exact
    | Approx
        (** Whether {!at} uses an AST-derived exact interpolation rule or falls back to the
            enclosing period's materialized value. *)

  type 'c t
  (** Materialized balance values paired with their evaluation timeline. *)

  val make : Timeline.t -> float array -> 'c t
  (** [make tl values] binds [values] to [tl] with approximate query mode.

      This is a low-level constructor. {!eval} is usually what you want. *)

  val query_mode : 'c t -> query_mode
  (** [query_mode m] reports whether point queries are exact or approximate. *)

  val timeline : 'c t -> Timeline.t
  (** [timeline m] is the timeline [m] was evaluated against. *)

  val to_array : 'c t -> float array
  (** [to_array m] copies the materialized values. *)

  val unsafe_values : 'c t -> float array
  (** [unsafe_values m] exposes the backing array. The caller must not mutate it. *)

  val length : 'c t -> int
  (** [length m] is the number of periods. *)

  val get : 'c t -> int -> float
  (** [get m i] is the end-of-period value at index [i]. *)

  val period : 'c t -> int -> Period.t
  (** [period m i] is the period at index [i]. *)

  val to_list : 'c t -> (Period.t * float) list
  (** [to_list m] is the [(period, value)] pairs. *)

  val iter : (Period.t -> float -> unit) -> 'c t -> unit
  (** [iter f m] iterates over [(period, value)] pairs. *)

  val fold : ('a -> Period.t -> float -> 'a) -> 'a -> 'c t -> 'a
  (** [fold f init m] folds over [(period, value)] pairs. *)

  val at : ?prorater:prorater -> 'c t -> Date.t -> float
  (** [at m date] interpolates the balance at [date].

      When [query_mode m = Exact], Orcaset interpolates from intrinsic AST semantics (for example
      observations and roll-forwards). Otherwise it falls back to the enclosing period's
      materialized value. *)
end

val eval : Timeline.t -> 'c t -> 'c Materialized.t
(** [eval tl b] materializes [b] against [tl]. *)

val eval_values : Timeline.t -> 'c t -> float array
(** [eval_values tl b] is like {!eval} but returns the raw array only. *)

(** {1 Escape hatches} *)

val unsafe_to_formula : 'c t -> (Formula.balance_kind, 'c) Formula.t
(** Exposes the underlying typed AST node. Internal escape hatch for {!Statement}. *)

val unsafe_of_formula : (Formula.balance_kind, 'c) Formula.t -> 'c t
(** Wraps a raw balance formula without changing semantics. *)

val unsafe_of_array : ?name:string -> float array -> 'c t
(** Alias of {!of_array}; named [unsafe] to signal that you are taking responsibility for the
    array's lifetime. *)

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
