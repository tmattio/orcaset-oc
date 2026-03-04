(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Point-in-time quantities (balances).

    A {!type-t} represents a snapshot value at a date: cash balance,
    debt outstanding, inventory level. The natural query is "what is
    the value at date [d]?", answered by {!Materialized.at}.

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

val init : ?name:string -> (Period.t -> float) -> 'c t
(** [init f] is a balance that produces [f period] at each period. *)

val of_dates :
  ?name:string ->
  ?before_first:float ->
  (Date.t * float) list ->
  'c t
(** [of_dates observations] distributes date-keyed observations into
    periods using last-observation-wins semantics: each period takes
    the value of the most recent observation whose date falls on or
    before the period's end date. Periods before any observation
    produce [before_first] (default [0.0]).

    Raises [Invalid_argument] if any observation date falls outside
    the timeline. *)

val of_observations :
  ?name:string ->
  ?before_first:float ->
  (Date.t * float) list ->
  'c t
(** [of_observations] is {!of_dates}. *)

(** {1:bridge Bridge from Flow} *)

val roll_forward : ?name:string -> init:float -> 'c Flow.t -> 'c t
(** [roll_forward ~init flow] is a balance where each period's
    value is the running sum of [flow] starting from [init]:
    - Period 0: [init + flow.(0)]
    - Period i: [result.(i-1) + flow.(i)]

    This is the fundamental flow-to-balance bridge. The companion
    flow is stored intrinsically and used by {!Materialized.at} for
    provenance-aware interpolation. *)

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

val neg : 'c t -> 'c t
(** [neg b] negates every value of [b]. *)

val map : ?name:string -> (float -> float) -> 'c t -> 'c t
(** [map f b] applies [f] to each value of [b]. *)

val map2 :
  ?name:string -> (float -> float -> float) -> 'c t -> 'c t -> 'c t
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
(** [clamp ~lo ~hi b] clamps each value to [[lo, hi]]. *)

val round : int -> 'c t -> 'c t
(** [round digits b] rounds each value to [digits] decimal
    places. *)

val where : cond:'a Flow.t -> then_:'c t -> else_:'c t -> 'c t
(** [where ~cond ~then_ ~else_] selects [then_] when
    [cond.(i) <> 0.0] and [else_] otherwise. *)

(** {1:cross_period Cross-period} *)

val prev : ?name:string -> 'c t -> default:float -> 'c t
(** [prev b ~default] produces [default] at period 0 and
    [b.(i-1)] at period [i > 0]. *)

val at_period_start : ?name:string -> 'c t -> default:float -> 'c t
(** [at_period_start b ~default] is {!prev}[ b ~default]. The
    balance at the start of a period is the end-of-previous-period
    value. *)

val at_period_end : 'c t -> 'c t
(** [at_period_end b] is [b] (identity). A balance naturally
    represents the end-of-period value. *)

(** {1:sample Bridge to Flow} *)

val sample : ?name:string -> 'c t -> 'c Flow.t
(** [sample b] reads the end-of-period balance value at each period
    as a flow series. This is the primary cross-type bridge: it lets
    balance values participate in flow arithmetic without escape
    hatches.

    {b Note.} The resulting flow is a sampled point-in-time value,
    not an interval quantity. Date-range accrual
    ({!Flow.Materialized.accrue}) on the result is not meaningful.
    Use it for per-period calculations (e.g.
    [Flow.mul (Balance.sample bal) year_frac]). *)

val change : 'c t -> default:float -> 'c Flow.t
(** [change b ~default] is the per-period change in [b], as a flow.
    At period 0, the change is [b.(0) - default]. At period [i > 0],
    the change is [b.(i) - b.(i-1)]. *)

(** {1:feedback Feedback} *)

val feedback :
  ?name:string ->
  default:float ->
  ('c t -> 'c t * 'a) ->
  'a
(** [feedback ~default f] ties a self-referential knot for
    balances. It calls [f] with a balance representing the
    {e previous period's} value ([default] at period 0). [f]
    returns [(definition, result)] where [definition] is the
    balance fed back and [result] is returned to the caller.

    This is the standard pattern for loan amortization:

    {[
      let balance, (interest, principal) =
        Balance.feedback ~default:loan_amount (fun prev_bal ->
            let interest =
              Flow.scale (-.rate)
                (Flow.mul (Balance.sample prev_bal) year_fracs)
            in
            let principal = Flow.sub total_pmt interest in
            let bal =
              Balance.roll_forward ~init:loan_amount principal
            in
            (bal, (interest, principal)))
    ]} *)

val fixpoint :
  ?name:string ->
  ?tol:float ->
  ?max_iter:int ->
  guess:float ->
  ('c t -> 'c t) ->
  'c t
(** [fixpoint ~guess f] finds the value [x] at each period such
    that [f (const x)] converges to [x] (within [tol]).

    Use this for same-period circular dependencies where the
    balance depends on itself within the same period (e.g. LTC
    construction loans where interest capitalizes into the
    balance).

    [tol] defaults to [1e-10]. [max_iter] defaults to [100].

    Raises [Formula.Convergence_error] if [max_iter] iterations
    are exhausted. *)

(** {1:convert Currency conversion} *)

val convert : rate:float -> 'c1 t -> 'c2 t
(** [convert ~rate b] scales [b] by [rate] and changes the
    currency tag. *)

(** {1:unsafe Unsafe escape hatches}

    These operations drop to the untyped {!Formula.t} layer.
    Prefer the safe API above for new code. *)

val unsafe_to_formula : 'c t -> 'c Formula.t
(** [unsafe_to_formula b] is the underlying {!Formula.t}. *)

val unsafe_of_formula : 'c Formula.t -> 'c t
(** [unsafe_of_formula s] wraps [s] as a balance. The caller
    asserts that [s] has balance semantics (point-in-time
    quantities). *)

val unsafe_of_array : ?name:string -> float array -> 'c t
(** [unsafe_of_array arr] is a balance that produces [arr.(i)]
    at period [i]. Periods beyond [Array.length arr] produce
    [0.0].

    Prefer {!init} or {!roll_forward} for new code. *)

(** {1:eval Evaluation} *)

(** {2:materialized Materialized results}

    Evaluation results that keep period bindings attached,
    preventing accidental misalignment between values and their
    periods. *)

module Materialized : sig
  type 'c t
  (** The type for materialized balance results. Each value is
      bound to its period. *)

  type interp =
    | Step    (** End-of-previous-period balance. *)
    | Series  (** Prior balance + pro-rated current-period flow. *)
  (** The type for point-in-time interpolation methods. *)

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

  val at :
    ?interp:interp ->
    ?split_fn:Formula.Query.split_fn ->
    _ t ->
    Date.t ->
    float
  (** [at m date] is the interpolated balance at [date].

      When [interp] is [Series] (the default) and the balance was
      created with {!val:roll_forward}, computes the prior period's
      ending balance plus the accrued portion of the companion flow
      up to [date], using the flow's query provenance when available.

      When [interp] is [Step], returns the pro-rated value of the
      enclosing period (ignoring companion flow).

      For balances without a companion flow (e.g. {!val:of_dates},
      {!val:const}), both modes fall back to period-level
      interpolation.

      Raises [Invalid_argument] if [date] is outside the
      timeline. *)
end

(** {1:deps Dependency graph} *)

module Deps : sig
  type node = Flow.Deps.node
  (** A node in the dependency graph. *)

  type edge = Flow.Deps.edge
  (** A directed edge from a dependency to a consumer. *)

  val graph : ?named_only:bool -> _ t list -> node list * edge list
  (** [graph roots] returns all nodes and edges reachable from
      [roots]. *)

  val pp_dot :
    ?named_only:bool -> Format.formatter -> _ t list -> unit
  (** [pp_dot ppf roots] formats a Graphviz DOT representation. *)
end

val eval : Timeline.t -> 'c t -> 'c Materialized.t
(** [eval tl b] materializes [b] against [tl], returning a
    {!Materialized.t} with period bindings attached. When the
    balance was created with {!roll_forward}, the companion flow
    is co-evaluated for provenance-aware {!Materialized.at}. *)

val eval_values : Timeline.t -> 'c t -> float array
(** [eval_values tl b] materializes [b] against [tl] as a raw
    [float array]. Expert use; prefer {!eval}. *)
