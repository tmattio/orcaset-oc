(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Declarative computations over a timeline.

    A {!type-t} value is a recipe for computing one [float] per {!Period.t} in a {!Timeline.t}.
    Recipes are composed declaratively as a directed acyclic graph of combinators and materialized
    into a [float array] by {!eval}.

    {1:model Computation model}

    A series holds no data. It is a node in a DAG that is evaluated against a concrete
    {!Timeline.t}. Evaluation proceeds left to right (period 0, 1, {e ...}, n-1) and every
    {e (series, period)} cell is computed at most once.

    {!prev} and {!scan} are the only combinators that introduce cross-period dependencies; all
    others are pointwise within a single period. Same-period cycles are detected at evaluation time
    and raise {!Cycle_error}. Cross-period cycles are structurally impossible: {!prev} shifts by one
    period and {!scan} only reads its own earlier output.

    {1:currency Currency safety}

    The type parameter ['c] is a phantom tag representing the currency or unit of the series.
    Combinators like {!add} and {!sub} require both operands to share the same tag, preventing
    accidental mixing of currencies at compile time. Constructors return a universally quantified
    ['c t], so single-currency models need no annotations. For multi-currency models, annotate entry
    points to pin the currency and use {!convert} to change it:

    {[
      type usd
      type eur

      let eur_revenue : eur Series.t = Series.growth_simple ~start_date ~rate:0.05 8000.0
      let usd_revenue : usd Series.t = Series.convert ~rate:1.08 eur_revenue
    ]}

    {b Thread safety.} Series values are pure DAG descriptions, but evaluation mutates internal
    caches ({!type-t} values with {!feedback} or {!fixpoint} contain mutable refs). Do not evaluate
    the same series concurrently from multiple domains.

    {1:constructors Constructors} *)

type +'c t
(** The type for series tagged with currency or unit ['c]. *)

val const : ?name:string -> float -> 'c t
(** [const v] is a series that produces [v] at every period. *)

val of_array : ?name:string -> float array -> 'c t
(** [of_array arr] is a series that produces [arr.(i)] at period [i]. Periods beyond
    [Array.length arr] produce [0.0]. The array is captured by reference and must not be mutated
    after the call. *)

val init : ?name:string -> (int -> Period.t -> float) -> 'c t
(** [init f] is a series that produces [f i period] at period [i]. The function receives both the
    zero-based index and the {!Period.t}, giving access to calendar dates.

    {[
      let days = Series.init (fun _i p -> Period.days p |> float_of_int)
    ]} *)

val init_tl : ?name:string -> (Timeline.t -> int -> Period.t -> float) -> 'c t
(** [init_tl f] is like {!init} but [f] also receives the {!Timeline.t} being evaluated. Use this
    when the computation needs timeline-level information such as the total number of periods or
    precomputation across all periods. *)


val growth_simple :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_simple ~start_date ~rate initial] is a series that produces
    [initial *. (1.0 +. rate *. daycount start_date period_start)] at each period (simple / linear
    growth). [daycount] defaults to {!Daycount.actual_360}.

    {[
      let revenue = Series.growth_simple ~start_date ~rate:0.05 8000.0

      let rent =
        Series.growth_simple ~start_date ~rate:0.03 ~daycount:Daycount.calendar_monthly 2000.0
    ]}

    See also {!growth_compound} for compounding growth. *)

val growth_compound :
  ?name:string ->
  ?daycount:(Date.t -> Date.t -> float) ->
  start_date:Date.t ->
  rate:float ->
  float ->
  'c t
(** [growth_compound ~start_date ~rate initial] is a series that produces
    [initial *. (1.0 +. rate) ** (daycount start_date period_start)] at each period (discrete
    compounding). [daycount] defaults to {!Daycount.actual_360}.

    Use this when the rate compounds rather than accrues linearly. See also {!growth_simple}. *)

val year_frac : ?name:string -> (Date.t -> Date.t -> float) -> 'c t
(** [year_frac daycount] is a series that produces [daycount period_start period_end] at each
    period. Useful for computing interest as [balance * rate * year_frac]. *)

val of_events : ?name:string -> (Date.t * float) list -> 'c t
(** [of_events events] distributes sparse [(date, value)] pairs into periods. Each event is placed
    in the period found by {!Timeline.find_index} (start-inclusive, end-exclusive; last period
    end-inclusive). Multiple events in the same period are summed. Periods with no events produce
    [0.0]. Events outside the timeline are silently dropped.

    {b Performance.} Events are binned once per evaluation in O(events {e *} log periods) using
    binary search; subsequent period lookups are O(1). *)

(** {1:naming Naming} *)

val named : string -> 'c t -> 'c t
(** [named name s] attaches [name] to [s] for use in {!Cycle_error} and {!Convergence_error}
    diagnostics and as a label in {!Deps.pp_dot}. The returned series shares memoization state with
    [s]. *)

(** {1:pointwise Pointwise combinators}

    These combinators operate independently within each period. For period [i], only the values at
    period [i] of the input series are used. *)

val map : ?name:string -> (float -> float) -> 'c t -> 'c t
(** [map f s] applies [f] to each value of [s]. *)

val map2 : ?name:string -> (float -> float -> float) -> 'c t -> 'c t -> 'c t
(** [map2 f a b] applies [f] to the values of [a] and [b] at each period. *)

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
(** [div a b] is the pointwise quotient [a /. b]. Division by zero produces [infinity] or [nan] per
    IEEE 754. *)

val abs : 'c t -> 'c t
(** [abs s] is the pointwise absolute value of [s]. *)

val min : 'c t -> 'c t -> 'c t
(** [min a b] is the pointwise minimum of [a] and [b]. *)

val max : 'c t -> 'c t -> 'c t
(** [max a b] is the pointwise maximum of [a] and [b]. *)

val clamp : lo:float -> hi:float -> 'c t -> 'c t
(** [clamp ~lo ~hi s] clamps each value of [s] to the range [[lo, hi]]. *)

val round : int -> 'c t -> 'c t
(** [round digits s] rounds each value of [s] to [digits] decimal places. Useful for simulating
    currency precision. *)

val sum : ?name:string -> 'c t list -> 'c t
(** [sum ss] is the pointwise sum of all series in [ss]. The empty list produces [0.0] at every
    period. *)

(** {1:conditional Conditional} *)

val where : cond:'a t -> then_:'c t -> else_:'c t -> 'c t
(** [where ~cond ~then_ ~else_] selects [then_] when [cond.(i) <> 0.0] and [else_] otherwise. Only
    the selected branch is evaluated at each period.

    The condition tag ['a] is independent of the result tag ['c], so a condition derived from one
    currency can guard branches of another. *)

(** {1:cross_period Cross-period operators}

    These operators introduce dependencies between periods and are the only way to express temporal
    relationships. *)

val prev : ?name:string -> 'c t -> default:float -> 'c t
(** [prev s ~default] produces [default] at period 0 and [s.(i-1)] at period [i > 0]. This is the
    primitive for cross-period dependencies and the mechanism by which {!feedback} breaks
    same-period cycles in mutually recursive series. *)

val scan : ?name:string -> init:float -> (acc:float -> x:float -> float) -> 'c t -> 'c t
(** [scan ~init f flow] produces a running accumulation over [flow]:
    - Period 0: [f ~acc:init ~x:flow.(0)]
    - Period i: [f ~acc:result.(i-1) ~x:flow.(i)]

    The accumulator [result.(i-1)] refers to the output of [scan] itself at the previous period, not
    to [flow]. This makes [scan] suitable for balance-style computations where the output feeds back
    into itself.

    {[
      let balance = Series.scan ~init:1000.0 (fun ~acc ~x -> acc +. x) net_flow
      (* balance.(0) = 1000 + net_flow.(0)
         balance.(1) = balance.(0) + net_flow.(1)
         ... *)
    ]} *)

val cumsum : ?name:string -> init:float -> 'c t -> 'c t
(** [cumsum ~init flow] is {!scan}[ ~init (fun ~acc ~x -> acc +. x) flow]. Produces a running total
    starting from [init]. *)

val feedback : ?name:string -> default:float -> ('c t -> 'c t * 'a) -> 'a
(** [feedback ~default f] ties a self-referential knot. It calls [f] with a series representing the
    {e previous period's} value of the series that [f] defines ([default] at period 0). [f] returns
    [(definition, result)] where [definition] is the series fed back and [result] is returned to the
    caller.

    This is the standard pattern for mutual recursion between series. For example, interest depends
    on the prior balance but the balance depends on principal which depends on interest:

    {[
      let balance, interest =
        Series.feedback ~default:loan_amount (fun prev_balance ->
            let interest =
              Series.map2 (fun bal yf -> -.bal *. rate *. yf) prev_balance year_fracs
            in
            let balance = Series.cumsum ~init:loan_amount (Series.sub total_pmt interest) in
            (balance, (balance, interest)))
    ]}

    @raise Cycle_error if [definition] contains a same-period cycle. *)

val fixpoint : ?name:string -> ?tol:float -> ?max_iter:int -> guess:float -> ('c t -> 'c t) -> 'c t
(** [fixpoint ~guess f] finds the value [x] at each period such that [f (const x)] produces [x]
    (within tolerance [tol]).

    At period 0, iteration starts from [guess]. At subsequent periods it starts from the previous
    period's converged value (warm start).

    This differs from {!feedback}: [feedback] shifts by one period to break cycles, while [fixpoint]
    iterates {e within} a single period to resolve same-period circular dependencies.

    {[
      let loan_commitment =
        Series.fixpoint ~guess:0.0 (fun commitment ->
            let interest_reserve = Series.mul (Series.scale rate commitment) year_fracs in
            let total_costs = Series.add hard_costs interest_reserve in
            Series.scale ltc_ratio total_costs)
    ]}

    [tol] defaults to [1e-10]. [max_iter] defaults to [100].

    @raise Convergence_error if [max_iter] iterations are exhausted. *)

(** {1:convert Currency conversion} *)

val convert : rate:float -> 'c1 t -> 'c2 t
(** [convert ~rate s] scales [s] by [rate] and changes the currency tag. The target currency is
    inferred from context. *)

(** {1:eval Evaluation} *)

exception Cycle_error of { series_name : string option; period_index : int }
(** Raised when evaluation detects a same-period dependency cycle. [series_name] is present when the
    series was given a name with {!named} or a [?name] parameter. *)

exception Convergence_error of { series_name : string option; period_index : int; iterations : int }
(** Raised when {!fixpoint} does not converge. [iterations] is the number of iterations attempted.
*)

val eval : Timeline.t -> 'c t -> float array
(** [eval tl s] materializes [s] against [tl], returning one [float] per period. Each series
    evaluates at a given period at most once.

    @raise Cycle_error if a same-period cycle is detected.
    @raise Convergence_error if a {!fixpoint} does not converge. *)

val eval_many : Timeline.t -> 'c t list -> float array list
(** [eval_many tl ss] materializes each series in [ss] against [tl], sharing a single memoization
    context. Use this when evaluating multiple series that share subexpressions to avoid redundant
    computation.

    @raise Cycle_error if a same-period cycle is detected.
    @raise Convergence_error if a {!fixpoint} does not converge. *)

(** {1:syntax Infix syntax}

    Open this module to use arithmetic operators on series.

    {b Warning.} This shadows the [Stdlib] integer operators [( + )], [( - )], [( * )] and [( / )].
*)

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
  (** [k *$ s] is {!scale}[ k s]. *)
end

(** {1:query Date queries}

    Functions for probing materialized [float array] values at arbitrary dates. These operate on the
    results of {!eval}, not on {!type-t} values directly.

    Boundary periods are split using a {!Query.split_fn} that defaults to pro-rata by day count
    ({!Query.default_split_fn}). *)

module Query : sig
  type split_fn =
    start_date:Date.t -> end_date:Date.t -> split_date:Date.t -> value:float -> float * float
  (** The type for functions that split a period's value at a date. Given the period's [start_date],
      [end_date], and a [split_date] within the period, returns [(before, after)] where
      [before +. after = value]. *)

  val default_split_fn : split_fn
  (** [default_split_fn] distributes the value proportionally by day count. For a period of [d] days
      where [split_date] falls [k] days after the start: [before = value *. k /. d]. Returns
      [(0.0, 0.0)] when the period has zero days. *)

  val interpolate : ?split_fn:split_fn -> Timeline.t -> float array -> Date.t -> float
  (** [interpolate tl values date] is the portion of the enclosing period's value that falls before
      [date].

      @raise Invalid_argument if [date] is outside [tl]. *)

  val accrue :
    ?split_fn:split_fn -> Timeline.t -> float array -> start_date:Date.t -> end_date:Date.t -> float
  (** [accrue tl values ~start_date ~end_date] sums values over the date range. Periods fully
      contained in the range contribute their whole value. Boundary periods are split: the first
      period contributes its {e after} portion and the last period its {e before} portion. Returns
      [0.0] if the date range does not overlap the timeline. *)

  val balance_at :
    ?split_fn:split_fn -> Timeline.t -> balance:float array -> flow:float array -> Date.t -> float
  (** [balance_at tl ~balance ~flow date] is the interpolated balance at [date]. Computes the prior
      period's ending balance plus the portion of the current period's flow that falls before
      [date].

      Typical usage with a {!scan}-based balance:

      {[
        let flow_v = Series.eval tl flow in
        let balance_v = Series.eval tl balance in
        Series.Query.balance_at tl ~balance:balance_v ~flow:flow_v (Date.make 2025 7 1)
      ]}

      @raise Invalid_argument if [date] is outside [tl]. *)
end

(** {1:deps Dependency graph}

    Inspect the structure of the computation DAG. *)

module Deps : sig
  type node = { id : int; name : string option; kind : string }
  (** A node in the dependency graph. [kind] is one of: ["const"], ["init"], ["map"], ["map2"],
      ["sum"], ["prev"], ["scan"], ["where"], ["fixpoint"], ["var"]. *)

  type edge = { src : int; dst : int }
  (** A directed edge from a node to one of its dependencies. *)

  val graph : ?named_only:bool -> _ t list -> node list * edge list
  (** [graph roots] returns all nodes and edges reachable from [roots] by depth-first traversal.

      When [named_only] is [true], only named nodes are returned. Unnamed intermediate nodes are
      collapsed: if named node A reaches named node B through any chain of unnamed nodes, the result
      contains a direct edge from A to B. This produces a high-level "line item" view of the model.
      Default is [false]. *)

  val pp_dot : ?named_only:bool -> Format.formatter -> _ t list -> unit
  (** [pp_dot ppf roots] formats a Graphviz DOT representation of the DAG reachable from [roots].
      Named nodes use their name as label; unnamed nodes use their kind. Nodes are styled by
      category: leaf data (box, blue), pointwise operators (ellipse, gray), cross-period operators
      (dashed box, orange), and conditionals (diamond, gray).

      When [named_only] is [true], unnamed nodes are collapsed (see {!graph}) and all nodes are
      rendered as uniform rounded boxes. Default is [false]. *)
end
