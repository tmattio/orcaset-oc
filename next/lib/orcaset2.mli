(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Declarative financial modeling.

    Orcaset models financial projections as declarative computations over time:

    + Define {!Flow}s (interval quantities) and {!Balance}s (point-in-time values).
    + Bridge flows to balances with {!Balance.roll_forward}.
    + Materialize against a {!Timeline} or {!Schedule}.
    + Query: {!Flow.Materialized.accrue} for date-range totals, {!Balance.Materialized.at} for
      point-in-time values.
    + Organize into hierarchical {!Statement}s for structured output.

    {1 Quick start}

    {[
      open Orcaset2

      let () =
        let start = Date.make 2025 1 1 in
        let tl = Timeline.monthly ~start_date:start ~n:12 in
        let revenue = Flow.growth_simple ~start_date:start ~rate:0.05 8000.0 in
        let expense = Flow.const (-3000.0) in
        let income = Flow.add revenue expense in
        let cash = Balance.roll_forward ~init:50000.0 income in
        let cash_m = Balance.eval tl cash in
        Printf.printf "Cash at mid-year: %.0f\n"
          (Balance.Materialized.at cash_m (Date.make 2025 7 15));
        let income_m = Flow.eval tl income in
        Printf.printf "H1 income: %.0f\n"
          (Flow.Materialized.accrue income_m ~start_date:start ~end_date:(Date.make 2025 7 1))
    ]}

    Open the module to use it, it defines only modules in your scope.

    {1 Modules}

    {!modules:Date Period Daycount Calendar Timeline Schedule Key Scope Prorater Pointwise Flow
    Balance Statement} *)

module Date : module type of Date
(** Gregorian calendar dates. *)

module Period : module type of Period
(** Contiguous time intervals and calendar offsets. *)

module Daycount : module type of Daycount
(** Day count conventions for year fraction calculations. *)

module Calendar : module type of Calendar
(** Business day calendars and adjustment conventions. *)

module Timeline : module type of Timeline
(** Finite ordered sequences of periods. *)

module Schedule : module type of Schedule
(** Financial schedule generation with roll, stub, and business day conventions. *)

module Key : sig
  (** Typed unique line-item identifiers.

      A {!type-t} is a typed identity for a model line item. The type parameter ['a] encodes what
      type of value this key maps to in a {!Scope}. Two keys with the same display name but created
      by different {!make} calls are always distinct.

      {1:keys Keys} *)

  type 'a t = 'a Key.t
  (** The type for line-item keys. ['a] is the type of the associated value in any {!Scope}. *)

  val make : string -> 'a t
  (** [make name] is a fresh key with display name [name]. Each call produces a distinct key; two
      calls with the same [name] are never equal. *)

  val name : 'a t -> string
  (** [name k] is the display name of [k]. *)

  val equal : 'a t -> 'b t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same key (same {!make} call). Keys of different
      types are never equal. *)

  val compare : 'a t -> 'b t -> int
  (** [compare] is a total order on keys consistent with creation order. *)

  val to_string : 'a t -> string
  (** [to_string k] is [name k]. *)

  val pp : Format.formatter -> 'a t -> unit
  (** [pp] formats a key with its display name. *)

  (** {1:packing Existential packing} *)

  type packed = Key.packed = Key : 'a t -> packed  (** A key with its type erased. *)

  val pack : 'a t -> packed
  (** [pack k] erases the type of [k]. *)
end

module Scope : module type of Scope
(** Typed heterogeneous model registries. *)

module Prorater : module type of Prorater
(** Allocate a full-period value to a sub-range. *)

module Pointwise : module type of Pointwise
(** Per-cell values without interval or point-in-time query semantics. *)

module Flow : sig
  (** Interval quantities (flows).

      A {!type-t} represents a quantity that accrues over intervals: revenue, expenses, cash
      movements, principal amortization. The natural query is "how much over a date range?",
      answered by {!Materialized.accrue}.

      {2 Algebra}

      The {!section:algebra} ({!add}, {!sub}, {!scale}, {!neg}, {!sum}) composes correctly through
      partial-period accrual.

      The {!section:cell_local} ({!map}, {!map2}, {!mul}) applies per-period and does {b not}
      commute with {!Materialized.accrue} in general. Use for percentage calculations, conditional
      logic, and cross-type operations (e.g. balance {e ×} year fraction).

      {1:types Types} *)

  type 'c t = 'c Flow.t
  (** The type for flows tagged with currency or unit ['c]. *)

  (** {1:exceptions Exceptions} *)

  exception Cycle_error of { formula_name : string option; period_index : int }
  (** Raised when evaluation detects a same-period dependency cycle. *)

  exception
    Convergence_error of { formula_name : string option; period_index : int; iterations : int }
  (** Raised when {!fixpoint} does not converge. *)

  (** {1:constructors Constructors} *)

  val const : ?name:string -> float -> 'c t
  (** [const v] is a flow that produces [v] at every period. *)

  val init : ?name:string -> (Period.t -> float) -> 'c t
  (** [init f] is a flow that produces [f period] at each period. *)

  val init_indexed : ?name:string -> (int -> Period.t -> float) -> 'c t
  (** [init_indexed f] is a flow that produces [f i period] at period [i]. Prefer {!init} unless the
      index is genuinely needed (e.g. indexing into an external array). *)

  val of_array : ?name:string -> float array -> 'c t
  (** [of_array arr] is a flow that produces [arr.(i)] at period [i]. Periods beyond
      [Array.length arr] produce [0.0]. The array is captured by reference and must not be mutated
      after the call. *)

  val of_events : ?name:string -> (Date.t * float) list -> 'c t
  (** [of_events events] distributes sparse [(date, value)] pairs into periods. Each event is placed
      in the period found by {!Timeline.find_index} (start-inclusive, end-exclusive; last period
      end-inclusive). Multiple events in the same period are summed. Periods with no events produce
      [0.0].

      Raises [Invalid_argument] if any event date falls outside the timeline. *)

  val of_periods : ?name:string -> ?prorater:Prorater.t -> (Period.t * float) list -> 'c t
  (** [of_periods pairs] distributes period-keyed values into the evaluation timeline using
      overlap-based splitting. Each source period's value is allocated to evaluation periods
      proportionally to the overlap, using [prorater] (default: pro-rata by day count).

      Source periods that partially overlap an evaluation period contribute only the overlapping
      portion. Multiple source periods overlapping the same evaluation period are summed. Evaluation
      periods with no overlap produce [0.0]. *)

  val growth_simple :
    ?name:string ->
    ?daycount:(Date.t -> Date.t -> float) ->
    start_date:Date.t ->
    rate:float ->
    float ->
    'c t
  (** [growth_simple ~start_date ~rate initial] is a flow with simple (linear) growth. *)

  val growth_compound :
    ?name:string ->
    ?daycount:(Date.t -> Date.t -> float) ->
    start_date:Date.t ->
    rate:float ->
    float ->
    'c t
  (** [growth_compound ~start_date ~rate initial] is a flow with compound growth. *)

  val year_frac : ?name:string -> (Date.t -> Date.t -> float) -> 'c t
  (** [year_frac daycount] is a flow that produces the year fraction of each period. *)

  (** {1:naming Naming} *)

  val named : string -> 'c t -> 'c t
  (** [named name f] attaches [name] to [f] for diagnostics. *)

  (** {1:algebra Exact algebra}

      These operations compose correctly through partial-period accrual. *)

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

      These operations apply per-period and do {b not} compose through partial-period accrual: in
      general, [f(accrue(flow))] {e !=} [accrue(map(f, flow))]. Use them for percentage
      calculations, conditional logic, and cross-type operations where period-level semantics
      suffice. *)

  val map : ?name:string -> (float -> float) -> 'c t -> 'c t
  (** [map f flow] applies [f] to each period's value. *)

  val map2 : ?name:string -> (float -> float -> float) -> 'c t -> 'c t -> 'c t
  (** [map2 f a b] applies [f] to the values of [a] and [b] at each period. *)

  val mul : 'c t -> 'c t -> 'c t
  (** [mul a b] is the pointwise product of [a] and [b]. *)

  val div : 'c t -> 'c t -> 'c t
  (** [div a b] is the pointwise quotient [a /. b]. Division by zero produces [infinity] or [nan]
      per IEEE 754. *)

  val abs : 'c t -> 'c t
  (** [abs f] is the pointwise absolute value of [f]. *)

  val min : 'c t -> 'c t -> 'c t
  (** [min a b] is the pointwise minimum of [a] and [b]. *)

  val max : 'c t -> 'c t -> 'c t
  (** [max a b] is the pointwise maximum of [a] and [b]. *)

  val clamp : lo:float -> hi:float -> 'c t -> 'c t
  (** [clamp ~lo ~hi f] clamps each value to [[lo, hi]]. *)

  val round : int -> 'c t -> 'c t
  (** [round digits f] rounds each value to [digits] decimal places. *)

  val where : cond:'a t -> then_:'c t -> else_:'c t -> 'c t
  (** [where ~cond ~then_ ~else_] selects [then_] when [cond.(i) <> 0.0] and [else_] otherwise. Only
      the selected branch is evaluated at each period. *)

  (** {1:feedback Feedback} *)

  val feedback : ?name:string -> default:float -> ('c t -> 'c t * 'a) -> 'a
  (** [feedback ~default f] ties a self-referential knot for flows. It calls [f] with a flow
      representing the {e previous period's} value ([default] at period 0). [f] returns
      [(definition, result)] where [definition] is the flow fed back and [result] is returned to the
      caller.

      Raises {!Cycle_error} if [definition] contains a same-period cycle. *)

  val fixpoint :
    ?name:string -> ?tol:float -> ?max_iter:int -> guess:float -> ('c t -> 'c t) -> 'c t
  (** [fixpoint ~guess f] finds the value [x] at each period such that [f (const x)] converges to
      [x] (within [tol]).

      [tol] defaults to [1e-10]. [max_iter] defaults to [100].

      Raises {!Convergence_error} if [max_iter] iterations are exhausted. *)

  (** {1:convert Currency conversion} *)

  val convert : rate:float -> 'c1 t -> 'c2 t
  (** [convert ~rate f] scales [f] by [rate] and changes the currency tag. *)

  (** {1:eval Evaluation} *)

  (** Materialized results that keep period bindings attached, preventing accidental misalignment
      between values and their periods. *)
  module Materialized : sig
    type query_mode = Flow.Materialized.query_mode =
      | Exact
      | Approx
          (** Whether {!accrue} is answered from exact AST semantics or by prorating materialized
              cells. *)

    type 'c t = 'c Flow.Materialized.t
    (** The type for materialized flow results. Each value is bound to its period. *)

    val query_mode : _ t -> query_mode
    (** [query_mode m] reports whether date-range accrual is safe to trust as exact. *)

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
    (** [fold f init m] folds [f] over each [(period, value)] pair. *)

    val accrue : ?prorater:Prorater.t -> _ t -> start_date:Date.t -> end_date:Date.t -> float
    (** [accrue m ~start_date ~end_date] sums the flow over the date range.

        When [query_mode m = Exact], the query is answered from exact AST semantics (events,
        source-period overlap, and exact linear compositions). Otherwise it falls back to [prorater]
        (default: pro-rata by day count). Returns [0.0] if the date range does not overlap the
        timeline. Range semantics are half-open: [start_date] is included and [end_date] is
        excluded. *)
  end

  val eval : Timeline.t -> 'c t -> 'c Materialized.t
  (** [eval tl f] materializes [f] against [tl], returning a {!Materialized.t} with period bindings
      attached. *)

  val eval_many : Timeline.t -> 'c t list -> 'c Materialized.t list
  (** [eval_many tl fs] materializes each flow in [fs] against [tl], sharing a single memoization
      context. Use this when evaluating multiple flows that share subexpressions to avoid redundant
      computation. *)

  (** {1:deps Dependency graph} *)

  module Deps : sig
    type node = Flow.Deps.node = { id : int; name : string option; kind : string }
    (** A node in the dependency graph. *)

    type edge = Flow.Deps.edge = { src : int; dst : int }
    (** A directed edge from a dependency to a consumer. *)

    val graph : ?named_only:bool -> _ t list -> node list * edge list
    (** [graph roots] returns all nodes and edges reachable from [roots]. When [named_only] is
        [true], unnamed intermediate nodes are collapsed. Default is [false]. *)

    val pp_dot : ?named_only:bool -> Format.formatter -> _ t list -> unit
    (** [pp_dot ppf roots] formats a Graphviz DOT representation. *)
  end

  (** {1:syntax Infix syntax}

      Open this module to use arithmetic operators on flows.

      {b Warning.} This shadows the [Stdlib] integer operators [( + )], [( - )], [( * )] and
      [( / )]. *)

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
end

module Balance : sig
  (** Point-in-time quantities (balances).

      A {!type-t} represents a snapshot value at a date: cash balance, debt outstanding, inventory
      level. The natural query is "what is the value at date [d]?", answered by {!Materialized.at}.

      Unlike {!Flow.t}, balances support {!map} and {!map2} since pointwise transforms on snapshots
      preserve the "value at a date" semantics. The fundamental bridge from flow to balance is
      {!roll_forward}: a running sum of a flow starting from an initial value.

      {1:constructors Constructors} *)

  type 'c t = 'c Balance.t
  (** The type for balances tagged with currency or unit ['c]. *)

  val const : ?name:string -> float -> 'c t
  (** [const v] is a balance that produces [v] at every period. *)

  val of_array : ?name:string -> float array -> 'c t
  (** [of_array arr] is a balance that produces [arr.(i)] at period [i]. Periods beyond
      [Array.length arr] produce [0.0]. The array is captured by reference and must not be mutated
      after the call. *)

  val of_dates : ?name:string -> ?before_first:float -> (Date.t * float) list -> 'c t
  (** [of_dates observations] distributes date-keyed observations into periods using
      last-observation-wins semantics: each period takes the value of the most recent observation
      whose date falls on or before the period's end date. Periods before any observation produce
      [before_first] (default [0.0]).

      Raises [Invalid_argument] if any observation date falls outside the timeline. *)

  val of_observations : ?name:string -> ?before_first:float -> (Date.t * float) list -> 'c t
  (** [of_observations] is {!of_dates}. *)

  (** {1:bridge Bridge from Flow} *)

  val roll_forward : ?name:string -> init:float -> 'c Flow.t -> 'c t
  (** [roll_forward ~init flow] is a balance where each period's value is the running sum of [flow]
      starting from [init]:
      - Period 0: [init + flow.(0)]
      - Period i: [result.(i-1) + flow.(i)]

      This is the fundamental flow-to-balance bridge. The roll-forward semantics are stored
      intrinsically in the private typed AST and used by {!Materialized.at} for exact point queries.
  *)

  val roll_forward_with :
    ?name:string -> init:float -> (acc:float -> x:float -> float) -> 'c Flow.t -> 'c t
  (** [roll_forward_with ~init f flow] is like {!roll_forward} but uses [f] as the accumulation
      step. Point queries are conservative and report approximate mode. *)

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

  val map2 : ?name:string -> (float -> float -> float) -> 'c t -> 'c t -> 'c t
  (** [map2 f a b] applies [f] to values of [a] and [b] at each period. *)

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

  (** {1:cross_period Cross-period} *)

  val at_period_start : ?name:string -> 'c t -> default:float -> 'c t
  (** [at_period_start b ~default] produces [default] at period 0 and [b.(i-1)] at period [i > 0].
      The balance at the start of a period is the end-of-previous-period value. *)

  val at_period_end : 'c t -> 'c t
  (** [at_period_end b] is [b] (identity). A balance naturally represents the end-of-period value.
  *)

  (** {1:bridge_to_flow Bridge to Flow} *)

  val change : 'c t -> default:float -> 'c Flow.t
  (** [change b ~default] is the per-period change in [b], as a flow. At period 0, the change is
      [b.(0) - default]. At period [i > 0], the change is [b.(i) - b.(i-1)].

      Use {!Pointwise.of_balance} for per-cell arithmetic on balances. *)

  (** {1:feedback Feedback} *)

  val feedback : ?name:string -> default:float -> ('c t -> 'c t * 'a) -> 'a
  (** [feedback ~default f] ties a self-referential knot for balances. It calls [f] with a balance
      representing the {e previous period's} value ([default] at period 0). [f] returns
      [(definition, result)] where [definition] is the balance fed back and [result] is returned to
      the caller.

      This is the standard pattern for loan amortization:

      {[
        let balance, (interest, principal) =
          Balance.feedback ~default:loan_amount (fun prev_bal ->
              let interest =
                Flow.scale (-.rate)
                  (Pointwise.to_flow_approx
                     (Pointwise.mul (Pointwise.of_balance prev_bal) (Pointwise.of_flow year_fracs)))
              in
              let principal = Flow.sub total_pmt interest in
              let bal = Balance.roll_forward ~init:loan_amount principal in
              (bal, (interest, principal)))
      ]} *)

  val fixpoint :
    ?name:string -> ?tol:float -> ?max_iter:int -> guess:float -> ('c t -> 'c t) -> 'c t
  (** [fixpoint ~guess f] solves a same-period fixed point in each period. Point queries are
      conservative and report approximate mode unless the resulting balance has intrinsically exact
      semantics. *)

  (** {1:convert Currency conversion} *)

  val convert : rate:float -> 'c1 t -> 'c2 t
  (** [convert ~rate b] scales [b] by [rate] and changes the currency tag. *)

  (** {1:eval Evaluation} *)

  (** Materialized results that keep period bindings attached, preventing accidental misalignment
      between values and their periods. *)
  module Materialized : sig
    type query_mode = Balance.Materialized.query_mode =
      | Exact
      | Approx
          (** Whether {!at} is answered from exact AST semantics or by falling back to the enclosing
              period's materialized value. *)

    type 'c t = 'c Balance.Materialized.t
    (** The type for materialized balance results. Each value is bound to its period. *)

    val query_mode : _ t -> query_mode
    (** [query_mode m] reports whether point queries are safe to trust as exact. *)

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
    (** [fold f init m] folds [f] over each [(period, value)] pair. *)

    val at : ?prorater:Prorater.t -> _ t -> Date.t -> float
    (** [at m date] is the balance at [date].

        When [query_mode m = Exact], the answer is derived from intrinsic AST semantics (for example
        {!val:of_dates}, exact {!val:at_period_start}, and exact pointwise combinations). Otherwise
        it falls back to approximate timeline-cell semantics.

        Raises [Invalid_argument] if [date] is outside the timeline. *)
  end

  (** {1:deps Dependency graph} *)

  module Deps : sig
    type node = Balance.Deps.node
    (** A node in the dependency graph. *)

    type edge = Balance.Deps.edge
    (** A directed edge from a dependency to a consumer. *)

    val graph : ?named_only:bool -> _ t list -> node list * edge list
    (** [graph roots] returns all nodes and edges reachable from [roots]. *)

    val pp_dot : ?named_only:bool -> Format.formatter -> _ t list -> unit
    (** [pp_dot ppf roots] formats a Graphviz DOT representation. *)
  end

  val eval : Timeline.t -> 'c t -> 'c Materialized.t
  (** [eval tl b] materializes [b] against [tl], returning a {!Materialized.t} with period bindings
      attached. Exact point-query capability is derived from the balance AST itself. *)
end

module Statement : module type of Statement
(** Hierarchical statement structure for financial reports. *)
