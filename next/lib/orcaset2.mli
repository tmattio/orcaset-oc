(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Declarative financial modeling.

    Orcaset models financial projections as declarative computations
    over time:

    + Define {!Flow}s (interval quantities) and {!Balance}s
      (point-in-time values).
    + Bridge flows to balances with {!Balance.roll_forward}.
    + Materialize against a {!Timeline} or {!Schedule}.
    + Query: {!Flow.Materialized.accrue} for date-range totals,
      {!Balance.Materialized.at} for point-in-time values.
    + Organize into hierarchical {!Statement}s for structured
      output.

    For advanced grid-local operations ({!Formula.map},
    {!Formula.mul}, {!Formula.prev}, {!Formula.feedback},
    {!Formula.fixpoint}), use {!Formula}.

    {1 Quick start}

    {[
      open Orcaset2

      let () =
        let start = Date.make 2025 1 1 in
        let tl = Timeline.monthly ~start_date:start ~n:12 in
        let revenue =
          Flow.growth_simple ~start_date:start ~rate:0.05 8000.0
        in
        let expense = Flow.const (-3000.0) in
        let income = Flow.add revenue expense in
        let cash = Balance.roll_forward ~init:50000.0 income in
        let cash_m = Balance.eval tl cash in
        let income_m = Flow.eval tl income in
        Printf.printf "Cash at mid-year: %.0f\n"
          (Balance.Materialized.at cash_m
             ~flow:income_m (Date.make 2025 7 15));
        Printf.printf "H1 income: %.0f\n"
          (Flow.Materialized.accrue income_m
             ~start_date:start
             ~end_date:(Date.make 2025 7 1))
    ]}

    Open the module to use it, it defines only modules in your
    scope.

    {1 Modules}

    {!modules:
    Date Period Daycount Calendar Timeline Schedule
    Key Scope
    Flow Balance Statement
    Formula} *)

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
(** Financial schedule generation with roll, stub, and business day
    conventions. *)

module Key : module type of Key
(** Typed unique line-item identifiers. *)

module Scope : module type of Scope
(** Typed heterogeneous model registries. *)

module Flow : module type of Flow
(** Interval quantities (revenue, expenses, cash movements). *)

module Balance : module type of Balance
(** Point-in-time quantities (cash balance, debt outstanding). *)

module Statement : module type of Statement
(** Hierarchical statement structure for financial reports. *)

module Formula : module type of Formula
(** {b Advanced.} Grid-local formula engine for pointwise
    operations ({!Formula.map}, {!Formula.mul}) and sequential
    recurrences ({!Formula.prev}, {!Formula.scan},
    {!Formula.feedback}, {!Formula.fixpoint}). Most users should
    prefer {!Flow} and {!Balance}. *)
