(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Declarative financial modeling.

    Orcaset models financial projections as declarative computations over time. The pipeline is:

    + Define {!Date}s and {!Period}s to establish calendar boundaries.
    + Build a {!Timeline} or {!Schedule} -- a finite sequence of periods shared by all computations.
    + Describe {!Formula} -- lazy, composable recipes that produce one [float] per period.
    + Optionally, organize formulas into a hierarchical {!Statement} for structured output.

    Use {!Daycount} conventions for year fraction calculations (interest accrual, growth rates) and
    {!Calendar} conventions for business day adjustment.

    {1 Quick start}

    {[
      open Orcaset2

      let () =
        let start = Date.make 2025 1 1 in
        let tl = Timeline.monthly ~start_date:start ~n:12 in
        let revenue = Formula.growth_simple ~start_date:start ~rate:0.05 8000.0 in
        let expense = Formula.const (-3000.0) in
        let income = Formula.add revenue expense in
        let values = Formula.eval tl income in
        Array.iter (fun v -> Printf.printf "%.0f\n" v) values
    ]}

    Open the module to use it, it defines only modules in your scope.

    {1 Modules}

    {!modules:Date Period Daycount Calendar Timeline Schedule Formula Statement} *)

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

module Key : module type of Key
(** Unique line-item identifiers. *)

module Scope : module type of Scope
(** Model-level registries for line-item definitions. *)

(* CR: do we need to expose formula globally? can it be an internal module so we avoid exposing API that we don't want the user to use? *)
module Formula : module type of Formula
(** Declarative computations over a timeline. *)

module Flow : module type of Flow
(** Interval quantities (revenue, expenses, cash movements). *)

module Balance : module type of Balance
(** Point-in-time quantities (cash balance, debt outstanding). *)

module Statement : module type of Statement
(** Hierarchical statement structure for financial reports. *)
