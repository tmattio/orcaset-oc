(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Declarative financial modeling.

    Orcaset models financial projections as declarative computations over time. The pipeline is:

    + Define {!Date}s and {!Period}s to establish calendar boundaries.
    + Build a {!Timeline} -- a finite sequence of periods shared by all computations.
    + Describe {!Series} -- lazy, composable recipes that produce one [float] per period.
    + Optionally, organize series into a hierarchical {!Statement} for structured output.

    Use {!Daycount} conventions for year fraction calculations (interest accrual, growth rates,
    etc.).

    {1 Quick start}

    {[
      open Orcaset2

      let () =
        let start = Date.make 2025 1 1 in
        let tl = Timeline.monthly ~start_date:start ~n:12 in
        let revenue = Series.growth_simple ~start_date:start ~rate:0.05 8000.0 in
        let expense = Series.const (-3000.0) in
        let income = Series.add revenue expense in
        let values = Series.eval tl income in
        Array.iter (fun v -> Printf.printf "%.0f\n" v) values
    ]}

    Open the module to use it, it defines only modules in your scope.

    {1 Modules}

    {!modules:Date Period Daycount Timeline Series Statement} *)

module Date : module type of Date
(** Gregorian calendar dates. *)

module Period : module type of Period
(** Contiguous time intervals. *)

module Daycount : module type of Daycount
(** Day count conventions for year fraction calculations. *)

module Timeline : module type of Timeline
(** Finite ordered sequences of periods. *)

module Series : module type of Series
(** Declarative computations over a timeline. *)

module Statement : module type of Statement
(** Hierarchical statement structure. *)
