(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Day count conventions.

    Year fraction calculations used in financial modelling to express the time between two dates as
    a fraction of a year. Each convention makes different assumptions about month and year lengths.

    All functions in this module are {e directed}: the result is positive when [d1] is after [d0]
    and negative when [d1] is before [d0]. Swapping the arguments flips the sign.

    {1 Conventions} *)

val actual_360 : Date.t -> Date.t -> float
(** [actual_360 d0 d1] is the actual number of calendar days between [d0] and [d1] divided by 360.
    The simplest convention; see also {!thirty_360_us} and {!calendar_monthly}. *)

val actual_365 : Date.t -> Date.t -> float
(** [actual_365 d0 d1] is the actual number of calendar days between [d0] and [d1] divided by 365.
    Also known as Actual/365 Fixed. Unlike {!actual_actual_isda}, the denominator is always 365
    regardless of leap years. *)

val actual_actual_isda : Date.t -> Date.t -> float
(** [actual_actual_isda d0 d1] is the year fraction under the Actual/Actual (ISDA) convention. The
    interval is split at calendar year boundaries and each portion is divided by the actual number
    of days in that year (365 or 366). For intervals within a single year, this equals
    [actual_days / days_in_year]. For intervals spanning multiple years, the contributions of each
    year are summed. *)

val thirty_360_us : Date.t -> Date.t -> float
(** [thirty_360_us d0 d1] is the year fraction under the 30/360 US convention (NASD / Bond Basis).
    Each month is treated as 30 days and each year as 360 days, with end-of-month adjustments
    applied as a cascade where each rule's output feeds the next:

    + If the start day is 31, it becomes 30.
    + If the start date falls on February month-end, the start day becomes 30.
    + If both dates fall on February month-end, the end day becomes 30.
    + If the end day is 31 and the (adjusted) start day is {e >= 30}, the end day becomes 30.

    See also {!actual_360} and {!calendar_monthly}. *)

val calendar_monthly : Date.t -> Date.t -> float
(** [calendar_monthly d0 d1] is the year fraction where each whole calendar month counts as exactly
    1/12 of a year. Partial months are prorated by position within the month:
    [month_position(d) = month_index + (day - 1) / days_in_month], and the result is
    [(month_position(d1) - month_position(d0)) / 12].

    This ensures exact results at month boundaries: Jan 1 to Feb 1 is exactly 1/12, Jan 1 to Jul 1
    is exactly 1/2, and Jan 1 to Jan 1 next year is exactly 1.0. See also {!actual_360} and
    {!thirty_360_us}. *)
