(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Finite ordered sequences of periods.

    A timeline is an array of contiguous {!Period.t} values against which all {!Series.t} are
    evaluated. Every series in a model shares the same timeline, which fixes the number of output
    values and the calendar boundaries of each cell.

    {1:construction Construction}

    Use {!monthly}, {!quarterly}, or {!yearly} for common calendars. For custom offsets use {!make}.
    For fully explicit control use {!of_periods}. For financial schedules with roll, stub, and
    business day conventions use {!Schedule.make} and convert via {!Schedule.to_timeline}. *)

type t
(** The type for timelines. *)

(** {1:convenience Convenience constructors} *)

val monthly : start_date:Date.t -> n:int -> t
(** [monthly ~start_date ~n] creates [n] calendar-month periods starting from [start_date].
    Equivalent to [make ~start_date ~offset:(Period.make_offset ~months:1 ()) ~n].

    @raise Invalid_argument if [n <= 0]. *)

val quarterly : start_date:Date.t -> n:int -> t
(** [quarterly ~start_date ~n] creates [n] calendar-quarter periods starting from [start_date].
    Equivalent to [make ~start_date ~offset:(Period.make_offset ~quarters:1 ()) ~n].

    @raise Invalid_argument if [n <= 0]. *)

val yearly : start_date:Date.t -> n:int -> t
(** [yearly ~start_date ~n] creates [n] calendar-year periods starting from [start_date]. Equivalent
    to [make ~start_date ~offset:(Period.make_offset ~years:1 ()) ~n].

    @raise Invalid_argument if [n <= 0]. *)

(** {1:general General constructors} *)

val make : start_date:Date.t -> offset:Period.offset -> n:int -> t
(** [make ~start_date ~offset ~n] creates [n] contiguous periods starting from [start_date], each
    advanced by [offset]. Prefer {!monthly}, {!quarterly}, or {!yearly} for standard calendars.

    {b Day-of-month drift.} Each period starts where the previous one ended. Dates are shifted
    sequentially, so end-of-month start dates may drift: [Jan 31 -> Feb 28 -> Mar 28], not [Mar 31].
    For financial schedules that require stable day-of-month across periods, use {!Schedule.make}
    with a {!Schedule.roll} convention and convert via {!Schedule.to_timeline}.

    @raise Invalid_argument if [n <= 0]. *)

val of_periods : Period.t array -> t
(** [of_periods ps] creates a timeline from an explicit array of periods. The array is copied.

    {b Note.} No contiguity or ordering check is performed on the periods.

    @raise Invalid_argument if [ps] is empty. *)

(** {1:accessors Accessors} *)

val length : t -> int
(** [length tl] is the number of periods in [tl]. Always [>= 1]. *)

val get : t -> int -> Period.t
(** [get tl i] is the [i]th period of [tl]. Prefer {!period_start} and {!period_end} when only dates
    are needed. *)

val period_start : t -> int -> Date.t
(** [period_start tl i] is the start date of period [i]. *)

val period_end : t -> int -> Date.t
(** [period_end tl i] is the end date of period [i]. *)

val start_date : t -> Date.t
(** [start_date tl] is the start date of the first period. *)

val end_date : t -> Date.t
(** [end_date tl] is the end date of the last period. *)

(** {1:lookup Lookup} *)

val find_index : t -> Date.t -> int option
(** [find_index tl date] is [Some i] if [date] falls within period [i], or [None] if [date] is
    outside the timeline. Periods are start-inclusive and end-exclusive, except for the last period
    which is end-inclusive. Lookup is O(log n) by binary search.

    {b Note.} The last period is end-inclusive so that a date equal to the timeline's final
    {!end_date} maps to the last period rather than falling outside the timeline. *)

(** {1:fmt Formatting} *)

val to_string : t -> string
(** [to_string tl] is [tl] formatted as ["YYYY-MM-DD..YYYY-MM-DD (N periods)"]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a timeline with {!to_string}. *)
