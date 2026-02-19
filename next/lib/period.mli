(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Contiguous time intervals.

    A period is an ordered pair of dates representing a time interval interpreted as
    [\[start_date, end_date)] (start-inclusive, end-exclusive). This ensures contiguous periods
    produced by {!make_seq} tile without overlap. The length in days is [end_date - start_date],
    which is zero when both dates are equal and negative when [end_date] precedes [start_date] (no
    validation is performed).

    {1 Periods} *)

type t
(** The type for time periods. *)

val make : start_date:Date.t -> end_date:Date.t -> t
(** [make ~start_date ~end_date] is a period from [start_date] to [end_date].

    {b Note.} No validation is performed on date ordering. *)

(** {1:accessors Accessors} *)

val start_date : t -> Date.t
(** [start_date p] is [p]'s start date. *)

val end_date : t -> Date.t
(** [end_date p] is [p]'s end date. *)

val days : t -> int
(** [days p] is the number of calendar days in [p], i.e. {!Date.diff}[ (end_date p) (start_date p)].
*)

val contains : t -> Date.t -> bool
(** [contains p d] is [true] iff [start_date p <= d] and [d < end_date p]. Start-inclusive,
    end-exclusive. *)

(** {1:offsets Offsets}

    An offset describes a composite duration made of days, weeks, months, quarters, and years.
    Offsets are used to shift dates and periods by calendar-aware increments. *)

type offset = {
  days : int;
  weeks : int;
  months : int;
  quarters : int;
  years : int;
  month_end : bool;
}
(** The type for calendar offsets. When [month_end] is [true], the resulting date is snapped to the
    last day of its month after all other components are applied. *)

val make_offset :
  ?days:int ->
  ?weeks:int ->
  ?months:int ->
  ?quarters:int ->
  ?years:int ->
  ?month_end:bool ->
  unit ->
  offset
(** [make_offset ()] is an offset with all components defaulting to [0] and [month_end] to [false].
*)

(** {1:shifting Shifting}

    Shifting applies month-based components first (months, quarters, years) then day-based
    components (days, weeks), and finally the [month_end] snap. *)

val shift_date : offset -> Date.t -> Date.t
(** [shift_date offset d] is [d] advanced by [offset]. Month-based components are applied before
    day-based ones. Day clamping from {!Date.add_months} applies to the month step. *)

val shift : offset -> t -> t
(** [shift offset p] is [p] with both endpoints advanced by [offset]. *)

(** {1:sequences Sequences} *)

val make_seq : start_date:Date.t -> offset:offset -> t Seq.t
(** [make_seq ~start_date ~offset] is an infinite sequence of contiguous periods. The first period
    starts at [start_date] and ends at [shift_date offset start_date]. Each subsequent period starts
    where the previous one ended. *)

(** {1:preds Predicates and comparisons} *)

val compare : t -> t -> int
(** [compare p0 p1] is a total order on periods, ordered by {!start_date} then {!end_date}. *)

val equal : t -> t -> bool
(** [equal p0 p1] is [true] iff both endpoints are equal. *)

(** {1:fmt Formatting} *)

val to_string : t -> string
(** [to_string p] is [p] formatted as ["YYYY-MM-DD..YYYY-MM-DD"]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a period with {!to_string}. *)
