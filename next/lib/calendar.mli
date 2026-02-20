(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Business day calendars and adjustment conventions.

    A {!calendar} predicate determines which dates are business days. A {!bdc} (business day
    convention) determines how to adjust a date that falls on a non-business day. Together they are
    used by {!Schedule.make} for payment date adjustment and can be applied directly via {!adjust}.

    {[
      let pay_date = Calendar.adjust Modified_following Calendar.weekdays raw_date
    ]}

    {1 Calendars} *)

type t = Date.t -> bool
(** The type for business day calendars. [t d] is [true] iff [d] is a business day. Supply a custom
    function to model exchange-specific holiday calendars. *)

val weekdays : t
(** [weekdays] treats all Monday--Friday dates as business days. No public holidays are excluded. *)

(** {1 Business day conventions} *)

(** The type for business day conventions. Determines how a non-business day is adjusted to a nearby
    business day.

    {b Note.} "Crosses a month boundary" means {!Date.month} differs between the original and
    candidate date. Year boundaries alone do not trigger the modified fallback. *)
type bdc =
  | Unadjusted  (** Return the date unchanged. *)
  | Following  (** Roll forward to the next business day. *)
  | Modified_following
      (** Roll forward to the next business day, unless it crosses a month boundary -- then roll
          backward instead. The most common convention for interest rate swaps and bonds. *)
  | Preceding  (** Roll backward to the previous business day. *)
  | Modified_preceding
      (** Roll backward to the previous business day, unless it crosses a month boundary -- then
          roll forward instead. *)

(** {1 Adjusting} *)

val adjust : bdc -> t -> Date.t -> Date.t
(** [adjust bdc cal d] adjusts [d] according to [bdc] and [cal]. When [bdc] is {!Unadjusted}, [d] is
    returned unchanged regardless of [cal]. When [d] is already a business day according to [cal],
    it is returned as-is for all conventions.

    {b Note.} The search for the nearest business day is unbounded. If [cal] returns [false] for all
    dates, [adjust] does not terminate. *)
