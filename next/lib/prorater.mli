(*---------------------------------------------------------------------------
   Copyright (c) 2026 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
 ---------------------------------------------------------------------------*)

(** Allocate a full-period value to a sub-range.

    A `Prorater.t` answers a simple question: what fraction of a period's value belongs to a
    sub-range of that period?

    Both {!Flow} and {!Balance} use proraters when exact query semantics are unavailable and Orcaset
    needs to fall back to proportional allocation.

    {1 Built-in proraters}

    Orcaset ships a small, opinionated set of built-in proraters:

    - {!actual_days} for calendar-day overlap
    - {!thirty_360_us} for 30/360 US overlap
    - {!business_days} for business-day overlap under a supplied {!Calendar.t} *)

type t = start_date:Date.t -> end_date:Date.t -> sub_start:Date.t -> sub_end:Date.t -> float
(** The type for proraters.

    Given the full period [[start_date, end_date)] and a sub-range
    [[sub_start, sub_end)], returns the fraction of the full-period value that
    belongs to the sub-range. *)

val actual_days : t
(** [actual_days] allocates proportionally by calendar-day overlap. Zero-length periods map to
    [0.0]. *)

val thirty_360_us : t
(** [thirty_360_us] allocates by the 30/360 US convention.

    This is useful when a source period's value is intended to accrue under a 30/360-style
    convention rather than by actual calendar days. Zero-length periods map to [0.0]. *)

val business_days : Calendar.t -> t
(** [business_days cal] allocates proportionally by business-day overlap under [cal].

    Non-business days receive zero weight. If the full period contains no business days under [cal],
    the result is [0.0]. *)
