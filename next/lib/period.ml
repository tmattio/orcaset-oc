(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type t = { start_date : Date.t; end_date : Date.t }

type offset = {
  days : int;
  weeks : int;
  months : int;
  quarters : int;
  years : int;
  month_end : bool;
}

let make ~start_date ~end_date = { start_date; end_date }

(* CR: consider renaming to offset. *)
let make_offset ?(days = 0) ?(weeks = 0) ?(months = 0) ?(quarters = 0) ?(years = 0)
    ?(month_end = false) () =
  { days; weeks; months; quarters; years; month_end }

(* Accessors *)

let start_date p = p.start_date
let end_date p = p.end_date
let days p = Date.diff p.end_date p.start_date
let contains period date = Date.(date >= period.start_date && date < period.end_date)

(* Application order matters: months first (so day-clamping from
   Date.add_months happens before day offsets), then days, then the
   month_end snap which overrides the day to the last of the month. *)
let shift_date offset date =
  let total_months = offset.months + (offset.quarters * 3) + (offset.years * 12) in
  let total_days = offset.days + (offset.weeks * 7) in
  let d = Date.add_months date total_months in
  let d = if total_days = 0 then d else Date.add_days d total_days in
  if offset.month_end then Date.make (Date.year d) (Date.month d) (Date.days_in_month d) else d

let shift offset period =
  { start_date = shift_date offset period.start_date; end_date = shift_date offset period.end_date }

let compare p0 p1 =
  let c = Date.compare p0.start_date p1.start_date in
  if c <> 0 then c else Date.compare p0.end_date p1.end_date

let equal p0 p1 = compare p0 p1 = 0
let to_string p = Date.to_string p.start_date ^ ".." ^ Date.to_string p.end_date
let pp fmt p = Format.pp_print_string fmt (to_string p)

(* Produces an infinite sequence; callers must use Seq.take or similar
   to bound it. Each period starts where the previous one ended, so
   the sequence covers contiguous, non-overlapping intervals. *)
let make_seq ~start_date ~offset =
  let end_date = shift_date offset start_date in
  let initial_period = { start_date; end_date } in
  Seq.unfold
    (fun period ->
      let next_period = shift offset period in
      Some (period, next_period))
    initial_period
