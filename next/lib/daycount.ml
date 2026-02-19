(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Helpers *)

let is_month_end date = Date.day date = Date.days_in_month date

(* All day-count conventions in this module are symmetric: swapping the two
   dates negates the result. Rather than handling directionality in each
   convention, we factor it out into [directed]. Each convention defines a
   [forward] function that assumes dt1 <= dt2 and returns a non-negative
   fraction; [directed] takes care of swapping and sign-flipping. *)
let directed f dt1 dt2 = if Date.(dt1 > dt2) then -.f dt2 dt1 else f dt1 dt2

(* Conventions *)

let actual_360 dt1 dt2 = float_of_int (Date.diff dt2 dt1) /. 360.0

(* NASD / US Bond Basis 30/360. Adjust d1 first, then d2 using the
   adjusted d1. The cascade:
   1. If d1 = 31, set d1 = 30
   2. If start is Feb month-end, set d1 = 30
   3. If both dates fall on Feb month-end, set d2 = 30
   4. If d2 = 31 and (adjusted) d1 >= 30, set d2 = 30
   The final formula uses a "30/360 epoch" (y*360 + m*30 + d) to compute
   the day difference in the fictitious 30-day-month calendar. *)
let thirty_360 =
  let forward dt1 dt2 =
    let y1 = Date.year dt1 in
    let m1 = Date.month dt1 in
    let d1 = Date.day dt1 in
    let y2 = Date.year dt2 in
    let m2 = Date.month dt2 in
    let d2 = Date.day dt2 in
    let start_is_feb_eom = m1 = 2 && is_month_end dt1 in
    let end_is_feb_eom = m2 = 2 && is_month_end dt2 in
    let d1 = if d1 = 31 then 30 else d1 in
    let d1 = if start_is_feb_eom then 30 else d1 in
    let d2 = if end_is_feb_eom && start_is_feb_eom then 30 else d2 in
    let d2 = if d2 = 31 && d1 >= 30 then 30 else d2 in
    let days = d2 + (m2 * 30) + (y2 * 360) - (d1 + (m1 * 30) + (y1 * 360)) in
    float_of_int days /. 360.0
  in
  directed forward

let actual_365 dt1 dt2 = float_of_int (Date.diff dt2 dt1) /. 365.0

let actual_actual_isda =
  let days_in_year y = if Date.is_leap_year y then 366 else 365 in
  let forward dt1 dt2 =
    let y1 = Date.year dt1 in
    let y2 = Date.year dt2 in
    if y1 = y2 then float_of_int (Date.diff dt2 dt1) /. float_of_int (days_in_year y1)
    else
      let jan1_next = Date.make (y1 + 1) 1 1 in
      let first_year = float_of_int (Date.diff jan1_next dt1) /. float_of_int (days_in_year y1) in
      let jan1_last = Date.make y2 1 1 in
      let last_year = float_of_int (Date.diff dt2 jan1_last) /. float_of_int (days_in_year y2) in
      let middle_years = float_of_int (y2 - y1 - 1) in
      first_year +. middle_years +. last_year
  in
  directed forward

(* Continuous month-position difference. Each date maps to a position on the
   month axis: month_index + (day - 1) / days_in_month. The year fraction is
   the position difference divided by 12.

   Sanity checks:
   - Jan 1 -> Feb 1 = exactly 1/12
   - Jan 1 -> Jan 1 next year = exactly 1.0
   - Partial months prorated by actual days in that month *)
let calendar_monthly =
  let month_pos dt =
    let y = Date.year dt in
    let m = Date.month dt in
    let d = Date.day dt in
    let dim = Date.days_in_month dt in
    float_of_int ((y * 12) + m - 1) +. (float_of_int (d - 1) /. float_of_int dim)
  in
  let forward dt1 dt2 = (month_pos dt2 -. month_pos dt1) /. 12.0 in
  directed forward
