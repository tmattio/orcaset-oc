(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type roll = Same_day | End_of_month | Day_of_month of int
type stub = Short_first | Short_last | Long_first | Long_last
type t = { unadjusted : Date.t array; adjusted : Date.t array }

let snap_eom d = Date.make (Date.year d) (Date.month d) (Date.days_in_month d)

let apply_roll roll d =
  match roll with
  | Same_day -> d
  | End_of_month -> snap_eom d
  | Day_of_month target ->
      Date.make (Date.year d) (Date.month d) (Int.min target (Date.days_in_month d))

(* Compute the kth date from anchor by applying k * offset directly,
   avoiding day-of-month drift (Jan 31 + 2m = Mar 31, not Mar 28). *)
let gen_date (offset : Period.offset) roll anchor k =
  let total_months = k * (offset.months + (offset.quarters * 3) + (offset.years * 12)) in
  let total_days = k * (offset.days + (offset.weeks * 7)) in
  let d = Date.add_months anchor total_months in
  let d = if total_days = 0 then d else Date.add_days d total_days in
  let d = if offset.month_end then snap_eom d else d in
  apply_roll roll d

(* Returns (dates, has_stub) where has_stub is true when the first period
   does not align with the natural grid (start_date differs from the
   generated grid point). *)
let gen_backward offset roll ~start_date ~end_date =
  let rec collect k acc =
    let d = gen_date offset roll end_date (-k) in
    if Date.(d <= start_date) then (start_date :: acc, Date.(d < start_date))
    else collect (k + 1) (d :: acc)
  in
  collect 1 [ end_date ]

(* Returns (dates, has_stub) where has_stub is true when the last period
   does not align with the natural grid. *)
let gen_forward offset roll ~start_date ~end_date =
  let rec collect k acc =
    let d = gen_date offset roll start_date k in
    if Date.(d >= end_date) then (List.rev (end_date :: acc), Date.(d > end_date))
    else collect (k + 1) (d :: acc)
  in
  collect 1 [ start_date ]

let merge_first = function
  | a :: _ :: rest when rest <> [] -> a :: rest
  | dates -> dates

let merge_last dates =
  let arr = Array.of_list dates in
  let n = Array.length arr in
  if n < 3 then dates
  else
    Array.to_list (Array.init (n - 1) (fun i ->
        if i < n - 2 then arr.(i) else arr.(n - 1)))

(* Construction *)

let make ~start_date ~end_date ~offset ?(roll = Same_day) ?(stub = Short_first)
    ?(bdc = Calendar.Unadjusted) ?(calendar = Calendar.weekdays) () =
  if Date.(end_date <= start_date) then
    invalid_arg "Schedule.make: end_date must be after start_date";
  let total_months =
    offset.Period.months + (offset.quarters * 3) + (offset.years * 12)
  in
  let total_days = offset.days + (offset.weeks * 7) in
  if total_months <= 0 && total_days <= 0 then
    invalid_arg "Schedule.make: offset must advance by at least one day or month";
  let dates, has_stub =
    match stub with
    | Short_first | Long_first -> gen_backward offset roll ~start_date ~end_date
    | Short_last | Long_last -> gen_forward offset roll ~start_date ~end_date
  in
  let unadjusted_list =
    match stub with
    | Short_first | Short_last -> dates
    | Long_first -> if has_stub then merge_first dates else dates
    | Long_last -> if has_stub then merge_last dates else dates
  in
  let unadjusted = Array.of_list unadjusted_list in
  let n = Array.length unadjusted in
  if n < 2 then invalid_arg "Schedule.make: offset produces no periods";
  let adjusted =
    Array.init n (fun i ->
        if i = 0 || i = n - 1 then unadjusted.(i) else Calendar.adjust bdc calendar unadjusted.(i))
  in
  { unadjusted; adjusted }

(* Accessors *)

let length s = Array.length s.adjusted - 1
let dates s = Array.copy s.adjusted
let unadjusted_dates s = Array.copy s.unadjusted

let make_periods arr =
  Array.init (Array.length arr - 1) (fun i -> Period.make ~start_date:arr.(i) ~end_date:arr.(i + 1))

let periods s = make_periods s.adjusted
let unadjusted_periods s = make_periods s.unadjusted

(* Conversion *)

let to_timeline s = Timeline.of_periods (periods s)
let to_unadjusted_timeline s = Timeline.of_periods (unadjusted_periods s)

let to_events ?(at = `Start) f s =
  let ps = periods s in
  List.init (Array.length ps) (fun i ->
    let d = match at with `Start -> Period.start_date ps.(i) | `End -> Period.end_date ps.(i) in
    (d, f i ps.(i)))

(* Formatting *)

let to_string s =
  let n = length s in
  Printf.sprintf "%s..%s (%d period%s)"
    (Date.to_string s.adjusted.(0))
    (Date.to_string s.adjusted.(Array.length s.adjusted - 1))
    n
    (if n = 1 then "" else "s")

let pp fmt s = Format.pp_print_string fmt (to_string s)
