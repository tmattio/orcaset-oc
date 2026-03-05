(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type t = { periods : Period.t array }

let make ~start_date ~offset ~n =
  if n <= 0 then invalid_arg "Timeline.make: n must be positive";
  let dummy = Period.make ~start_date ~end_date:start_date in
  let periods = Array.make n dummy in
  let cur_start = ref start_date in
  for i = 0 to n - 1 do
    let next = Period.shift_date offset !cur_start in
    periods.(i) <- Period.make ~start_date:!cur_start ~end_date:next;
    cur_start := next
  done;
  { periods }

let monthly ~start_date ~n = make ~start_date ~offset:(Period.make_offset ~months:1 ()) ~n
let quarterly ~start_date ~n = make ~start_date ~offset:(Period.make_offset ~quarters:1 ()) ~n
let yearly ~start_date ~n = make ~start_date ~offset:(Period.make_offset ~years:1 ()) ~n

let validate_periods periods =
  let n = Array.length periods in
  for i = 1 to n - 1 do
    let prev_end = Period.end_date periods.(i - 1) in
    let cur_start = Period.start_date periods.(i) in
    if Date.(cur_start < prev_end) then
      invalid_arg (Printf.sprintf "Timeline.of_periods: period %d overlaps period %d" i (i - 1));
    if Date.(cur_start > prev_end) then
      invalid_arg (Printf.sprintf "Timeline.of_periods: gap between period %d and %d" (i - 1) i)
  done

let of_periods periods =
  if Array.length periods = 0 then invalid_arg "Timeline.of_periods: empty array";
  validate_periods periods;
  { periods = Array.copy periods }

let unsafe_of_periods periods =
  if Array.length periods = 0 then invalid_arg "Timeline.unsafe_of_periods: empty array";
  { periods = Array.copy periods }

let length tl = Array.length tl.periods
let get tl i = tl.periods.(i)
let period_start tl i = Period.start_date tl.periods.(i)
let period_end tl i = Period.end_date tl.periods.(i)
let start_date tl = Period.start_date tl.periods.(0)
let end_date tl = Period.end_date tl.periods.(Array.length tl.periods - 1)

(* Binary search over the sorted period array.
   Periods use half-open intervals [start, end) so that contiguous periods
   tile without overlap. The last period is special-cased to be fully closed
   [start, end] -- otherwise a date equal to the timeline's final end_date
   would fall outside every period, which is unintuitive for users querying
   the boundary of their model. *)
let find_index tl date =
  let n = Array.length tl.periods in
  let rec search lo hi =
    if lo > hi then None
    else
      let mid = lo + ((hi - lo) / 2) in
      let p = tl.periods.(mid) in
      let cmp_start = Date.compare date (Period.start_date p) in
      let cmp_end = Date.compare date (Period.end_date p) in
      if cmp_start >= 0 && cmp_end < 0 then Some mid
      else if mid = n - 1 && cmp_start >= 0 && cmp_end <= 0 then
        Some mid (* last period: end-inclusive *)
      else if cmp_start < 0 then search lo (mid - 1)
      else search (mid + 1) hi
  in
  search 0 (n - 1)

(* Formatting *)

let to_string tl =
  Printf.sprintf "%s..%s (%d periods)"
    (Date.to_string (start_date tl))
    (Date.to_string (end_date tl))
    (length tl)

let pp fmt tl = Format.pp_print_string fmt (to_string tl)
