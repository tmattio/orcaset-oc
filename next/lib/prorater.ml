(*---------------------------------------------------------------------------
   Copyright (c) 2026 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
 ---------------------------------------------------------------------------*)

type t = start_date:Date.t -> end_date:Date.t -> sub_start:Date.t -> sub_end:Date.t -> float

let clamp_subrange ~start_date ~end_date ~sub_start ~sub_end =
  let sub_start = Date.max start_date sub_start in
  let sub_end = Date.min end_date sub_end in
  (sub_start, sub_end)

let actual_days ~start_date ~end_date ~sub_start ~sub_end =
  let sub_start, sub_end = clamp_subrange ~start_date ~end_date ~sub_start ~sub_end in
  let total_days = Int.max 0 (Date.diff end_date start_date) in
  if total_days = 0 then 0.0
  else
    let sub_days = Int.max 0 (Date.diff sub_end sub_start) in
    float_of_int sub_days /. float_of_int total_days

let by_daycount daycount ~start_date ~end_date ~sub_start ~sub_end =
  let sub_start, sub_end = clamp_subrange ~start_date ~end_date ~sub_start ~sub_end in
  if Date.(sub_end <= sub_start) then 0.0
  else
    let total = daycount start_date end_date in
    if total = 0.0 then 0.0 else daycount sub_start sub_end /. total

let thirty_360_us = by_daycount Daycount.thirty_360_us

let business_days calendar =
  let rec count_business_days acc date end_date =
    if Date.(date >= end_date) then acc
    else
      let acc = if calendar date then acc + 1 else acc in
      count_business_days acc (Date.add_days date 1) end_date
  in
  fun ~start_date ~end_date ~sub_start ~sub_end ->
    let sub_start, sub_end = clamp_subrange ~start_date ~end_date ~sub_start ~sub_end in
    let total_days = count_business_days 0 start_date end_date in
    if total_days = 0 then 0.0
    else
      let sub_days = count_business_days 0 sub_start sub_end in
      float_of_int sub_days /. float_of_int total_days
