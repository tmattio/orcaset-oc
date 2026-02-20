(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type t = Date.t -> bool
type bdc = Unadjusted | Following | Modified_following | Preceding | Modified_preceding

let weekdays d = Date.weekday d <= 5
let rec next_bday cal d = if cal d then d else next_bday cal (Date.add_days d 1)
let rec prev_bday cal d = if cal d then d else prev_bday cal (Date.add_days d (-1))

let modified primary fallback cal d =
  let adj = primary cal d in
  if Date.month adj <> Date.month d then fallback cal d else adj

let adjust bdc cal d =
  match bdc with
  | Unadjusted -> d
  | Following -> next_bday cal d
  | Preceding -> prev_bday cal d
  | Modified_following -> modified next_bday prev_bday cal d
  | Modified_preceding -> modified prev_bday next_bday cal d
