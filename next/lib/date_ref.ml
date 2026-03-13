(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type t =
  | Period_start
  | Period_end
  | Shift of Period.offset * t
  | Adjust of Calendar.bdc * Calendar.t * t

let period_start = Period_start
let period_end = Period_end
let shift offset base = Shift (offset, base)
let adjust bdc cal base = Adjust (bdc, cal, base)

let rec resolve period = function
  | Period_start -> Period.start_date period
  | Period_end -> Period.end_date period
  | Shift (offset, base) -> Period.shift_date offset (resolve period base)
  | Adjust (bdc, cal, base) -> Calendar.adjust bdc cal (resolve period base)

let pp_offset ppf (o : Period.offset) =
  let parts = ref [] in
  if o.years <> 0 then parts := Printf.sprintf "%dy" o.years :: !parts;
  if o.quarters <> 0 then parts := Printf.sprintf "%dq" o.quarters :: !parts;
  if o.months <> 0 then parts := Printf.sprintf "%dm" o.months :: !parts;
  if o.weeks <> 0 then parts := Printf.sprintf "%dw" o.weeks :: !parts;
  if o.days <> 0 then parts := Printf.sprintf "%dd" o.days :: !parts;
  if o.month_end then parts := "ME" :: !parts;
  match !parts with
  | [] -> Format.pp_print_string ppf "0"
  | ps -> Format.pp_print_string ppf (String.concat "" (List.rev ps))

let rec pp ppf = function
  | Period_start -> Format.pp_print_string ppf "period_start"
  | Period_end -> Format.pp_print_string ppf "period_end"
  | Shift (offset, base) -> Format.fprintf ppf "shift(%a, %a)" pp_offset offset pp base
  | Adjust (_bdc, _cal, base) -> Format.fprintf ppf "adjust(..., %a)" pp base
