(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type t = { name : string; id : int }

let next_id =
  let counter = Atomic.make 0 in
  fun () -> Atomic.fetch_and_add counter 1

let make name = { name; id = next_id () }
let name k = k.name
let compare a b = Int.compare a.id b.id
let equal a b = a.id = b.id
let to_string k = k.name
let pp fmt k = Format.pp_print_string fmt k.name
