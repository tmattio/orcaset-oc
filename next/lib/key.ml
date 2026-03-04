(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type 'a t = { name : string; id : int; uid : 'a Type.Id.t }

let next_id =
  let counter = Atomic.make 0 in
  fun () -> Atomic.fetch_and_add counter 1

let make name = { name; id = next_id (); uid = Type.Id.make () }
let name k = k.name
let compare (type a b) (a : a t) (b : b t) = Int.compare a.id b.id
let equal (type a b) (a : a t) (b : b t) = a.id = b.id
let to_string k = k.name
let pp fmt k = Format.pp_print_string fmt k.name

type packed = Key : 'a t -> packed

let pack k = Key k
let uid k = k.uid
