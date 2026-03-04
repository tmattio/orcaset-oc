(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type 'a t = { mutable entries : (Key.t * 'a) list; mutable sealed : bool }

let create () = { entries = []; sealed = false }

let define scope key value =
  if scope.sealed then
    invalid_arg
      (Printf.sprintf "Scope.define: scope is sealed, cannot define %s" (Key.name key));
  if List.exists (fun (k, _) -> Key.equal k key) scope.entries then
    invalid_arg (Printf.sprintf "Scope.define: duplicate key %s" (Key.name key));
  scope.entries <- (key, value) :: scope.entries

let seal scope = scope.sealed <- true

let find scope key =
  match List.find_opt (fun (k, _) -> Key.equal k key) scope.entries with
  | Some (_, v) -> v
  | None -> invalid_arg (Printf.sprintf "Scope.find: key %s not found" (Key.name key))

let find_opt scope key =
  Option.map snd (List.find_opt (fun (k, _) -> Key.equal k key) scope.entries)

let entries scope = List.rev scope.entries
let keys scope = List.rev_map fst scope.entries
let is_sealed scope = scope.sealed
