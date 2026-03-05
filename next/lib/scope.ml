(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

type binding = Binding : 'a Key.t * 'a -> binding
type t = { mutable entries : binding list; mutable sealed : bool; imports : t list }

let create ?(imports = []) () = { entries = []; sealed = false; imports }

let has_local_key (type a) scope (key : a Key.t) =
  List.exists (fun (Binding (k, _)) -> Key.equal k key) scope.entries

let define (type a) scope (key : a Key.t) (value : a) =
  if scope.sealed then
    invalid_arg (Printf.sprintf "Scope.define: scope is sealed, cannot define %s" (Key.name key));
  if has_local_key scope key then
    invalid_arg (Printf.sprintf "Scope.define: duplicate key %s" (Key.name key));
  scope.entries <- Binding (key, value) :: scope.entries

let find_in_entries : type a. binding list -> a Key.t -> a option =
 fun entries key ->
  let uid_key : a Type.Id.t = Key.uid key in
  let rec loop = function
    | [] -> None
    | Binding (k, v) :: rest -> (
        match Type.Id.provably_equal (Key.uid k) uid_key with
        | Some Type.Equal -> Some (v : a)
        | None -> loop rest)
  in
  loop entries

let rec find_opt : type a. t -> a Key.t -> a option =
 fun scope key ->
  match find_in_entries scope.entries key with
  | Some _ as r -> r
  | None -> (
      let found = List.filter_map (fun imp -> find_opt imp key) scope.imports in
      match found with
      | [ v ] -> Some v
      | [] -> None
      | _ ->
          invalid_arg
            (Printf.sprintf "Scope.find: key %s is ambiguous across imports" (Key.name key)))

let seal scope =
  (* Validate: all imported scopes must be sealed *)
  List.iter
    (fun imp -> if not imp.sealed then invalid_arg "Scope.seal: imported scope is not sealed")
    scope.imports;
  (* Validate: no key is ambiguous across the import graph.
     For each key reachable from any import, try find_opt on the
     imports to detect ambiguity eagerly. *)
  let rec collect_keys s =
    let local = List.rev_map (fun (Binding (k, _)) -> Key.pack k) s.entries in
    let imported = List.concat_map collect_keys s.imports in
    local @ imported
  in
  let all_keys = List.concat_map collect_keys scope.imports in
  List.iter
    (fun (Key.Key k) ->
      let found = List.filter_map (fun imp -> find_opt imp k) scope.imports in
      if List.length found > 1 then
        invalid_arg (Printf.sprintf "Scope.seal: key %s is ambiguous across imports" (Key.name k)))
    all_keys;
  scope.sealed <- true

let find : type a. t -> a Key.t -> a =
 fun scope key ->
  match find_opt scope key with
  | Some v -> v
  | None -> invalid_arg (Printf.sprintf "Scope.find: key %s not found" (Key.name key))

let mem scope key = Option.is_some (find_opt scope key)
let mem_local scope key = has_local_key scope key

let find_local : type a. t -> a Key.t -> a option =
 fun scope key -> find_in_entries scope.entries key

let imports scope = scope.imports
let keys scope = List.rev_map (fun (Binding (k, _)) -> Key.pack k) scope.entries
let size scope = List.length scope.entries
let is_sealed scope = scope.sealed
