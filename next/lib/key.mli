(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** {b Internal} -- Typed unique identifiers for model line items.

    Each {!make} call produces a distinct identity backed by a fresh
    {!Stdlib.Type.Id}. Two keys with the same display name are never
    equal. Public API is constrained by [orcaset2.mli]. *)

type 'a t
(** The type for keys. ['a] is the type of the associated value
    in a {!Scope}. *)

val make : string -> 'a t
(** [make name] is a fresh key with display name [name]. Each call
    allocates a new {!Stdlib.Type.Id}, so keys are identity-equal
    only to themselves. *)

val name : 'a t -> string
(** [name k] is [k]'s display name. *)

val equal : 'a t -> 'b t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same key
    (same {!make} call). *)

val compare : 'a t -> 'b t -> int
(** [compare a b] is a total order consistent with creation
    order. *)

val to_string : 'a t -> string
(** [to_string k] is [name k]. *)

val pp : Format.formatter -> 'a t -> unit
(** [pp] formats the display name. *)

type packed = Key : 'a t -> packed
(** A key with its type parameter erased. *)

val pack : 'a t -> packed
(** [pack k] erases the type of [k]. *)

val uid : 'a t -> 'a Type.Id.t
(** [uid k] is the underlying type witness. Used by {!Scope} for
    type-safe heterogeneous lookup via [Type.Id.provably_equal]. *)
