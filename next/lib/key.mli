(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Typed unique line-item identifiers.

    A {!type-t} is a typed identity for a model line item. The type parameter ['a] encodes what type
    of value this key maps to in a {!Scope}. Two keys with the same display name but created by
    different {!make} calls are always distinct.

    {[
      let revenue : [`USD] Flow.t Key.t = Key.make "Revenue"
      let cash    : [`USD] Balance.t Key.t = Key.make "Cash Balance"
    ]}

    {1:keys Keys} *)

type 'a t
(** The type for line-item keys. ['a] is the type of the associated value in any {!Scope}. *)

val make : string -> 'a t
(** [make name] is a fresh key with display name [name]. The type ['a] is determined by context.
    Each call produces a distinct key; two calls with the same [name] are never equal. *)

val name : 'a t -> string
(** [name k] is the display name of [k]. *)

val equal : 'a t -> 'b t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same key (same {!make} call). Keys of different
    types are never equal. *)

val compare : 'a t -> 'b t -> int
(** [compare] is a total order on keys consistent with creation order. *)

val to_string : 'a t -> string
(** [to_string k] is [name k]. *)

val pp : Format.formatter -> 'a t -> unit
(** [pp] formats a key with its display name. *)

(** {1:packing Existential packing}

    Occasionally you need a collection of keys with different types — for example when enumerating
    all entries in a scope. {!pack} erases the type parameter. *)

type packed = Key : 'a t -> packed
(** A key with its type erased. Pattern-match to recover the key; the type ['a] is not recoverable
    without a {!Scope} lookup. *)

val pack : 'a t -> packed
(** [pack k] erases the type of [k]. *)

(**/**)

val uid : 'a t -> 'a Type.Id.t
