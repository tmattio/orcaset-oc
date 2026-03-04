(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Unique line-item identifiers.

    A {!t} is a unique identity for a line item in a financial model. Two keys created by separate
    calls to {!make} are always distinct, even if they share the same display name. Use keys for
    stable identity in {!Scope} registries, reports, and cross-model references.

    {[
      let revenue = Key.make "Revenue"
      let cogs = Key.make "COGS"
    ]}

    {1 Keys} *)

type t
(** The type for line-item keys. *)

val make : string -> t
(** [make name] is a fresh key with display name [name]. Each call produces a distinct key. *)

val name : t -> string
(** [name k] is the display name of [k]. *)

val compare : t -> t -> int
(** [compare] is a total order on keys consistent with creation order. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same key (same {!make} call). *)

val to_string : t -> string
(** [to_string k] is [name k]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a key with its display name. *)
