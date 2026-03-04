(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Model-level registries for line-item definitions.

    A scope is a mutable-then-sealed registry that maps {!Key.t} values to definitions. Definitions
    are registered with {!define} and the scope is frozen with {!seal}. After sealing, no new
    definitions can be added. This catches duplicate and late definitions eagerly.

    {[
      let scope = Scope.create () in
      Scope.define scope revenue_key revenue_series;
      Scope.define scope cogs_key cogs_series;
      Scope.seal scope;
      let rev = Scope.find scope revenue_key
    ]}

    {1 Scopes} *)

type 'a t
(** The type for scopes mapping {!Key.t} to ['a]. *)

val create : unit -> 'a t
(** [create ()] is a fresh, unsealed scope. *)

val define : 'a t -> Key.t -> 'a -> unit
(** [define scope key value] registers [value] under [key].

    @raise Invalid_argument if [scope] is sealed or [key] is already defined. *)

val seal : 'a t -> unit
(** [seal scope] freezes [scope]. Subsequent calls to {!define} raise. *)

val find : 'a t -> Key.t -> 'a
(** [find scope key] is the value registered under [key].

    @raise Invalid_argument if [key] is not defined. *)

val find_opt : 'a t -> Key.t -> 'a option
(** [find_opt scope key] is [Some v] if [key] is defined, [None] otherwise. *)

val entries : 'a t -> (Key.t * 'a) list
(** [entries scope] is the list of [(key, value)] pairs in definition order. *)

val keys : 'a t -> Key.t list
(** [keys scope] is the list of keys in definition order. *)

val is_sealed : 'a t -> bool
(** [is_sealed scope] is [true] after {!seal} has been called. *)
