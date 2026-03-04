(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Typed heterogeneous model registries.

    A scope is a mutable-then-sealed registry mapping typed {!Key.t} values to values of the
    corresponding type. A single scope can hold values of different types simultaneously — flows,
    balances, formulas, or any user-defined type.

    {[
      let revenue_key : [`USD] Flow.t Key.t    = Key.make "Revenue"
      let cash_key    : [`USD] Balance.t Key.t = Key.make "Cash"

      let scope = Scope.create () in
      Scope.define scope revenue_key revenue_flow;
      Scope.define scope cash_key    cash_balance;
      Scope.seal scope;

      let rev  : [`USD] Flow.t    = Scope.find scope revenue_key
      let cash : [`USD] Balance.t = Scope.find scope cash_key
    ]}

    {1:scopes Scopes} *)

type t
(** The type for heterogeneous scopes. *)

val create : ?imports:t list -> unit -> t
(** [create ?imports ()] is a fresh, unsealed scope. When [imports] is provided, lookups that fail
    locally fall through to the imported scopes. Imports are searched in order; if a key is found in
    exactly one import (or its transitive imports), it is returned. If a key is found in multiple
    imports, {!find} raises [Invalid_argument] (ambiguity). Shadowing an imported key with a local
    {!define} is permitted. *)

(** {1:mutation Mutation} *)

val define : t -> 'a Key.t -> 'a -> unit
(** [define scope key value] registers [value] under [key] in this scope.

    Raises [Invalid_argument] if [scope] is sealed or [key] is already defined in this scope. *)

val seal : t -> unit
(** [seal scope] freezes [scope]. Subsequent calls to {!define} raise. *)

(** {1:lookup Lookup} *)

val find : t -> 'a Key.t -> 'a
(** [find scope key] is the value registered under [key]. Searches this scope first, then the
    imported scopes transitively.

    Raises [Invalid_argument] if [key] is not defined anywhere in the import chain.
    Raises [Invalid_argument] if [key] is ambiguous across multiple imports. *)

val find_opt : t -> 'a Key.t -> 'a option
(** [find_opt scope key] is [Some v] if [key] is defined, [None] otherwise. Searches the import
    chain.

    Raises [Invalid_argument] if [key] is ambiguous across multiple imports. *)

val find_local : t -> 'a Key.t -> 'a option
(** [find_local scope key] is [Some v] if [key] is defined in this scope's own entries, [None]
    otherwise. Does not search imports. *)

val mem : t -> 'a Key.t -> bool
(** [mem scope key] is [true] if [key] is defined in this scope or any import. *)

val mem_local : t -> 'a Key.t -> bool
(** [mem_local scope key] is [true] only if [key] is defined in this scope's own entries. *)

(** {1:meta Metadata} *)

val is_sealed : t -> bool
(** [is_sealed scope] is [true] after {!seal} has been called. *)

val imports : t -> t list
(** [imports scope] is the list of imported scopes provided at {!create} time. *)

val keys : t -> Key.packed list
(** [keys scope] is the list of type-erased keys defined in this scope, in definition order. Does
    not include imported keys. *)

val size : t -> int
(** [size scope] is the number of entries defined in this scope (not counting parent entries). *)
