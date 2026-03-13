(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Dates relative to the current evaluation period.

    A {!type-t} is a small inert expression that, given a {!Period.t}, resolves to a concrete
    {!Date.t}. Use it to express date-based queries that shift with the evaluation grid:

    {[
      let trailing_3m_start =
        Date_ref.shift (Period.make_offset ~months:(-3) ()) Date_ref.period_start
    ]}

    {1 Date references} *)

type t
(** The type for relative date expressions. *)

val period_start : t
(** [period_start] resolves to the start date of the evaluation period. *)

val period_end : t
(** [period_end] resolves to the end date of the evaluation period. *)

val shift : Period.offset -> t -> t
(** [shift offset base] resolves [base] and then applies [offset]. *)

val adjust : Calendar.bdc -> Calendar.t -> t -> t
(** [adjust bdc cal base] resolves [base] and then applies the business day convention [bdc] under
    calendar [cal]. *)

val resolve : Period.t -> t -> Date.t
(** [resolve period ref] collapses [ref] to a concrete date using [period]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf ref] formats [ref] for diagnostics. *)
