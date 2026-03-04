(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(** Hierarchical statement structure.

    A statement is a tree of labeled data, typically {!Formula.t} values, organized into the sections
    and line items of a financial statement (income statements, balance sheets, cash flow
    statements, etc.).

    Leaf nodes ({!Line}) carry a label and a datum. Interior nodes ({!Group}) carry a label, a list
    of children, and an optional total. When the total is omitted, {!auto_total} (called implicitly
    by {!eval}) synthesizes one by summing the direct children.

    {1 Building}

    Construct a statement with {!line} and {!group}:

    {[
      let stmt =
        let open Statement in
        group "Income Statement"
          [
            line "Revenue" revenue;
            group "Operating Expenses" [ line "COGS" cogs; line "Rent" rent ];
            line "Net Income" net_income;
          ]
    ]}

    {1 Evaluating}

    Use {!eval} to materialize a [Formula.t item] into a [float array item] against a {!Timeline.t}.
    All formulas in the tree share a single memoization context via {!Formula.eval_many}.

    {1 Traversing}

    {!fold}, {!iter}, {!map}, and {!lines} traverse the tree for rendering or analysis. *)

(** {1:items Items} *)

(** The type for statement items parameterized by the data type ['a]. *)
type 'a item =
  | Line of { label : string; data : 'a }  (** A leaf node with a label and a datum. *)
  | Group of { label : string; items : 'a item list; total : 'a option }
      (** An interior node with a label, children, and an optional aggregate. *)

(** {1:constructors Constructors} *)

val line : string -> 'a -> 'a item
(** [line label data] is [Line {label; data}]. *)

val group : ?total:'a -> string -> 'a item list -> 'a item
(** [group ?total label items] is [Group {label; items; total}].

    When [total] is omitted, {!eval} and {!auto_total} will synthesize one by summing the direct
    children's data. Supply an explicit [total] to override this with a custom calculation. *)

val flow_line : string -> 'c Flow.t -> 'c Formula.t item
(** [flow_line label f] is [line label (Flow.unsafe_to_formula f)]. Convenience for adding a flow
    to a statement without manually extracting the formula.

    {b Note.} The flow/balance distinction is erased: the statement tree holds {!Formula.t} values.
    This means {!auto_total} sums all children uniformly. Mixing flows and balances in the same
    group is permitted but the total loses semantic meaning. *)

val balance_line : string -> 'c Balance.t -> 'c Formula.t item
(** [balance_line label b] is [line label (Balance.unsafe_to_formula b)]. Convenience for adding a
    balance to a statement without manually extracting the formula.

    See the note on {!flow_line} about flow/balance erasure. *)

val flow_group : ?total:'c Flow.t -> string -> 'c Formula.t item list -> 'c Formula.t item
(** [flow_group ?total label items] is
    [group ?total:(Option.map Flow.unsafe_to_formula total) label items]. Convenience for adding a
    group with a flow total without manually extracting the formula.

    See the note on {!flow_line} about flow/balance erasure. *)

val balance_group :
  ?total:'c Balance.t -> string -> 'c Formula.t item list -> 'c Formula.t item
(** [balance_group ?total label items] is
    [group ?total:(Option.map Balance.unsafe_to_formula total) label items]. Convenience for adding
    a group with a balance total without manually extracting the formula.

    See the note on {!flow_line} about flow/balance erasure. *)

(** {1:traversal Traversal} *)

val fold :
  line_fn:(string -> 'a -> 'b) -> group_fn:(string -> 'b list -> 'a option -> 'b) -> 'a item -> 'b
(** [fold ~line_fn ~group_fn item] reduces [item] bottom-up.

    For a {!Line}, calls [line_fn label data]. For a {!Group}, first folds each child recursively,
    then calls [group_fn label results total] where [results] is the list of folded children and
    [total] is the group's own total (if any). *)

val iter :
  line_fn:(string -> 'a -> unit) ->
  group_fn:(string -> 'a option -> [> `Enter | `Exit ] -> unit) ->
  'a item ->
  unit
(** [iter ~line_fn ~group_fn item] traverses [item] depth-first.

    For a {!Line}, calls [line_fn label data]. For a {!Group}, calls [group_fn label total `Enter]
    before visiting children and [group_fn label total `Exit] after. This bracketed traversal is
    well-suited to rendering with indentation or section delimiters. *)

val map : ('a -> 'b) -> 'a item -> 'b item
(** [map f item] applies [f] to every datum in [item], preserving the tree structure. Group totals,
    when present, are also mapped. *)

val lines : 'a item -> (string * 'a) list
(** [lines item] collects all leaf [(label, data)] pairs in depth-first, left-to-right order. Group
    totals are not included. *)

(** {1:evaluation Evaluation} *)

val auto_total : 'c Formula.t item -> 'c Formula.t item
(** [auto_total item] fills in missing group totals. For each {!Group} without an explicit total,
    synthesizes one by summing the data of its direct children: each child {!Line} contributes its
    data, and each child {!Group} contributes its total (if any). Children with no extractable data
    are skipped. Groups that already have a total are unchanged. Operates recursively, bottom-up.

    {!eval} calls this automatically before materializing. Call [auto_total] directly only to
    inspect the formula tree prior to evaluation. *)

val eval : Timeline.t -> 'c Formula.t item -> float array item
(** [eval tl item] materializes every formula in [item] against [tl].

    Applies {!auto_total} first, then evaluates all formulas in a single shared memoization context
    via {!Formula.eval_many}. The result is a structurally identical tree with [float array] data.

    Raises [Formula.Cycle_error] if a same-period cycle is detected. *)

val eval_materialized : Timeline.t -> 'c Formula.t item -> 'c Formula.Materialized.t item
(** [eval_materialized tl item] is like {!eval} but returns {!Formula.Materialized.t} values that
    keep period bindings attached to each result array. Applies {!auto_total} first and shares a
    single memoization context.

    Raises [Formula.Cycle_error] if a same-period cycle is detected. *)

(** {1:pp Pretty-printing} *)

type layout
(** A column layout for tabular output. Captures the timeline dimensions, column widths, header
    labels, number formatter, and separator character. Create with {!layout} and pass to {!pp} or
    {!pp_row}. *)

val layout :
  ?label_width:int ->
  ?col_width:int ->
  ?col_header:(int -> string) ->
  ?pp_num:(Format.formatter -> float -> unit) ->
  ?sep:char ->
  Timeline.t ->
  layout
(** [layout tl] derives a column layout from [tl].

    @param label_width Width of the label column. Default [25].
    @param col_width Width of each data column. Default [10].
    @param col_header
      Maps period index to column header string. Defaults to three-letter month abbreviation of each
      period's start date.
    @param pp_num
      Formats individual float values into a column. Default is right-aligned integers (no decimal
      places) padded to [col_width].
    @param sep Character used for separator lines between groups. Default ['-']. *)

val pp : layout -> Format.formatter -> float array item -> unit
(** [pp l ppf item] pretty-prints [item] as a table. Outputs column headers, a separator, then the
    statement tree: group labels on their own line, indented child lines, ["Total"] rows for groups,
    and separators between groups.

    {[
      let results = Statement.eval tl income_statement in
      Statement.pp (Statement.layout tl) Format.std_formatter results
    ]} *)

val pp_row : layout -> Format.formatter -> string -> float array -> unit
(** [pp_row l ppf label values] prints a single data row aligned with [l]. Use for additional rows
    outside the statement tree (e.g., a cash balance line).

    {[
      Statement.pp_row l Format.std_formatter "Cash Balance" cash_values
    ]} *)
