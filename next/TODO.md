# TODO

## Fixpoint invalidation performance

`invalidate_period` walks into all dependencies of the fixpoint body,
including exogenous series that don't depend on the iteration variable.
This causes redundant recomputation during fixpoint iteration. Not a
correctness issue (results are correct), but wasteful for deep graphs.

Fix: use a nested evaluation context for the fixpoint loop, or tag nodes
that transitively depend on the `Var` so only those are invalidated.

Ref: `series.ml`, `invalidate_period`.

## Business day calendars

`Period.offset` handles Y/M/D but has no concept of holiday calendars or
business day conventions (Following, Modified Following, Preceding). When
a period boundary falls on a weekend or holiday, financial models need to
roll the date according to a convention.

Requires: a `Calendar.t` type (set of holiday dates) and a
`roll_convention` argument for date shifting functions.

## Compilation step for evaluation

The current evaluator uses a `Hashtbl`-based memo table driven by
recursive DAG traversal. For very large models (hundreds of series, 360+
periods), a compilation step would improve performance:

1. Topological sort the DAG once per timeline.
2. Allocate a flat `float array` of size `nodes * periods`.
3. Execute sorted nodes in a tight loop over periods.

This eliminates hash table overhead, improves cache locality, and moves
cycle detection to compile time. Not urgent until benchmarks show the
evaluator is a bottleneck.
