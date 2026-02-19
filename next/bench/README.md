# Benchmarks

Run with `dune exec next/bench/main.exe`.

50 samples per benchmark, GC compaction between each sample. Measured on Apple M-series.

## Results vs v1

Peak: **26x** on full CRE proforma (360 periods), **15x** on add_months.

Geometric mean on comparable benchmarks: **3.9x faster**.

No performance regressions.

| Benchmark          | v1       | v2       | speedup |
| ------------------ | -------- | -------- | ------- |
| **Date**           |          |          |         |
| make               | 6.7 us   | 2.9 us   | 2.3x    |
| diff               | 21.9 us  | 21.0 us  | 1.0x    |
| add_days           | 465.8 us | 94.7 us  | 4.9x    |
| add_months         | 126.2 us | 8.2 us   | 15x     |
| **Period gen** [1] |          |          |         |
| 12                 | n/a      | 343 ns   |         |
| 120                | n/a      | 1.9 us   |         |
| 360                | n/a      | 10.1 us  |         |
| **Day count**      |          |          |         |
| actual_360         | 361.6 us | 362.5 us | 1.0x    |
| thirty_360         | 4.70 ms  | 1.34 ms  | 3.5x    |
| calendar_monthly   | 6.16 ms  | 1.79 ms  | 3.4x    |
| **Growth**         |          |          |         |
| 12                 | 3.2 us   | 968 ns   | 3.3x    |
| 120                | 33.2 us  | 8.2 us   | 4.0x    |
| 360                | 134.9 us | 28.3 us  | 4.8x    |
| **Compose** [2]    |          |          |         |
| 2 series           | n/a      | 2.7 us   |         |
| 5 series           | n/a      | 2.6 us   |         |
| 10 series          | n/a      | 2.8 us   |         |
| 20 series          | n/a      | 2.9 us   |         |
| **Accumulation**   |          |          |         |
| 12                 | 3.9 us   | 1.1 us   | 3.5x    |
| 120                | 38.3 us  | 8.6 us   | 4.5x    |
| 360                | 115.7 us | 30.9 us  | 3.7x    |
| **Balance query**  |          |          |         |
| 120                | 152.9 us | 22.8 us  | 6.7x    |
| **Loan**           |          |          |         |
| 12                 | 6.6 us   | 4.8 us   | 1.4x    |
| 120                | 101.9 us | 48.1 us  | 2.1x    |
| 360                | 612.2 us | 142.6 us | 4.3x    |
| **CRE proforma**   |          |          |         |
| 12                 | 77.1 us  | 29.5 us  | 2.6x    |
| 120                | 3.21 ms  | 292.2 us | 11x     |
| 360                | 25.29 ms | 960.6 us | **26x** |
| **Scale**          |          |          |         |
| 12                 | 3.3 us   | 963 ns   | 3.4x    |
| 120                | 32.6 us  | 8.1 us   | 4.0x    |
| 360                | 130.6 us | 27.9 us  | 4.7x    |
| 1200               | 426.6 us | 88.8 us  | 4.8x    |
| 3600               | 1.30 ms  | 262.5 us | 5.0x    |

**[1] Period gen** - no v1 equivalent. v1 is stream-oriented (`Seq.t`) and never materializes all periods into a finite array. `Timeline.monthly` is a v2 concept.

**[2] Compose** - no v1 equivalent. v1 only has binary `sum_seq`; summing N series requires chaining N-1 `Seq.map2` closures (O(N) per element). v2's `Series.sum` takes a list and builds a single DAG node (O(1) per element). A v1 user would compute values independently and sum floats.
