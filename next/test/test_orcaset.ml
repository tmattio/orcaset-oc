open Orcaset2
open Alcotest

(* Helpers *)

let date = Date.make
let eps = 1e-9
let fl msg exp act = check (float eps) msg exp act

let fla msg exp act =
  check int (msg ^ " length") (Array.length exp) (Array.length act);
  Array.iteri (fun i e -> fl (Printf.sprintf "%s[%d]" msg i) e act.(i)) exp

let ev msg tl s exp = fla msg exp (Formula.eval tl s)
let ds msg exp d = check string msg exp (Date.to_string d)
let raises msg exn f = check_raises msg exn (fun () -> ignore (f ()))
let invalid msg f = raises msg (Invalid_argument msg) f

let to_s f =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  f ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let has sub s =
  let slen = String.length sub and hlen = String.length s in
  if slen > hlen then false
  else
    let rec loop i =
      if i > hlen - slen then false else if String.sub s i slen = sub then true else loop (i + 1)
    in
    loop 0

let tl3 = Timeline.monthly ~start_date:(date 2025 1 1) ~n:3

(* Date *)

let test_date () =
  (* make + accessors *)
  let d = date 2025 3 15 in
  check int "year" 2025 (Date.year d);
  check int "month" 3 (Date.month d);
  check int "day" 15 (Date.day d);
  (* validation *)
  invalid "Date.make: invalid month 0" (fun () -> date 2025 0 1);
  invalid "Date.make: invalid month 13" (fun () -> date 2025 13 1);
  invalid "Date.make: invalid day 32 for 2025-01" (fun () -> date 2025 1 32);
  invalid "Date.make: invalid day 29 for 2025-02" (fun () -> date 2025 2 29);
  (* leap year *)
  check bool "2024 leap" true (Date.is_leap_year 2024);
  check bool "2025 not" false (Date.is_leap_year 2025);
  check bool "2000 leap" true (Date.is_leap_year 2000);
  check bool "1900 not" false (Date.is_leap_year 1900);
  check int "feb 29 leap" 29 (Date.day (date 2024 2 29));
  (* compare *)
  let d1 = date 2025 1 1 and d2 = date 2025 1 2 and d3 = date 2025 2 1 in
  check bool "d1<d2" true Date.(d1 < d2);
  check bool "d1=d1" true (Date.equal d1 d1);
  check bool "d1<d3" true Date.(d1 < d3);
  (* diff *)
  check int "same" 0 (Date.diff (date 2025 1 1) (date 2025 1 1));
  check int "1d" 1 (Date.diff (date 2025 1 2) (date 2025 1 1));
  check int "-1d" (-1) (Date.diff (date 2025 1 1) (date 2025 1 2));
  check int "365d" 365 (Date.diff (date 2026 1 1) (date 2025 1 1));
  check int "leap 366" 366 (Date.diff (date 2025 1 1) (date 2024 1 1));
  (* add_days *)
  ds "jan30+2" "2025-02-01" (Date.add_days (date 2025 1 30) 2);
  ds "mar1-1" "2025-02-28" (Date.add_days (date 2025 3 1) (-1));
  (* add_months: clamping, leap year, cross-year *)
  ds "jan31+1m" "2025-02-28" (Date.add_months (date 2025 1 31) 1);
  ds "jan31+1m leap" "2024-02-29" (Date.add_months (date 2024 1 31) 1);
  ds "jan15+12m" "2026-01-15" (Date.add_months (date 2025 1 15) 12);
  ds "mar15-2m" "2025-01-15" (Date.add_months (date 2025 3 15) (-2));
  ds "mar15-25m" "2023-02-15" (Date.add_months (date 2025 3 15) (-25));
  (* days_in_month *)
  check int "jan" 31 (Date.days_in_month (date 2025 1 1));
  check int "feb" 28 (Date.days_in_month (date 2025 2 1));
  check int "feb leap" 29 (Date.days_in_month (date 2024 2 1));
  check int "apr" 30 (Date.days_in_month (date 2025 4 1));
  (* to_string *)
  ds "fmt" "2025-01-01" (date 2025 1 1);
  (* min/max *)
  let lo = date 2025 1 1 and hi = date 2025 6 15 in
  check bool "min" true (Date.equal lo (Date.min lo hi));
  check bool "max" true (Date.equal hi (Date.max lo hi));
  (* roundtrip: make -> add_days 0 -> to_string *)
  List.iter
    (fun (y, m, d) ->
      let orig = date y m d in
      ds (Printf.sprintf "rt %04d-%02d-%02d" y m d) (Date.to_string orig) (Date.add_days orig 0))
    [ (2025, 1, 1); (2024, 2, 29); (2000, 1, 1); (1, 1, 1); (2100, 6, 15) ];
  (* add_days roundtrip: add n then diff = n *)
  let d = date 2025 6 15 in
  List.iter
    (fun n -> check int (Printf.sprintf "add %d" n) n (Date.diff (Date.add_days d n) d))
    [ 0; 1; -1; 31; -31; 365; -365; 1000; -1000 ]

(* Period *)

let test_period () =
  let p = Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  check int "days" 31 (Period.days p);
  (* half-open: [start, end) *)
  check bool "start in" true (Period.contains p (date 2025 1 1));
  check bool "end out" false (Period.contains p (date 2025 2 1));
  check bool "last day" true (Period.contains p (date 2025 1 31));
  check bool "before" false (Period.contains p (date 2024 12 31));
  (* make_seq monthly: contiguous tiling *)
  let ps =
    Period.make_seq ~start_date:(date 2025 1 1) ~offset:(Period.make_offset ~months:1 ())
    |> Seq.take 3 |> Array.of_seq
  in
  ds "p0 start" "2025-01-01" (Period.start_date ps.(0));
  ds "p0 end" "2025-02-01" (Period.end_date ps.(0));
  ds "p1 start" "2025-02-01" (Period.start_date ps.(1));
  ds "p2 start" "2025-03-01" (Period.start_date ps.(2));
  (* make_seq quarterly *)
  let qs =
    Period.make_seq ~start_date:(date 2025 1 1) ~offset:(Period.make_offset ~quarters:1 ())
    |> Seq.take 4 |> Array.of_seq
  in
  ds "q1 end" "2025-04-01" (Period.end_date qs.(0));
  ds "q4 end" "2026-01-01" (Period.end_date qs.(3))

(* Timeline *)

let test_timeline () =
  let tl = Timeline.monthly ~start_date:(date 2025 1 1) ~n:12 in
  check int "len" 12 (Timeline.length tl);
  ds "start" "2025-01-01" (Timeline.start_date tl);
  ds "end" "2026-01-01" (Timeline.end_date tl);
  ds "p0 start" "2025-01-01" (Timeline.period_start tl 0);
  ds "p0 end" "2025-02-01" (Timeline.period_end tl 0);
  (* convenience constructors *)
  let qtl = Timeline.quarterly ~start_date:(date 2025 1 1) ~n:4 in
  check int "quarterly" 4 (Timeline.length qtl);
  ds "q1 end" "2025-04-01" (Timeline.period_end qtl 0);
  let ytl = Timeline.yearly ~start_date:(date 2025 1 1) ~n:2 in
  check int "yearly" 2 (Timeline.length ytl);
  ds "y1 end" "2026-01-01" (Timeline.period_end ytl 0);
  (* find_index: half-open + last period end-inclusive *)
  let oi = check (option int) in
  oi "jan 15" (Some 0) (Timeline.find_index tl (date 2025 1 15));
  oi "feb 1" (Some 1) (Timeline.find_index tl (date 2025 2 1));
  oi "dec 31" (Some 11) (Timeline.find_index tl (date 2025 12 31));
  oi "end incl" (Some 11) (Timeline.find_index tl (date 2026 1 1));
  oi "before" None (Timeline.find_index tl (date 2024 12 31));
  oi "after" None (Timeline.find_index tl (date 2026 1 2));
  (* invalid *)
  invalid "Timeline.make: n must be positive" (fun () ->
      Timeline.monthly ~start_date:(date 2025 1 1) ~n:0);
  (* of_periods: overlapping rejected *)
  invalid "Timeline.of_periods: period 1 overlaps period 0" (fun () ->
      Timeline.of_periods
        [|
          Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 15);
          Period.make ~start_date:(date 2025 2 1) ~end_date:(date 2025 3 1);
        |]);
  (* of_periods: gap rejected *)
  invalid "Timeline.of_periods: gap between period 0 and 1" (fun () ->
      Timeline.of_periods
        [|
          Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1);
          Period.make ~start_date:(date 2025 2 5) ~end_date:(date 2025 3 1);
        |]);
  (* of_periods_unsafe: accepts anything *)
  let _tl_unsafe =
    Timeline.of_periods_unsafe
      [|
        Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 15);
        Period.make ~start_date:(date 2025 2 1) ~end_date:(date 2025 3 1);
      |]
  in
  ()

(* Daycount *)

let test_daycount () =
  (* actual/360 *)
  fl "a360 half" (181.0 /. 360.0) (Daycount.actual_360 (date 2025 1 1) (date 2025 7 1));
  (* 30/360 *)
  fl "30/360" (28.0 /. 360.0) (Daycount.thirty_360_us (date 2025 1 30) (date 2025 2 28));
  (* 30/360 symmetry *)
  let yf = Daycount.thirty_360_us (date 2025 1 1) (date 2025 7 1) in
  fl "30/360 neg" (-.yf) (Daycount.thirty_360_us (date 2025 7 1) (date 2025 1 1));
  (* 30/360 edge cases *)
  fl "feb eom+31" (30.0 /. 360.0) (Daycount.thirty_360_us (date 2025 2 28) (date 2025 3 31));
  fl "both feb eom" (360.0 /. 360.0) (Daycount.thirty_360_us (date 2024 2 29) (date 2025 2 28));
  fl "31 to 31" (60.0 /. 360.0) (Daycount.thirty_360_us (date 2025 1 31) (date 2025 3 31));
  (* actual/365 *)
  fl "a365 half" (181.0 /. 365.0) (Daycount.actual_365 (date 2025 1 1) (date 2025 7 1));
  fl "a365 sym" (-.(181.0 /. 365.0)) (Daycount.actual_365 (date 2025 7 1) (date 2025 1 1));
  (* actual/actual ISDA *)
  fl "aa same yr" (181.0 /. 365.0) (Daycount.actual_actual_isda (date 2025 1 1) (date 2025 7 1));
  fl "aa cross yr" 1.0 (Daycount.actual_actual_isda (date 2025 1 1) (date 2026 1 1));
  fl "aa leap"
    ((float_of_int (Date.diff (date 2025 1 1) (date 2024 7 1)) /. 366.0)
    +. (float_of_int (Date.diff (date 2025 7 1) (date 2025 1 1)) /. 365.0))
    (Daycount.actual_actual_isda (date 2024 7 1) (date 2025 7 1));
  fl "aa sym"
    (-.Daycount.actual_actual_isda (date 2024 7 1) (date 2025 7 1))
    (Daycount.actual_actual_isda (date 2025 7 1) (date 2024 7 1));
  (* calendar_monthly *)
  fl "cm 1yr" 1.0 (Daycount.calendar_monthly (date 2025 1 1) (date 2026 1 1));
  fl "cm jan-feb" (1.0 /. 12.0) (Daycount.calendar_monthly (date 2025 1 1) (date 2025 2 1));
  fl "cm half" 0.5 (Daycount.calendar_monthly (date 2025 1 1) (date 2025 7 1));
  fl "cm quarter" 0.25 (Daycount.calendar_monthly (date 2025 1 1) (date 2025 4 1));
  (* calendar_monthly symmetry *)
  let yf = Daycount.calendar_monthly (date 2025 1 1) (date 2025 7 1) in
  fl "cm neg" (-.yf) (Daycount.calendar_monthly (date 2025 7 1) (date 2025 1 1))

(* Formula: constructors *)

let test_formula_constructors () =
  ev "const" tl3 (Formula.const 42.0) [| 42.0; 42.0; 42.0 |];
  ev "of_array" tl3 (Formula.of_array [| 1.0; 2.0 |]) [| 1.0; 2.0; 0.0 |];
  ev "init" tl3 (Formula.init (fun i _p -> float_of_int (i + 1))) [| 1.0; 2.0; 3.0 |];
  ev "init days" tl3
    (Formula.init (fun _i p -> Period.days p |> float_of_int))
    [| 31.0; 28.0; 31.0 |];
  ev "init_flow" tl3
    (Formula.init_flow (fun p -> Period.days p |> float_of_int))
    [| 31.0; 28.0; 31.0 |];
  ev "init_tl" tl3
    (Formula.init_tl (fun tl _i _p -> float_of_int (Timeline.length tl)))
    [| 3.0; 3.0; 3.0 |];
  (* of_events: aggregation, boundary placement, outside-timeline drop *)
  ev "of_events" tl3
    (Formula.of_events [ (date 2025 1 10, 100.0); (date 2025 1 20, 50.0); (date 2025 3 5, 200.0) ])
    [| 150.0; 0.0; 200.0 |];
  ev "boundary" tl3 (Formula.of_events [ (date 2025 2 1, 100.0) ]) [| 0.0; 100.0; 0.0 |];
  ev "outside drop" tl3
    (Formula.of_events [ (date 2024 1 1, 999.0); (date 2025 1 15, 100.0); (date 2026 1 1, 999.0) ])
    [| 100.0; 0.0; 0.0 |];
  (* of_events: same series evaluated against two different timelines *)
  let events = Formula.of_events [ (date 2025 1 15, 100.0); (date 2025 4 10, 200.0) ] in
  let tl_short = Timeline.monthly ~start_date:(date 2025 1 1) ~n:2 in
  let tl_long = Timeline.monthly ~start_date:(date 2025 1 1) ~n:6 in
  ev "cross-tl short" tl_short events [| 100.0; 0.0 |];
  ev "cross-tl long" tl_long events [| 100.0; 0.0; 0.0; 200.0; 0.0; 0.0 |]

(* Formula: pointwise *)

let test_formula_pointwise () =
  let a = Formula.of_array [| 1.0; 5.0; 3.0 |] in
  let b = Formula.of_array [| 2.0; 4.0; 3.0 |] in
  ev "add" tl3 (Formula.add a b) [| 3.0; 9.0; 6.0 |];
  ev "sub" tl3 (Formula.sub a b) [| -1.0; 1.0; 0.0 |];
  ev "mul" tl3 (Formula.mul a b) [| 2.0; 20.0; 9.0 |];
  ev "div" tl3 (Formula.div a b) [| 0.5; 1.25; 1.0 |];
  ev "neg" tl3 (Formula.neg a) [| -1.0; -5.0; -3.0 |];
  ev "abs" tl3 (Formula.abs (Formula.neg a)) [| 1.0; 5.0; 3.0 |];
  ev "min" tl3 (Formula.min a b) [| 1.0; 4.0; 3.0 |];
  ev "max" tl3 (Formula.max a b) [| 2.0; 5.0; 3.0 |];
  ev "scale" tl3 (Formula.scale 2.5 a) [| 2.5; 12.5; 7.5 |];
  ev "clamp" tl3 (Formula.clamp ~lo:2.0 ~hi:4.0 a) [| 2.0; 4.0; 3.0 |];
  ev "round" tl3 (Formula.round 2 (Formula.of_array [| 1.005; 2.555; 3.999 |])) [| 1.0; 2.56; 4.0 |];
  ev "sum" tl3 (Formula.sum [ a; b; Formula.const 10.0 ]) [| 13.0; 19.0; 16.0 |];
  ev "map" tl3 (Formula.map (fun x -> x *. 2.0) (Formula.const 5.0)) [| 10.0; 10.0; 10.0 |];
  ev "map2" tl3 (Formula.map2 ( +. ) a b) [| 3.0; 9.0; 6.0 |];
  (* where: cond <> 0 selects then_, else otherwise *)
  let cond = Formula.of_array [| 1.0; 0.0; 1.0 |] in
  ev "where" tl3 (Formula.where ~cond ~then_:a ~else_:b) [| 1.0; 4.0; 3.0 |];
  (* syntax spot-check *)
  let open Formula.Syntax in
  ev "+" tl3 (a + b) [| 3.0; 9.0; 6.0 |];
  ev "*$" tl3 (2.0 *$ a) [| 2.0; 10.0; 6.0 |]

(* Formula: cross-period *)

let test_formula_cross_period () =
  let flow = Formula.of_array [| 100.0; 200.0; 300.0 |] in
  ev "prev" tl3 (Formula.prev flow ~default:0.0) [| 0.0; 100.0; 200.0 |];
  ev "scan" tl3
    (Formula.scan ~init:1000.0 (fun ~acc ~x -> acc +. x) flow)
    [| 1100.0; 1300.0; 1600.0 |];
  ev "cumsum" tl3 (Formula.cumsum ~init:1000.0 flow) [| 1100.0; 1300.0; 1600.0 |]

(* Formula: feedback *)

let test_formula_feedback () =
  (* counter: simplest self-referential feedback *)
  ev "counter" tl3
    (Formula.feedback ~default:0.0 (fun prev ->
         let s = Formula.map (fun v -> v +. 1.0) prev in
         (s, s)))
    [| 1.0; 2.0; 3.0 |];
  (* doubling *)
  ev "doubling" tl3
    (Formula.feedback ~default:1.0 (fun prev ->
         let s = Formula.map (fun v -> v *. 2.0) prev in
         (s, s)))
    [| 2.0; 4.0; 8.0 |];
  (* mutual recursion: a = 1 + prev(b), b = 2*a *)
  let a, b =
    Formula.feedback ~default:0.0 (fun prev_b ->
        let a = Formula.add (Formula.const 1.0) prev_b in
        let b = Formula.scale 2.0 a in
        (b, (a, b)))
  in
  fla "mutual a" [| 1.0; 3.0; 7.0 |] (Formula.eval tl3 a);
  fla "mutual b" [| 2.0; 6.0; 14.0 |] (Formula.eval tl3 b);
  (* interest accrual: balance depends on interest depends on prev balance *)
  let balance, interest =
    Formula.feedback ~default:100.0 (fun prev_bal ->
        let interest = Formula.map (fun b -> b *. 0.01) prev_bal in
        let balance = Formula.map2 (fun b i -> b +. 10.0 +. i) prev_bal interest in
        (balance, (balance, interest)))
  in
  let vb = Formula.eval tl3 balance in
  let vi = Formula.eval tl3 interest in
  fl "bal[0]" 111.0 vb.(0);
  fl "bal[1]" 122.11 vb.(1);
  fl "int[0]" 1.0 vi.(0);
  fl "int[1]" 1.11 vi.(1)

(* Formula: fixpoint *)

let test_formula_fixpoint () =
  let c = Formula.of_array [| 10.0; 20.0; 30.0 |] in
  (* x = 0.5*(x+c) converges to x=c *)
  ev "linear" tl3
    (Formula.fixpoint ~guess:0.0 (fun x -> Formula.scale 0.5 (Formula.add x c)))
    [| 10.0; 20.0; 30.0 |];
  (* warm start: period 1 starts from period 0's converged value *)
  let c2 = Formula.of_array [| 10.0; 100.0; 100.0 |] in
  ev "warm start" tl3
    (Formula.fixpoint ~guess:0.0 (fun x -> Formula.scale 0.5 (Formula.add x c2)))
    [| 10.0; 100.0; 100.0 |];
  (* fixpoint + feedback: flow converges to prev balance each period *)
  ev "fixpoint+feedback" tl3
    (Formula.feedback ~default:100.0 (fun prev_bal ->
         let flow =
           Formula.fixpoint ~guess:0.0 (fun x -> Formula.scale 0.5 (Formula.add x prev_bal))
         in
         let balance = Formula.cumsum ~init:100.0 flow in
         (balance, balance)))
    [| 200.0; 400.0; 800.0 |];
  (* divergence: x = 2x+1 *)
  raises "diverges"
    (Formula.Convergence_error { formula_name = Some "divergent"; period_index = 0; iterations = 5 })
    (fun () ->
      Formula.eval tl3
        (Formula.fixpoint ~name:"divergent" ~max_iter:5 ~guess:0.0 (fun x ->
             Formula.add (Formula.scale 2.0 x) (Formula.const 1.0))));
  (* LTC construction loan: loan = ltc * (base_cost + loan*rate) *)
  let ltc = 0.8 and rate = 0.05 in
  let base_cost = Formula.of_array [| 1000.0; 2000.0; 3000.0 |] in
  let loan =
    Formula.fixpoint ~guess:0.0 (fun commitment ->
        Formula.scale ltc (Formula.add base_cost (Formula.scale rate commitment)))
  in
  let values = Formula.eval tl3 loan in
  let expected i =
    let bc = [| 1000.0; 2000.0; 3000.0 |].(i) in
    ltc *. bc /. (1.0 -. (ltc *. rate))
  in
  fl "ltc[0]" (expected 0) values.(0);
  fl "ltc[1]" (expected 1) values.(1);
  fl "ltc[2]" (expected 2) values.(2)

(* Formula: growth *)

let test_formula_growth () =
  let sd = date 2025 1 1 in
  (* zero rate: constant *)
  ev "zero rate" tl3
    (Formula.growth_simple ~start_date:sd ~rate:0.0 1000.0)
    [| 1000.0; 1000.0; 1000.0 |];
  (* simple growth: monotonically increasing *)
  let v = Formula.eval tl3 (Formula.growth_simple ~start_date:sd ~rate:1.0 1000.0) in
  fl "simple p0" 1000.0 v.(0);
  check bool "simple grows" true (v.(2) > v.(1));
  (* simple growth with calendar_monthly daycount *)
  let v =
    Formula.eval tl3
      (Formula.growth_simple ~start_date:sd ~rate:1.0 ~daycount:Daycount.calendar_monthly 1200.0)
  in
  fl "cm p0" 1200.0 v.(0);
  let yf1 = Daycount.calendar_monthly sd (date 2025 2 1) in
  fl "cm p1" (1200.0 *. (1.0 +. yf1)) v.(1);
  (* compound growth *)
  let v = Formula.eval tl3 (Formula.growth_compound ~start_date:sd ~rate:0.10 1000.0) in
  fl "compound p0" 1000.0 v.(0);
  let yf2 = Daycount.actual_360 sd (date 2025 3 1) in
  fl "compound p2" (1000.0 *. ((1.0 +. 0.10) ** yf2)) v.(2);
  (* year_frac: matches daycount applied to each period *)
  let v = Formula.eval tl3 (Formula.year_frac Daycount.thirty_360_us) in
  fl "yf jan" (Daycount.thirty_360_us (date 2025 1 1) (date 2025 2 1)) v.(0);
  fl "yf feb" (Daycount.thirty_360_us (date 2025 2 1) (date 2025 3 1)) v.(1)

(* Formula: query *)

let test_formula_query () =
  let tl = Timeline.monthly ~start_date:(date 2025 1 1) ~n:3 in
  let mk_mat arr = Formula.Materialized.make tl arr in
  (* interpolate *)
  fl "interp jan16"
    (310.0 *. 15.0 /. 31.0)
    (Formula.Query.interpolate (mk_mat [| 310.0; 280.0; 310.0 |]) (date 2025 1 16));
  raises "interp outside"
    (Invalid_argument "Formula.Query.interpolate: date 2024-01-01 is outside the timeline")
    (fun () -> Formula.Query.interpolate (mk_mat [| 100.0; 280.0; 310.0 |]) (date 2024 1 1));
  (* accrue *)
  let vals = mk_mat [| 100.0; 200.0; 300.0 |] in
  fl "accrue full" 600.0
    (Formula.Query.accrue vals ~start_date:(date 2025 1 1) ~end_date:(date 2025 4 1));
  fl "accrue feb" 200.0
    (Formula.Query.accrue vals ~start_date:(date 2025 2 1) ~end_date:(date 2025 3 1));
  let tl1 = Timeline.monthly ~start_date:(date 2025 1 1) ~n:1 in
  fl "accrue partial"
    (310.0 *. 15.0 /. 31.0)
    (Formula.Query.accrue
       (Formula.Materialized.make tl1 [| 310.0 |])
       ~start_date:(date 2025 1 1) ~end_date:(date 2025 1 16));
  (* balance_at *)
  let flow = Formula.of_array [| 100.0; 200.0; 300.0 |] in
  let balance = Formula.cumsum ~init:1000.0 flow in
  let fm = Formula.eval_materialized tl flow in
  let bm = Formula.eval_materialized tl balance in
  fl "bal jan1" 1000.0 (Formula.Query.balance_at ~balance:bm ~flow:fm (date 2025 1 1));
  fl "bal feb15"
    (1100.0 +. (200.0 *. 14.0 /. 28.0))
    (Formula.Query.balance_at ~balance:bm ~flow:fm (date 2025 2 15));
  raises "bal outside"
    (Invalid_argument "Formula.Query.balance_at: date 2024-01-01 is outside the timeline")
    (fun () -> Formula.Query.balance_at ~balance:bm ~flow:fm (date 2024 1 1))

(* Materialized *)

let test_materialized () =
  let s = Formula.of_array [| 10.0; 20.0; 30.0 |] in
  let m = Formula.eval_materialized tl3 s in
  check int "length" 3 (Formula.Materialized.length m);
  fl "get 0" 10.0 (Formula.Materialized.get m 0);
  fl "get 2" 30.0 (Formula.Materialized.get m 2);
  ds "period 0 start" "2025-01-01" (Period.start_date (Formula.Materialized.period m 0));
  let pairs = Formula.Materialized.to_list m in
  check int "to_list len" 3 (List.length pairs);
  let sum =
    Formula.Materialized.fold (fun acc _p v -> acc +. v) 0.0 m
  in
  fl "fold sum" 60.0 sum;
  let count = ref 0 in
  Formula.Materialized.iter (fun _p _v -> incr count) m;
  check int "iter count" 3 !count

(* Statement *)

let test_statement () =
  (* map *)
  (match Statement.lines (Statement.map (fun x -> x * 2) (Statement.line "X" 10)) with
  | [ (_, d) ] -> check int "map" 20 d
  | _ -> fail "expected 1 line");
  (* auto_total: sum of children *)
  let expenses =
    Statement.group "Expenses"
      [
        Statement.line "Rent" (Formula.const 2000.0); Statement.line "Salaries" (Formula.const 3000.0);
      ]
  in
  (match
     Statement.fold (Statement.eval tl3 expenses)
       ~line_fn:(fun _ _ -> None)
       ~group_fn:(fun _ _ total -> total)
   with
  | Some arr -> fla "auto total" [| 5000.0; 5000.0; 5000.0 |] arr
  | None -> fail "expected total");
  (* eval preserves structure *)
  let income =
    Statement.group "Income"
      [ Statement.line "Revenue" (Formula.const 1000.0); Statement.line "COGS" (Formula.const 300.0) ]
  in
  check int "lines" 2 (List.length (Statement.lines (Statement.eval tl3 income)));
  (* pp output contains expected labels and numbers *)
  let tl = Timeline.monthly ~start_date:(date 2025 1 1) ~n:3 in
  let rich =
    Statement.group "Income"
      [
        Statement.line "Revenue" (Formula.const 1000.0);
        Statement.group "Costs"
          [ Statement.line "COGS" (Formula.const 300.0); Statement.line "Rent" (Formula.const 200.0) ];
        Statement.line "Net" (Formula.const 500.0);
      ]
  in
  let out = to_s (fun ppf -> Statement.pp (Statement.layout tl) ppf (Statement.eval tl rich)) in
  List.iter
    (fun needle -> check bool needle true (has needle out))
    [ "Revenue"; "Total Costs"; "-----"; "1000"; "300" ];
  (* custom layout *)
  let tl2 = Timeline.monthly ~start_date:(date 2025 1 1) ~n:2 in
  let l =
    Statement.layout ~label_width:30 ~col_width:15
      ~col_header:(fun i -> Printf.sprintf "P%d" i)
      ~sep:'=' tl2
  in
  let out =
    to_s (fun ppf ->
        Statement.pp l ppf (Statement.eval tl2 (Statement.line "Test" (Formula.const 42.0))))
  in
  check bool "custom P0" true (has "P0" out);
  check bool "custom sep" true (has "=====" out);
  (* flow_line / balance_line convenience *)
  let rev = Flow.const 1000.0 in
  let cash = Balance.const 5000.0 in
  let stmt =
    Statement.group "Mixed"
      [ Statement.flow_line "Revenue" rev; Statement.balance_line "Cash" cash ]
  in
  let result = Statement.eval tl3 stmt in
  check int "mixed lines" 2 (List.length (Statement.lines result));
  (* eval_materialized keeps period bindings *)
  let mat_stmt =
    Statement.eval_materialized tl3
      (Statement.group "G"
         [ Statement.line "A" (Formula.const 7.0); Statement.line "B" (Formula.const 3.0) ])
  in
  let mat_lines = Statement.lines mat_stmt in
  List.iter
    (fun (_, m) ->
      check int "mat length" 3 (Formula.Materialized.length m);
      let p0 = Formula.Materialized.period m 0 in
      check bool "mat period" true (Date.equal (Period.start_date p0) (date 2025 1 1)))
    mat_lines;
  (* total is also materialized *)
  (match
     Statement.fold mat_stmt
       ~line_fn:(fun _ _ -> None)
       ~group_fn:(fun _ _ total -> total)
   with
  | Some m -> fla "mat total" [| 10.0; 10.0; 10.0 |] (Formula.Materialized.to_array m)
  | None -> fail "expected materialized total")

(* Deps *)

let test_deps () =
  let a = Formula.init ~name:"A" (fun _i _p -> 1.0) in
  let b = Formula.init ~name:"B" (fun _i _p -> 2.0) in
  let c = Formula.add a b |> Formula.named "C" in
  let nodes, edges = Formula.Deps.graph [ c ] in
  check int "nodes" 3 (List.length nodes);
  check int "edges" 2 (List.length edges);
  let names =
    List.filter_map (fun (n : Formula.Deps.node) -> n.name) nodes |> List.sort String.compare
  in
  check (list string) "names" [ "A"; "B"; "C" ] names;
  (* shared node deduplication *)
  let shared = Formula.init ~name:"Shared" (fun _i _p -> 1.0) in
  let x = Formula.map ~name:"X" (fun v -> v +. 1.0) shared in
  let y = Formula.map ~name:"Y" (fun v -> v *. 2.0) shared in
  let z = Formula.add x y |> Formula.named "Z" in
  let nodes, edges = Formula.Deps.graph [ z ] in
  check int "shared nodes" 4 (List.length nodes);
  check int "shared edges" 4 (List.length edges);
  (* named_only collapses unnamed intermediaries *)
  let revenue = Formula.init ~name:"Revenue" (fun _i _p -> 1000.0) in
  let cogs = Formula.map (fun x -> x *. 0.3) revenue |> Formula.named "COGS" in
  let gp = Formula.sub revenue cogs |> Formula.named "Gross Profit" in
  let nodes, edges = Formula.Deps.graph ~named_only:true [ gp ] in
  check int "named nodes" 3 (List.length nodes);
  check int "named edges" 3 (List.length edges)

(* Date: weekday *)

let test_weekday () =
  (* 2025-01-06 is Monday *)
  check int "mon" 1 (Date.weekday (date 2025 1 6));
  check int "tue" 2 (Date.weekday (date 2025 1 7));
  check int "wed" 3 (Date.weekday (date 2025 1 8));
  check int "thu" 4 (Date.weekday (date 2025 1 9));
  check int "fri" 5 (Date.weekday (date 2025 1 10));
  check int "sat" 6 (Date.weekday (date 2025 1 11));
  check int "sun" 7 (Date.weekday (date 2025 1 12))

(* Calendar *)

let test_calendar () =
  (* weekdays *)
  check bool "mon bday" true (Calendar.weekdays (date 2025 1 6));
  check bool "fri bday" true (Calendar.weekdays (date 2025 1 10));
  check bool "sat off" false (Calendar.weekdays (date 2025 1 11));
  check bool "sun off" false (Calendar.weekdays (date 2025 1 12));
  let adj = Calendar.adjust in
  let cal = Calendar.weekdays in
  (* Unadjusted: no change *)
  ds "unadj" "2025-01-11" (adj Unadjusted cal (date 2025 1 11));
  (* Following: next business day *)
  ds "fol sat" "2025-01-13" (adj Following cal (date 2025 1 11));
  ds "fol sun" "2025-01-13" (adj Following cal (date 2025 1 12));
  ds "fol weekday" "2025-01-10" (adj Following cal (date 2025 1 10));
  (* Preceding: previous business day *)
  ds "prec sat" "2025-01-10" (adj Preceding cal (date 2025 1 11));
  ds "prec sun" "2025-01-10" (adj Preceding cal (date 2025 1 12));
  (* Modified_following: next bday, but fall back if crosses month *)
  ds "mf normal" "2025-01-13" (adj Modified_following cal (date 2025 1 11));
  (* March 29, 2025 is Saturday; next bday is March 31 (same month) *)
  ds "mf eom ok" "2025-03-31" (adj Modified_following cal (date 2025 3 29));
  (* May 31, 2025 is Saturday; next bday is June 2 (crosses month) → fall back to May 30 *)
  ds "mf cross" "2025-05-30" (adj Modified_following cal (date 2025 5 31));
  (* Modified_preceding: prev bday, but next if crosses month *)
  ds "mp normal" "2025-01-10" (adj Modified_preceding cal (date 2025 1 11));
  (* March 1, 2025 is Saturday; prev bday is Feb 28 (crosses month) → next bday is March 3 *)
  ds "mp cross" "2025-03-03" (adj Modified_preceding cal (date 2025 3 1))

(* Schedule *)

let test_schedule () =
  let qoffset = Period.make_offset ~quarters:1 () in
  let moffset = Period.make_offset ~months:1 () in
  (* Basic quarterly schedule: 2025-01-01 to 2026-01-01 = 4 periods *)
  let s = Schedule.make ~start_date:(date 2025 1 1) ~end_date:(date 2026 1 1) ~offset:qoffset () in
  check int "q len" 4 (Schedule.length s);
  let d = Schedule.dates s in
  check int "q dates" 5 (Array.length d);
  ds "q d0" "2025-01-01" d.(0);
  ds "q d1" "2025-04-01" d.(1);
  ds "q d2" "2025-07-01" d.(2);
  ds "q d3" "2025-10-01" d.(3);
  ds "q d4" "2026-01-01" d.(4);
  (* Drift avoidance: monthly from Jan 31, dates should not drift *)
  let s =
    Schedule.make ~start_date:(date 2025 1 31) ~end_date:(date 2025 7 31) ~offset:moffset
      ~stub:Short_last ()
  in
  let d = Schedule.dates s in
  ds "drift d0" "2025-01-31" d.(0);
  ds "drift d1" "2025-02-28" d.(1);
  ds "drift d2" "2025-03-31" d.(2);
  ds "drift d3" "2025-04-30" d.(3);
  ds "drift d4" "2025-05-31" d.(4);
  ds "drift d5" "2025-06-30" d.(5);
  ds "drift d6" "2025-07-31" d.(6);
  (* Roll: End_of_month *)
  let s =
    Schedule.make ~start_date:(date 2025 1 15) ~end_date:(date 2025 4 15) ~offset:moffset
      ~roll:End_of_month ~stub:Short_last ()
  in
  let d = Schedule.dates s in
  ds "eom d1" "2025-02-28" d.(1);
  ds "eom d2" "2025-03-31" d.(2);
  (* Roll: Day_of_month *)
  let s =
    Schedule.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 4 1) ~offset:moffset
      ~roll:(Day_of_month 15) ~stub:Short_last ()
  in
  let d = Schedule.dates s in
  ds "dom d1" "2025-02-15" d.(1);
  ds "dom d2" "2025-03-15" d.(2);
  (* Stub: Short_first (backward) — stub at start *)
  let s =
    Schedule.make ~start_date:(date 2025 1 15) ~end_date:(date 2025 7 1) ~offset:qoffset
      ~stub:Short_first ()
  in
  let d = Schedule.dates s in
  ds "sf d0" "2025-01-15" d.(0);
  ds "sf d1" "2025-04-01" d.(1);
  ds "sf d2" "2025-07-01" d.(2);
  check int "sf len" 2 (Schedule.length s);
  (* Stub: Short_last (forward) — stub at end *)
  let s =
    Schedule.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 5 15) ~offset:qoffset
      ~stub:Short_last ()
  in
  let d = Schedule.dates s in
  ds "sl d0" "2025-01-01" d.(0);
  ds "sl d1" "2025-04-01" d.(1);
  ds "sl d2" "2025-05-15" d.(2);
  check int "sl len" 2 (Schedule.length s);
  (* Stub: Long_first — merge first two periods *)
  let s =
    Schedule.make ~start_date:(date 2025 1 15) ~end_date:(date 2025 7 1) ~offset:qoffset
      ~stub:Long_first ()
  in
  check int "lf len" 1 (Schedule.length s);
  let d = Schedule.dates s in
  ds "lf d0" "2025-01-15" d.(0);
  ds "lf d1" "2025-07-01" d.(1);
  (* Stub: Long_last — merge last two periods *)
  let s =
    Schedule.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 5 15) ~offset:qoffset
      ~stub:Long_last ()
  in
  check int "ll len" 1 (Schedule.length s);
  let d = Schedule.dates s in
  ds "ll d0" "2025-01-01" d.(0);
  ds "ll d1" "2025-05-15" d.(1);
  (* Stub: Long_first with even division — no stub exists, should NOT merge *)
  let s =
    Schedule.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 7 1) ~offset:qoffset
      ~stub:Long_first ()
  in
  check int "lf even len" 2 (Schedule.length s);
  let d = Schedule.dates s in
  ds "lf even d0" "2025-01-01" d.(0);
  ds "lf even d1" "2025-04-01" d.(1);
  ds "lf even d2" "2025-07-01" d.(2);
  (* Stub: Long_last with even division — no stub exists, should NOT merge *)
  let s =
    Schedule.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 7 1) ~offset:qoffset
      ~stub:Long_last ()
  in
  check int "ll even len" 2 (Schedule.length s);
  let d = Schedule.dates s in
  ds "ll even d0" "2025-01-01" d.(0);
  ds "ll even d1" "2025-04-01" d.(1);
  ds "ll even d2" "2025-07-01" d.(2);
  (* Business day adjustment: interior dates adjusted, endpoints fixed *)
  let s =
    Schedule.make ~start_date:(date 2025 1 1) ~end_date:(date 2026 1 1) ~offset:qoffset
      ~bdc:Following ~calendar:Calendar.weekdays ()
  in
  let unadj = Schedule.unadjusted_dates s and adj = Schedule.dates s in
  ds "bdc unadj d0" "2025-01-01" unadj.(0);
  ds "bdc adj d0" "2025-01-01" adj.(0);
  ds "bdc unadj d4" "2026-01-01" unadj.(4);
  ds "bdc adj d4" "2026-01-01" adj.(4);
  (* 2025-07-01 is Tuesday — no adjustment needed *)
  ds "bdc adj d2" "2025-07-01" adj.(2);
  (* Periods *)
  let ps = Schedule.periods s in
  check int "periods len" 4 (Array.length ps);
  ds "p0 start" "2025-01-01" (Period.start_date ps.(0));
  check bool "p0 end" true (Date.equal adj.(1) (Period.end_date ps.(0)));
  (* Unadjusted periods *)
  let ups = Schedule.unadjusted_periods s in
  ds "up0 end" "2025-04-01" (Period.end_date ups.(0));
  (* to_timeline round-trip *)
  let tl = Schedule.to_timeline s in
  check int "tl len" 4 (Timeline.length tl);
  check bool "tl start" true (Date.equal adj.(0) (Timeline.start_date tl));
  check bool "tl end" true (Date.equal adj.(4) (Timeline.end_date tl));
  (* to_events *)
  let events = Schedule.to_events ~at:`Start (fun i _p -> float_of_int (i + 1) *. 100.0) s in
  check int "events len" 4 (List.length events);
  let e0_date, e0_val = List.hd events in
  check bool "event0 date" true (Date.equal adj.(0) e0_date);
  fl "event0 val" 100.0 e0_val;
  (* to_events ~at:`End *)
  let events_end = Schedule.to_events ~at:`End (fun i _p -> float_of_int (i + 1) *. 100.0) s in
  let e0_date_end, _ = List.hd events_end in
  check bool "event0 end date" true (Date.equal adj.(1) e0_date_end);
  (* to_string / pp *)
  let str = Schedule.to_string s in
  check bool "str has periods" true (has "4 periods" str);
  let ppstr = to_s (fun ppf -> Schedule.pp ppf s) in
  check bool "pp matches" true (String.equal str ppstr);
  (* Error: end <= start *)
  invalid "Schedule.make: end_date must be after start_date" (fun () ->
      Schedule.make ~start_date:(date 2025 6 1) ~end_date:(date 2025 1 1) ~offset:qoffset ());
  invalid "Schedule.make: end_date must be after start_date" (fun () ->
      Schedule.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 1 1) ~offset:qoffset ());
  (* Error: zero offset *)
  invalid "Schedule.make: offset must advance by at least one day or month" (fun () ->
      Schedule.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 7 1)
        ~offset:(Period.make_offset ()) ())

(* Flow *)

let test_flow () =
  let evf msg tl f exp = fla msg exp (Flow.eval tl f) in
  evf "const" tl3 (Flow.const 42.0) [| 42.0; 42.0; 42.0 |];
  evf "of_array" tl3 (Flow.of_array [| 1.0; 2.0 |]) [| 1.0; 2.0; 0.0 |];
  evf "init" tl3
    (Flow.init (fun p -> Period.days p |> float_of_int))
    [| 31.0; 28.0; 31.0 |];
  evf "of_events" tl3
    (Flow.of_events [ (date 2025 1 10, 100.0); (date 2025 3 5, 200.0) ])
    [| 100.0; 0.0; 200.0 |];
  (* exact algebra *)
  let a = Flow.of_array [| 10.0; 20.0; 30.0 |] in
  let b = Flow.of_array [| 1.0; 2.0; 3.0 |] in
  evf "add" tl3 (Flow.add a b) [| 11.0; 22.0; 33.0 |];
  evf "sub" tl3 (Flow.sub a b) [| 9.0; 18.0; 27.0 |];
  evf "scale" tl3 (Flow.scale 2.0 a) [| 20.0; 40.0; 60.0 |];
  evf "neg" tl3 (Flow.neg a) [| -10.0; -20.0; -30.0 |];
  evf "sum" tl3 (Flow.sum [ a; b; Flow.const 100.0 ]) [| 111.0; 122.0; 133.0 |];
  (* escape hatch roundtrip *)
  let f = Flow.of_formula (Flow.formula a) in
  evf "roundtrip" tl3 f [| 10.0; 20.0; 30.0 |];
  (* named *)
  let _ = Flow.named "Revenue" a in
  (* eval_materialized *)
  let m = Flow.eval_materialized tl3 a in
  check int "mat length" 3 (Formula.Materialized.length m);
  fl "mat get 0" 10.0 (Formula.Materialized.get m 0)

(* Balance *)

let test_balance () =
  let evb msg tl b exp = fla msg exp (Balance.eval tl b) in
  evb "const" tl3 (Balance.const 100.0) [| 100.0; 100.0; 100.0 |];
  evb "of_array" tl3 (Balance.of_array [| 10.0; 20.0 |]) [| 10.0; 20.0; 0.0 |];
  (* roll_forward: running sum *)
  let flow = Flow.of_array [| 100.0; 200.0; 300.0 |] in
  evb "roll_forward" tl3
    (Balance.roll_forward ~init:1000.0 flow)
    [| 1100.0; 1300.0; 1600.0 |];
  (* roll_forward_with: custom accumulation *)
  evb "roll_forward_with" tl3
    (Balance.roll_forward_with ~init:1.0 (fun ~acc ~x -> acc *. x) flow)
    [| 100.0; 20000.0; 6000000.0 |];
  (* algebra *)
  let a = Balance.of_array [| 10.0; 20.0; 30.0 |] in
  let b = Balance.of_array [| 1.0; 2.0; 3.0 |] in
  evb "add" tl3 (Balance.add a b) [| 11.0; 22.0; 33.0 |];
  evb "sub" tl3 (Balance.sub a b) [| 9.0; 18.0; 27.0 |];
  evb "scale" tl3 (Balance.scale 2.0 a) [| 20.0; 40.0; 60.0 |];
  evb "map" tl3 (Balance.map (fun x -> x *. x) a) [| 100.0; 400.0; 900.0 |];
  evb "map2" tl3 (Balance.map2 ( *. ) a b) [| 10.0; 40.0; 90.0 |];
  (* cross-period *)
  evb "prev" tl3 (Balance.prev a ~default:0.0) [| 0.0; 10.0; 20.0 |];
  evb "at_period_start" tl3 (Balance.at_period_start a ~default:0.0) [| 0.0; 10.0; 20.0 |];
  evb "at_period_end" tl3 (Balance.at_period_end () a) [| 10.0; 20.0; 30.0 |];
  (* escape hatch roundtrip *)
  let rt = Balance.of_formula (Balance.formula a) in
  evb "roundtrip" tl3 rt [| 10.0; 20.0; 30.0 |];
  (* eval_materialized *)
  let m = Balance.eval_materialized tl3 a in
  check int "mat length" 3 (Formula.Materialized.length m);
  fl "mat get 1" 20.0 (Formula.Materialized.get m 1);
  (* change: balance -> flow *)
  let bal = Balance.of_array [| 100.0; 150.0; 120.0 |] in
  let chg = Balance.change bal ~default:0.0 in
  fla "change" [| 100.0; 50.0; -30.0 |] (Flow.eval tl3 chg);
  (* roundtrip: roll_forward (change b) ~ b when default = init *)
  let flow2 = Flow.of_array [| 10.0; 20.0; 30.0 |] in
  let bal2 = Balance.roll_forward ~init:0.0 flow2 in
  let chg2 = Balance.change bal2 ~default:0.0 in
  fla "roundtrip change" [| 10.0; 20.0; 30.0 |] (Flow.eval tl3 chg2);
  (* feedback: interest accrual on previous balance *)
  let balance2, interest =
    Balance.feedback ~default:100.0 (fun prev_bal ->
        let interest =
          Flow.of_formula (Formula.map (fun b -> b *. 0.01) (Balance.formula prev_bal))
        in
        let balance = Balance.roll_forward ~init:100.0 interest in
        (balance, (balance, interest)))
  in
  let vb2 = Balance.eval tl3 balance2 in
  let vi = Flow.eval tl3 interest in
  fl "fb bal[0]" 101.0 vb2.(0);
  fl "fb int[0]" 1.0 vi.(0);
  fl "fb int[1]" 1.01 vi.(1);
  (* fixpoint: LTC construction loan pattern *)
  let ltc = 0.8 and rate = 0.05 in
  let base_cost = Balance.of_array [| 1000.0; 2000.0; 3000.0 |] in
  let loan =
    Balance.fixpoint ~guess:0.0 (fun commitment ->
        Balance.map2 (fun bc c -> ltc *. (bc +. rate *. c))
          base_cost commitment)
  in
  let lv = Balance.eval tl3 loan in
  let expected i =
    let bc = [| 1000.0; 2000.0; 3000.0 |].(i) in
    ltc *. bc /. (1.0 -. (ltc *. rate))
  in
  fl "fix[0]" (expected 0) lv.(0);
  fl "fix[1]" (expected 1) lv.(1);
  fl "fix[2]" (expected 2) lv.(2)

(* Integration: coffee shop model *)

let test_coffee_shop () =
  let tl = Timeline.monthly ~start_date:(date 2025 1 1) ~n:12 in
  let revenue =
    Formula.init ~name:"revenue" (fun _i p ->
        let sd = Period.start_date p in
        8000.0 *. (1.0 +. (0.05 *. Daycount.calendar_monthly (date 2025 1 1) sd)))
  in
  let cogs = Formula.scale 0.30 revenue in
  let rent = Formula.const 2000.0 in
  let salaries = Formula.const 3500.0 in
  let total_opex = Formula.sum [ cogs; rent; salaries ] in
  let net_income = Formula.sub revenue total_opex in
  let equipment = Formula.of_events [ (date 2025 1 15, -15000.0) ] in
  let total_flow = Formula.add net_income equipment in
  let cash = Formula.cumsum ~name:"cash" ~init:50000.0 total_flow in
  let rv = Formula.eval tl revenue in
  let cv = Formula.eval tl cash in
  fl "revenue m0" 8000.0 rv.(0);
  check bool "revenue grows" true (rv.(11) > rv.(0));
  fl "cash m0" 35100.0 cv.(0);
  check bool "cash recovers" true (cv.(11) > cv.(0))

(* Key *)

let test_key () =
  let k1 = Key.make "Revenue" in
  let k2 = Key.make "COGS" in
  let k3 = Key.make "Revenue" in
  check string "name" "Revenue" (Key.name k1);
  check string "name2" "COGS" (Key.name k2);
  check bool "distinct" false (Key.equal k1 k2);
  check bool "same name distinct" false (Key.equal k1 k3);
  check bool "self equal" true (Key.equal k1 k1);
  check bool "compare self" true (Key.compare k1 k1 = 0);
  check bool "compare order" true (Key.compare k1 k2 < 0);
  check string "to_string" "Revenue" (Key.to_string k1);
  let ppstr = to_s (fun ppf -> Key.pp ppf k1) in
  check string "pp" "Revenue" ppstr

(* Scope *)

let test_scope () =
  let k1 : int Key.t = Key.make "A" in
  let k2 : int Key.t = Key.make "B" in
  let k3 : int Key.t = Key.make "C" in
  let s = Scope.create () in
  check bool "unsealed" false (Scope.is_sealed s);
  Scope.define s k1 1;
  Scope.define s k2 2;
  check int "find k1" 1 (Scope.find s k1);
  check int "find k2" 2 (Scope.find s k2);
  check (option int) "find_opt k3" None (Scope.find_opt s k3);
  check int "size" 2 (Scope.size s);
  check int "keys len" 2 (List.length (Scope.keys s));
  (* duplicate *)
  invalid "Scope.define: duplicate key A" (fun () -> Scope.define s k1 99);
  (* seal *)
  Scope.seal s;
  check bool "sealed" true (Scope.is_sealed s);
  invalid "Scope.define: scope is sealed, cannot define C" (fun () -> Scope.define s k3 3);
  (* find still works after seal *)
  check int "find after seal" 1 (Scope.find s k1);
  (* missing key *)
  invalid "Scope.find: key C not found" (fun () -> ignore (Scope.find s k3));
  (* heterogeneous: mix int and string in same scope *)
  let ki : int Key.t = Key.make "Int" in
  let ks : string Key.t = Key.make "Str" in
  let h = Scope.create () in
  Scope.define h ki 42;
  Scope.define h ks "hello";
  check int "het int" 42 (Scope.find h ki);
  check string "het string" "hello" (Scope.find h ks);
  (* parent scope *)
  let parent = Scope.create () in
  let kp : float Key.t = Key.make "Rate" in
  Scope.define parent kp 0.065;
  Scope.seal parent;
  let child = Scope.create_child parent in
  let kc : int Key.t = Key.make "Term" in
  Scope.define child kc 360;
  fl "parent lookup" 0.065 (Scope.find child kp);
  check int "child lookup" 360 (Scope.find child kc);
  check bool "mem parent key" true (Scope.mem child kp);
  check bool "mem_local parent key" false (Scope.mem_local child kp);
  check bool "mem_local child key" true (Scope.mem_local child kc);
  (* shadow parent *)
  let shadow = Scope.create_child parent in
  let kp2 : float Key.t = Key.make "Rate" in
  Scope.define shadow kp 0.05;
  fl "shadow" 0.05 (Scope.find shadow kp);
  fl "parent unchanged" 0.065 (Scope.find parent kp);
  ignore kp2

(* Run *)

let () =
  run "orcaset2"
    [
      ("Date", [ test_case "date" `Quick test_date; test_case "weekday" `Quick test_weekday ]);
      ("Period", [ test_case "period" `Quick test_period ]);
      ("Timeline", [ test_case "timeline" `Quick test_timeline ]);
      ("Daycount", [ test_case "daycount" `Quick test_daycount ]);
      ("Calendar", [ test_case "calendar" `Quick test_calendar ]);
      ("Schedule", [ test_case "schedule" `Quick test_schedule ]);
      ( "Formula",
         [
           test_case "constructors" `Quick test_formula_constructors;
           test_case "pointwise" `Quick test_formula_pointwise;
           test_case "cross-period" `Quick test_formula_cross_period;
           test_case "feedback" `Quick test_formula_feedback;
           test_case "fixpoint" `Quick test_formula_fixpoint;
           test_case "growth" `Quick test_formula_growth;
           test_case "query" `Quick test_formula_query;
         ] );
      ("Materialized", [ test_case "materialized" `Quick test_materialized ]);
      ("Statement", [ test_case "statement" `Quick test_statement ]);
      ("Deps", [ test_case "deps" `Quick test_deps ]);
      ("Flow", [ test_case "flow" `Quick test_flow ]);
      ("Balance", [ test_case "balance" `Quick test_balance ]);
      ("Key", [ test_case "key" `Quick test_key ]);
      ("Scope", [ test_case "scope" `Quick test_scope ]);
      ("Integration", [ test_case "coffee shop" `Quick test_coffee_shop ]);
    ]
