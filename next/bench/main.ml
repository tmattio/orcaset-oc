(* Harness *)

type bench = { name : string; f : unit -> unit }
type result = { name : string; mean_ns : float; min_ns : float; max_ns : float; std_ns : float }

let warmup_iters = 5
let sample_iters = 50

let time_one f =
  let t0 = Unix.gettimeofday () in
  f ();
  (Unix.gettimeofday () -. t0) *. 1e9

let measure ({ name; f } : bench) =
  for _ = 1 to warmup_iters do
    f ()
  done;
  let times =
    Array.init sample_iters (fun _ ->
        Gc.full_major ();
        Gc.compact ();
        time_one f)
  in
  let n = float_of_int sample_iters in
  let mean = Array.fold_left ( +. ) 0.0 times /. n in
  let var =
    Array.fold_left (fun acc t -> acc +. ((t -. mean) *. (t -. mean))) 0.0 times /. (n -. 1.0)
  in
  {
    name;
    mean_ns = mean;
    min_ns = Array.fold_left Float.min Float.infinity times;
    max_ns = Array.fold_left Float.max Float.neg_infinity times;
    std_ns = Float.sqrt var;
  }

let pp_time ns =
  if ns < 1e3 then Printf.sprintf "%.0f ns" ns
  else if ns < 1e6 then Printf.sprintf "%.1f us" (ns /. 1e3)
  else if ns < 1e9 then Printf.sprintf "%.2f ms" (ns /. 1e6)
  else Printf.sprintf "%.3f s" (ns /. 1e9)

let print_table results =
  Printf.printf "\n%-40s %12s %12s %12s %12s\n" "Benchmark" "mean" "std" "min" "max";
  Printf.printf "%s\n" (String.make 90 '-');
  List.iter
    (fun (category, rs) ->
      List.iter
        (fun (r : result) ->
          Printf.printf "%-40s %12s %12s %12s %12s\n"
            (category ^ " / " ^ r.name)
            (pp_time r.mean_ns) (pp_time r.std_ns) (pp_time r.min_ns) (pp_time r.max_ns))
        rs)
    results

let sized sizes f = List.map (fun n -> { name = string_of_int n; f = f n }) sizes

let run_suites ~title suites =
  let results =
    List.map
      (fun (category, benchmarks) ->
        Printf.printf "\n=== %s ===\n%!" category;
        let rs =
          List.map
            (fun (b : bench) ->
              Printf.printf "  %s ...%!" b.name;
              let r = measure b in
              Printf.printf " done (%s)\n%!" (pp_time r.mean_ns);
              r)
            benchmarks
        in
        (category, rs))
      suites
  in
  Printf.printf "\n\n========================================\n";
  Printf.printf "  %s\n" title;
  Printf.printf "========================================\n";
  print_table results

open Orcaset2

(* Date *)

let date_make () =
  for y = 2000 to 2030 do
    for m = 1 to 12 do
      ignore (Date.make y m 1)
    done
  done

let date_diff () =
  let d1 = Date.make 2025 1 1 in
  let d2 = Date.make 2055 12 31 in
  for _ = 1 to 10_000 do
    ignore (Date.diff d2 d1)
  done

let date_add_days () =
  let d = Date.make 2025 1 1 in
  for i = 0 to 9_999 do
    ignore (Date.add_days d i)
  done

let date_add_months () =
  let d = Date.make 2025 1 31 in
  for i = 0 to 999 do
    ignore (Date.add_months d i)
  done

let date_benchmarks =
  [
    { name = "make"; f = date_make };
    { name = "diff"; f = date_diff };
    { name = "add_days"; f = date_add_days };
    { name = "add_months"; f = date_add_months };
  ]

(* Period generation *)

let period_gen n () = ignore (Timeline.monthly ~start_date:(Date.make 2025 1 1) ~n)
let period_benchmarks = sized [ 12; 120; 360 ] period_gen

(* Day count *)

let daycount_benchmarks =
  let d1 = Date.make 2025 1 15 in
  let d2 = Date.make 2025 7 20 in
  let repeat dc () =
    for _ = 1 to 100_000 do
      ignore (dc d1 d2)
    done
  in
  [
    { name = "actual_360"; f = repeat Daycount.actual_360 };
    { name = "thirty_360"; f = repeat Daycount.thirty_360_us };
    { name = "calendar_monthly"; f = repeat Daycount.calendar_monthly };
  ]

(* Growth series *)

let growth_bench n () =
  let start = Date.make 2025 1 1 in
  let tl = Timeline.monthly ~start_date:start ~n in
  ignore (Flow.eval tl (Flow.growth_simple ~start_date:start ~rate:0.05 1000.0))

let growth_benchmarks = sized [ 12; 120; 360 ] growth_bench

(* Compose *)

let compose_bench n_series () =
  let start = Date.make 2025 1 1 in
  let tl = Timeline.monthly ~start_date:start ~n:120 in
  let series = List.init n_series (fun i -> Flow.const (float_of_int (i + 1))) in
  ignore (Flow.eval tl (Flow.sum series))

let compose_benchmarks =
  List.map (fun n -> { name = Printf.sprintf "%d series" n; f = compose_bench n }) [ 2; 5; 10; 20 ]

(* Accumulation *)

let accum_bench n () =
  let start = Date.make 2025 1 1 in
  let tl = Timeline.monthly ~start_date:start ~n in
  ignore (Balance.eval tl (Balance.roll_forward ~init:0.0 (Flow.const 100.0)))

let accum_benchmarks = sized [ 12; 120; 360 ] accum_bench

(* Balance query *)

let query_bench () =
  let n = 120 in
  let start = Date.make 2025 1 1 in
  let tl = Timeline.monthly ~start_date:start ~n in
  let flow = Flow.const 100.0 in
  let balance = Balance.roll_forward ~init:1000.0 flow in
  let balance_m = Balance.eval tl balance in
  for m = 0 to n - 1 do
    let d = Date.add_months start (m + 1) in
    ignore (Balance.Materialized.at balance_m d)
  done

let query_benchmarks = [ { name = "balance_at x120"; f = query_bench } ]

(* Loan *)

let monthly_payment ~amount ~rate ~term =
  let r = rate /. 12.0 in
  let n = float_of_int term in
  let factor = (1.0 +. r) ** n in
  amount *. (r *. factor) /. (factor -. 1.0)

let loan_bench n () =
  let loan_amount = 50_000_000.0 in
  let annual_rate = 0.065 in
  let start = Date.make 2025 1 1 in
  let tl = Timeline.monthly ~start_date:start ~n in
  let pmt = monthly_payment ~amount:loan_amount ~rate:annual_rate ~term:n in
  let total_pmt = Flow.const (-.pmt) in
  let year_fracs = Flow.year_frac Daycount.thirty_360_us in
  let balance, (interest, principal) =
    Balance.feedback ~default:loan_amount (fun prev_bal ->
        let interest = Flow.scale (-.annual_rate) (Flow.mul (Balance.sample prev_bal) year_fracs) in
        let principal = Flow.sub total_pmt interest in
        let balance = Balance.roll_forward ~init:loan_amount principal in
        (balance, (balance, (interest, principal))))
  in
  ignore (Flow.eval_many tl [ Balance.sample balance; interest; principal ])

let loan_benchmarks = sized [ 12; 120; 360 ] loan_bench

(* CRE proforma *)

let proforma_bench =
  let building_sf = 25_000.0 in
  let parking_spaces = 50 in
  let base_rent_per_sf_year1 = 22.0 in
  let rent_growth = 0.03 in
  let parking_rate_monthly = 75.0 in
  let cam_recovery_pct = 0.85 in
  let cam_estimate_first = 24_000.0 *. cam_recovery_pct /. 12.0 in
  let other_income_monthly = 1_500.0 in
  let vacancy_rate = 0.07 in
  let property_taxes_annual = 72_000.0 in
  let insurance_annual = 15_000.0 in
  let utilities_monthly = 6_250.0 in
  let repairs_monthly = 4_167.0 in
  let management_fee_pct = 0.04 in
  let janitorial_monthly = 5_208.0 in
  let landscaping_monthly = 1_250.0 in
  let security_monthly = 2_083.0 in
  let expense_growth = 0.025 in
  let purchase_price = 4_500_000.0 in
  let ltv = 0.70 in
  let pf_loan_amount = purchase_price *. ltv in
  let interest_rate = 0.055 in
  let loan_term_years = 25 in
  fun n () ->
    let start = Date.make 2023 1 1 in
    let tl = Timeline.monthly ~start_date:start ~n in
    let growing ~name ~rate initial = Flow.growth_simple ~name ~start_date:start ~rate initial in
    let base_rent_monthly = building_sf *. base_rent_per_sf_year1 /. 12.0 in
    let parking_monthly = float_of_int parking_spaces *. parking_rate_monthly in
    (* Revenue *)
    let base_rent = growing ~name:"Base Rent" ~rate:rent_growth base_rent_monthly in
    let parking = growing ~name:"Parking" ~rate:rent_growth parking_monthly in
    let other_income = growing ~name:"Other Income" ~rate:rent_growth other_income_monthly in
    (* Operating expenses *)
    let property_taxes =
      growing ~name:"Property Taxes" ~rate:expense_growth (-.property_taxes_annual /. 12.0)
    in
    let insurance = growing ~name:"Insurance" ~rate:expense_growth (-.insurance_annual /. 12.0) in
    let utilities = growing ~name:"Utilities" ~rate:expense_growth (-.utilities_monthly) in
    let repairs = growing ~name:"Repairs" ~rate:expense_growth (-.repairs_monthly) in
    let janitorial = growing ~name:"Janitorial" ~rate:expense_growth (-.janitorial_monthly) in
    let landscaping = growing ~name:"Landscaping" ~rate:expense_growth (-.landscaping_monthly) in
    let security = growing ~name:"Security" ~rate:expense_growth (-.security_monthly) in
    (* Circular: CAM <-> OpEx via feedback *)
    let _cam_recoveries, _gpr, _vacancy, egi, _mgmt, opex_total =
      Flow.feedback ~default:0.0 (fun prev_opex ->
          let cam_recoveries =
            Flow.map ~name:"CAM Recoveries"
              (fun prev ->
                if prev = 0.0 then cam_estimate_first else Float.abs prev *. cam_recovery_pct)
              prev_opex
          in
          let gpr = Flow.sum ~name:"GPR" [ base_rent; parking; cam_recoveries; other_income ] in
          let vacancy = Flow.scale (-.vacancy_rate) gpr in
          let egi = Flow.add gpr vacancy in
          let mgmt = Flow.scale (-.management_fee_pct) egi in
          let opex_total =
            Flow.sum
              [
                property_taxes;
                insurance;
                utilities;
                repairs;
                mgmt;
                janitorial;
                landscaping;
                security;
              ]
          in
          (opex_total, (cam_recoveries, gpr, vacancy, egi, mgmt, opex_total)))
    in
    let noi = Flow.add egi opex_total in
    (* Debt service *)
    let pmt =
      monthly_payment ~amount:pf_loan_amount ~rate:interest_rate ~term:(loan_term_years * 12)
    in
    let total_pmt = Flow.const (-.pmt) in
    let year_fracs = Flow.year_frac Daycount.actual_360 in
    let _debt_balance, (debt_interest, debt_principal) =
      Balance.feedback ~default:pf_loan_amount (fun prev_bal ->
          let interest =
            Flow.scale (-.interest_rate) (Flow.mul (Balance.sample prev_bal) year_fracs)
          in
          let principal = Flow.sub total_pmt interest in
          let balance = Balance.roll_forward ~init:pf_loan_amount principal in
          (balance, (balance, (interest, principal))))
    in
    let debt_service = Flow.add debt_interest debt_principal in
    ignore (Flow.eval tl (Flow.add noi debt_service))

let proforma_benchmarks = sized [ 12; 120; 360 ] proforma_bench

(* Scale *)

let scale_bench n () =
  let start = Date.make 2025 1 1 in
  let tl = Timeline.monthly ~start_date:start ~n in
  ignore (Flow.eval tl (Flow.growth_simple ~start_date:start ~rate:0.05 1000.0))

let scale_benchmarks = sized [ 12; 120; 360; 1200; 3600 ] scale_bench

(* Runner *)

let () =
  run_suites ~title:"Orcaset2 (next) benchmark results"
    [
      ("Date", date_benchmarks);
      ("Period gen", period_benchmarks);
      ("Day count", daycount_benchmarks);
      ("Growth series", growth_benchmarks);
      ("Compose", compose_benchmarks);
      ("Accumulation", accum_benchmarks);
      ("Balance query", query_benchmarks);
      ("Loan", loan_benchmarks);
      ("CRE proforma", proforma_benchmarks);
      ("Scale", scale_benchmarks);
    ]
