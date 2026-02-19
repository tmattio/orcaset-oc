(** CRE Portfolio -- Multi-Property Aggregation

    Aggregates 10,000 identical properties into a portfolio-level pro forma. Each property is a full
    model (revenue, opex, capex, debt) evaluated against a shared timeline. Aggregation is
    parallelized across CPU cores with [Domain.spawn].

    Each property's series are evaluated independently via [Series.eval_many], then the resulting
    [float array] values are summed across properties. The final portfolio totals are assembled into
    a [Statement] built from raw [float array] data rather than [Series.t] values -- showing that
    [Statement.group], [Statement.line], and [Statement.pp] work with pre-evaluated arrays.

    Demonstrates: Series.eval_many, Statement.line, Statement.group, Statement.layout, Statement.pp,
    Domain.spawn, parallel aggregation. *)

open Orcaset2

(* Assumptions *)

let property_count = 10_000
let start_date = Date.make 2023 1 1
let n_periods = 120
let display_n = 12

(* Timeline *)

(* Full 120-month timeline used for evaluation *)
let tl = Timeline.monthly ~start_date ~n:n_periods

(* Shorter timeline for display only -- Statement.pp prints one column per period,
   so we use a 12-period timeline to keep the output readable *)
let display_tl = Timeline.monthly ~start_date ~n:display_n

(* Property Model Builder *)

type assumptions = {
  building_sf : float;
  parking_spaces : int;
  base_rent_per_sf_year1 : float;
  rent_growth : float;
  parking_rate_monthly : float;
  cam_recovery_pct : float;
  cam_estimate_first : float;
  other_income_monthly : float;
  vacancy_rate : float;
  property_taxes_annual : float;
  insurance_annual : float;
  utilities_monthly : float;
  repairs_monthly : float;
  management_fee_pct : float;
  janitorial_monthly : float;
  landscaping_monthly : float;
  security_monthly : float;
  expense_growth : float;
  purchase_price : float;
  ltv : float;
  interest_rate : float;
  loan_term_years : int;
  reserve_pct : float;
  ti_per_sf_annual : float;
  leasing_commission_pct : float;
}

(* Build all line items for a single property as (label, Series.t) pairs.
   Returns unevaluated series -- the caller passes them to Series.eval_many
   to materialize the values against a timeline. *)
let build_property (a : assumptions) =
  let annual_growth ~name ~initial ~rate =
    Series.init ~name (fun _i p ->
        let sd = Period.start_date p in
        initial *. (1.0 +. (rate *. Daycount.actual_360 start_date sd)))
  in
  let base_rent_monthly = a.building_sf *. a.base_rent_per_sf_year1 /. 12.0 in
  let parking_monthly = float_of_int a.parking_spaces *. a.parking_rate_monthly in
  let loan_amount = a.purchase_price *. a.ltv in
  let loan_term_months = a.loan_term_years * 12 in

  (* Revenue + OpEx feedback loop *)
  let base_rent = annual_growth ~name:"Base Rent" ~initial:base_rent_monthly ~rate:a.rent_growth in
  let parking = annual_growth ~name:"Parking" ~initial:parking_monthly ~rate:a.rent_growth in
  let other_income =
    annual_growth ~name:"Other Income" ~initial:a.other_income_monthly ~rate:a.rent_growth
  in
  let property_taxes =
    annual_growth ~name:"Property Taxes"
      ~initial:(-.a.property_taxes_annual /. 12.0)
      ~rate:a.expense_growth
  in
  let insurance =
    annual_growth ~name:"Insurance" ~initial:(-.a.insurance_annual /. 12.0) ~rate:a.expense_growth
  in
  let utilities =
    annual_growth ~name:"Utilities" ~initial:(-.a.utilities_monthly) ~rate:a.expense_growth
  in
  let repairs =
    annual_growth ~name:"Repairs" ~initial:(-.a.repairs_monthly) ~rate:a.expense_growth
  in
  let janitorial =
    annual_growth ~name:"Janitorial" ~initial:(-.a.janitorial_monthly) ~rate:a.expense_growth
  in
  let landscaping =
    annual_growth ~name:"Landscaping" ~initial:(-.a.landscaping_monthly) ~rate:a.expense_growth
  in
  let security =
    annual_growth ~name:"Security" ~initial:(-.a.security_monthly) ~rate:a.expense_growth
  in
  let cam_recoveries, gpr, vacancy_loss, egi, management, opex_total =
    Series.feedback ~default:0.0 (fun prev_opex ->
        let cam_recoveries =
          Series.map ~name:"CAM Recoveries"
            (fun prev ->
              if prev = 0.0 then a.cam_estimate_first else Float.abs prev *. a.cam_recovery_pct)
            prev_opex
        in
        let gpr = Series.sum ~name:"GPR" [ base_rent; parking; cam_recoveries; other_income ] in
        let vacancy_loss = Series.named "Vacancy Loss" (Series.scale (-.a.vacancy_rate) gpr) in
        let egi = Series.named "EGI" (Series.add gpr vacancy_loss) in
        let management = Series.map ~name:"Management" (fun e -> -.e *. a.management_fee_pct) egi in
        let opex_total =
          Series.sum ~name:"Total OpEx"
            [
              property_taxes;
              insurance;
              utilities;
              repairs;
              management;
              janitorial;
              landscaping;
              security;
            ]
        in
        (opex_total, (cam_recoveries, gpr, vacancy_loss, egi, management, opex_total)))
  in

  (* NOI *)
  let noi = Series.named "NOI" (Series.add egi opex_total) in

  (* CapEx *)
  let capital_reserves = Series.map ~name:"Capital Reserves" (fun e -> -.e *. a.reserve_pct) egi in
  let ti =
    Series.const ~name:"Tenant Improvements" (-.(a.ti_per_sf_annual *. a.building_sf /. 12.0))
  in
  let leasing_commissions =
    Series.map ~name:"Leasing Commissions" (fun e -> -.e *. a.leasing_commission_pct) egi
  in
  let capex_total = Series.sum ~name:"Total CapEx" [ capital_reserves; ti; leasing_commissions ] in

  (* CFBF *)
  let cfbf = Series.named "CFBF" (Series.add noi capex_total) in

  let monthly_payment =
    let r = a.interest_rate /. 12.0 in
    let n = float_of_int loan_term_months in
    let f = (1.0 +. r) ** n in
    loan_amount *. (r *. f) /. (f -. 1.0)
  in
  let year_fracs =
    Series.init ~name:"Year Fracs" (fun _i p ->
        Daycount.actual_360 (Period.start_date p) (Period.end_date p))
  in
  let total_pmt = Series.const ~name:"Debt Payment" (-.monthly_payment) in
  let _balance, (interest, principal) =
    Series.feedback ~name:"Loan Balance" ~default:loan_amount (fun prev_bal ->
        let interest =
          Series.named "Interest"
            (Series.map2 (fun bal yf -> -.bal *. a.interest_rate *. yf) prev_bal year_fracs)
        in
        let principal = Series.named "Principal" (Series.sub total_pmt interest) in
        let balance = Series.cumsum ~init:loan_amount principal in
        (balance, (balance, (interest, principal))))
  in
  let debt_service = Series.named "Debt Service" (Series.add interest principal) in

  (* CFAF *)
  let cfaf = Series.named "CFAF" (Series.add cfbf debt_service) in

  [
    ("Base Rent", base_rent);
    ("Parking Income", parking);
    ("CAM Recoveries", cam_recoveries);
    ("Other Income", other_income);
    ("GPR", gpr);
    ("Vacancy & Credit Loss", vacancy_loss);
    ("EGI", egi);
    ("Property Taxes", property_taxes);
    ("Insurance", insurance);
    ("Utilities", utilities);
    ("Repairs & Maintenance", repairs);
    ("Property Management", management);
    ("Janitorial", janitorial);
    ("Landscaping", landscaping);
    ("Security", security);
    ("Total OpEx", opex_total);
    ("NOI", noi);
    ("Capital Reserves", capital_reserves);
    ("Tenant Improvements", ti);
    ("Leasing Commissions", leasing_commissions);
    ("Total CapEx", capex_total);
    ("CFBF", cfbf);
    ("Interest Expense", interest);
    ("Principal Payment", principal);
    ("Total Debt Service", debt_service);
    ("CFAF", cfaf);
  ]

(* Default Property Assumptions *)

let downtown_office : assumptions =
  {
    building_sf = 25000.0;
    parking_spaces = 50;
    base_rent_per_sf_year1 = 22.0;
    rent_growth = 0.03;
    parking_rate_monthly = 75.0;
    cam_recovery_pct = 0.85;
    cam_estimate_first = 24000.0 *. 0.85 /. 12.0;
    other_income_monthly = 1500.0;
    vacancy_rate = 0.07;
    property_taxes_annual = 72000.0;
    insurance_annual = 15000.0;
    utilities_monthly = 6250.0;
    repairs_monthly = 4167.0;
    management_fee_pct = 0.04;
    janitorial_monthly = 5208.0;
    landscaping_monthly = 1250.0;
    security_monthly = 2083.0;
    expense_growth = 0.025;
    purchase_price = 4500000.0;
    ltv = 0.70;
    interest_rate = 0.055;
    loan_term_years = 25;
    reserve_pct = 0.03;
    ti_per_sf_annual = 1.50;
    leasing_commission_pct = 0.02;
  }

(* Evaluation and Aggregation *)

(* Build series for one property, then evaluate them all in a single
   Series.eval_many call so shared subexpressions are computed once. *)
let eval_property assumptions =
  let series_list = build_property assumptions in
  let labels = List.map fst series_list in
  let series = List.map snd series_list in
  let values = Series.eval_many tl series in
  List.combine labels values

let sum_arrays a b = Array.init (Array.length a) (fun i -> a.(i) +. b.(i))
let zeros () = Array.make n_periods 0.0

let split_into_chunks n lst =
  let len = List.length lst in
  let chunk_size = (len + n - 1) / n in
  let rec take_chunk acc remaining count =
    match remaining with
    | [] -> (List.rev acc, [])
    | _ when count = 0 -> (List.rev acc, remaining)
    | x :: xs -> take_chunk (x :: acc) xs (count - 1)
  in
  let rec split acc remaining =
    match remaining with
    | [] -> List.rev acc
    | _ ->
        let chunk, rest = take_chunk [] remaining chunk_size in
        split (chunk :: acc) rest
  in
  split [] lst

let aggregate_chunk assumptions_chunk =
  let n_lines = List.length (build_property downtown_office) in
  let accum = Array.init n_lines (fun _ -> zeros ()) in
  List.iter
    (fun a ->
      let property_values = eval_property a in
      List.iteri (fun i (_, values) -> accum.(i) <- sum_arrays accum.(i) values) property_values)
    assumptions_chunk;
  accum

(* Parallel Evaluation *)

(* Each property is fully independent (its own Series DAG and eval context),
   so we partition properties across OS-level domains for parallel evaluation.
   The main domain processes one chunk while other domains handle the rest,
   then partial results are summed element-wise. *)
let () =
  let assumptions_list = List.init property_count (fun _ -> downtown_office) in
  let num_domains = Domain.recommended_domain_count () in
  let chunks = split_into_chunks num_domains assumptions_list in

  let aggregated =
    match chunks with
    | [] -> [||]
    | [ single ] -> aggregate_chunk single
    | main_chunk :: other_chunks ->
        (* Spawn worker domains for all chunks except the first, which runs
           on the main domain to avoid wasting a core *)
        let domains =
          List.map (fun chunk -> Domain.spawn (fun () -> aggregate_chunk chunk)) other_chunks
        in
        let main_result = aggregate_chunk main_chunk in
        let other_results = List.map Domain.join domains in
        List.fold_left
          (fun acc r -> Array.init (Array.length acc) (fun i -> sum_arrays acc.(i) r.(i)))
          main_result other_results
  in

  (* Build label index from a sample property for looking up aggregated arrays *)
  let labels = build_property downtown_office |> List.map fst |> Array.of_list in

  let get label =
    let i =
      let rec find j =
        if j >= Array.length labels then failwith ("Label not found: " ^ label)
        else if labels.(j) = label then j
        else find (j + 1)
      in
      find 0
    in
    aggregated.(i)
  in

  (* Statement.line and Statement.group accept float array directly --
     no need to wrap pre-evaluated data back into Series.t *)
  let portfolio_statement =
    let open Statement in
    group "Portfolio Totals"
      [
        group ~total:(get "GPR") "Gross Potential Rent"
          [
            line "Base Rent" (get "Base Rent");
            line "Parking Income" (get "Parking Income");
            line "CAM Recoveries" (get "CAM Recoveries");
            line "Other Income" (get "Other Income");
          ];
        line "Less: Vacancy & Credit Loss" (get "Vacancy & Credit Loss");
        line "Effective Gross Income" (get "EGI");
        group ~total:(get "Total OpEx") "Operating Expenses"
          [
            line "Property Taxes" (get "Property Taxes");
            line "Insurance" (get "Insurance");
            line "Utilities" (get "Utilities");
            line "Repairs & Maintenance" (get "Repairs & Maintenance");
            line "Property Management" (get "Property Management");
            line "Janitorial" (get "Janitorial");
            line "Landscaping" (get "Landscaping");
            line "Security" (get "Security");
          ];
        line "Net Operating Income (NOI)" (get "NOI");
        group ~total:(get "Total CapEx") "Capital Expenditures"
          [
            line "Capital Reserves" (get "Capital Reserves");
            line "Tenant Improvements" (get "Tenant Improvements");
            line "Leasing Commissions" (get "Leasing Commissions");
          ];
        line "Cash Flow Before Financing" (get "CFBF");
        group ~total:(get "Total Debt Service") "Debt Service"
          [
            line "Interest Expense" (get "Interest Expense");
            line "Principal Payment" (get "Principal Payment");
          ];
        line "Cash Flow After Financing" (get "CFAF");
      ]
  in

  (* Output *)
  let ppf = Format.std_formatter in

  Format.fprintf ppf "PORTFOLIO SUMMARY@.";
  Format.fprintf ppf "=================@.@.";
  Format.fprintf ppf "Properties in Portfolio: %d@." property_count;
  Format.fprintf ppf "Total Square Footage:    %.0f SF@."
    (float_of_int property_count *. downtown_office.building_sf);
  Format.fprintf ppf "Total Purchase Price:    $%.0f@."
    (float_of_int property_count *. downtown_office.purchase_price);
  Format.fprintf ppf "Total Loan Amount:       $%.0f@."
    (float_of_int property_count *. downtown_office.purchase_price *. downtown_office.ltv);
  Format.fprintf ppf "@.";

  Format.fprintf ppf "PORTFOLIO PRO FORMA PROJECTIONS@.";
  Format.fprintf ppf "==================================================@.@.";

  (* Layout uses display_tl (12 periods) so only the first year is printed,
     even though the underlying arrays contain all 120 months of data *)
  let l =
    Statement.layout ~label_width:40 ~col_width:14
      ~col_header:(fun i -> Date.to_string (Timeline.period_start tl i))
      ~pp_num:(fun ppf v -> Format.fprintf ppf "%14.0f" v)
      ~sep:'=' display_tl
  in
  Statement.pp l ppf portfolio_statement;

  (* Dependency graph for a single property *)
  let sample_series = build_property downtown_office |> List.map snd in
  let oc = open_out "model.dot" in
  let dot_ppf = Format.formatter_of_out_channel oc in
  Series.Deps.pp_dot dot_ppf sample_series;
  Format.pp_print_flush dot_ppf ();
  close_out oc
