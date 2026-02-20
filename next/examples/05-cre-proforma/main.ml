(** Commercial Real Estate Pro Forma -- Single Property

    Builds a monthly cash flow projection for a Class B office building: revenue, operating
    expenses, capital expenditures, and debt service flowing down to Cash Flow After Financing
    (CFAF).

    Key patterns demonstrated:
    - [Series.feedback] to break a circular dependency: CAM recoveries depend on prior-period OpEx,
      while OpEx includes management fees that depend on current-period EGI.
    - [Series.growth_simple] for calendar-aware annual growth.
    - [Series.year_frac] and [Series.mul] for interest calculations.
    - [Series.cumsum] for a running loan balance.
    - [Statement.group] with explicit [~total] to build a hierarchical pro forma report. *)

open Orcaset2

(* Assumptions *)

let start_date = Date.make 2023 1 1
let n_periods = 12

(* Physical characteristics *)
let building_sf = 25000.0
let parking_spaces = 50

(* Revenue *)
let base_rent_per_sf_year1 = 22.0
let rent_growth = 0.03
let parking_rate_monthly = 75.0
let cam_recovery_pct = 0.85
let cam_estimate_first = 24000.0 *. cam_recovery_pct /. 12.0
let other_income_monthly = 1500.0
let vacancy_rate = 0.07

(* Operating Expenses *)
let property_taxes_annual = 72000.0
let insurance_annual = 15000.0
let utilities_monthly = 6250.0
let repairs_monthly = 4167.0
let management_fee_pct = 0.04
let janitorial_monthly = 5208.0
let landscaping_monthly = 1250.0
let security_monthly = 2083.0
let expense_growth = 0.025

(* Debt *)
let purchase_price = 4500000.0
let ltv = 0.70
let loan_amount = purchase_price *. ltv
let interest_rate = 0.055
let loan_term_years = 25
let loan_term_months = loan_term_years * 12

(* CapEx *)
let reserve_pct = 0.03
let ti_per_sf_annual = 1.50
let leasing_commission_pct = 0.02

(* Timeline *)

let tl = Timeline.monthly ~start_date ~n:n_periods

(* Revenue *)

let base_rent_monthly = building_sf *. base_rent_per_sf_year1 /. 12.0
let parking_monthly = float_of_int parking_spaces *. parking_rate_monthly

let base_rent =
  Series.growth_simple ~name:"Base Rent" ~start_date ~rate:rent_growth base_rent_monthly

let parking = Series.growth_simple ~name:"Parking" ~start_date ~rate:rent_growth parking_monthly

let other_income =
  Series.growth_simple ~name:"Other Income" ~start_date ~rate:rent_growth other_income_monthly

(* Operating Expenses (non-EGI-dependent) *)

let property_taxes =
  Series.growth_simple ~name:"Property Taxes" ~start_date ~rate:expense_growth
    (-.property_taxes_annual /. 12.0)

let insurance =
  Series.growth_simple ~name:"Insurance" ~start_date ~rate:expense_growth
    (-.insurance_annual /. 12.0)

let utilities =
  Series.growth_simple ~name:"Utilities" ~start_date ~rate:expense_growth (-.utilities_monthly)

let repairs_maintenance =
  Series.growth_simple ~name:"Repairs" ~start_date ~rate:expense_growth (-.repairs_monthly)

let janitorial =
  Series.growth_simple ~name:"Janitorial" ~start_date ~rate:expense_growth (-.janitorial_monthly)

let landscaping =
  Series.growth_simple ~name:"Landscaping" ~start_date ~rate:expense_growth (-.landscaping_monthly)

let security =
  Series.growth_simple ~name:"Security" ~start_date ~rate:expense_growth (-.security_monthly)

(* Revenue ↔ OpEx feedback loop *)

(* CAM recoveries depend on prior-period OpEx, but OpEx includes management
   (% of EGI), and EGI includes CAM. [feedback] breaks the cycle: the function
   receives the prior period's opex_total and returns the current definition. *)
let cam_recoveries, gross_potential_rent, vacancy_loss, egi, property_management, opex_total =
  Series.feedback ~default:0.0 (fun prev_opex ->
      let cam_recoveries =
        Series.map ~name:"CAM Recoveries"
          (fun prev ->
            if prev = 0.0 then cam_estimate_first else Float.abs prev *. cam_recovery_pct)
          prev_opex
      in
      let gross_potential_rent =
        Series.sum ~name:"GPR" [ base_rent; parking; cam_recoveries; other_income ]
      in
      let vacancy_loss =
        Series.named "Vacancy Loss" (Series.scale (-.vacancy_rate) gross_potential_rent)
      in
      let egi = Series.named "EGI" (Series.add gross_potential_rent vacancy_loss) in
      let property_management =
        Series.named "Property Management" (Series.scale (-.management_fee_pct) egi)
      in
      let opex_total =
        Series.sum ~name:"Total OpEx"
          [
            property_taxes;
            insurance;
            utilities;
            repairs_maintenance;
            property_management;
            janitorial;
            landscaping;
            security;
          ]
      in
      ( opex_total,
        (cam_recoveries, gross_potential_rent, vacancy_loss, egi, property_management, opex_total)
      ))

(* Net Operating Income *)

let noi = Series.named "NOI" (Series.add egi opex_total)

(* Capital Expenditures *)

let capital_reserves = Series.named "Capital Reserves" (Series.scale (-.reserve_pct) egi)
let ti_annual = ti_per_sf_annual *. building_sf
let tenant_improvements = Series.const ~name:"Tenant Improvements" (-.ti_annual /. 12.0)

let leasing_commissions =
  Series.named "Leasing Commissions" (Series.scale (-.leasing_commission_pct) egi)

let capex_total =
  Series.sum ~name:"Total CapEx" [ capital_reserves; tenant_improvements; leasing_commissions ]

(* Cash Flow Before Financing *)

let cfbf = Series.named "CFBF" (Series.add noi capex_total)

(* Debt Service *)

let monthly_payment =
  let r = interest_rate /. 12.0 in
  let n = float_of_int loan_term_months in
  let factor = (1.0 +. r) ** n in
  loan_amount *. (r *. factor) /. (factor -. 1.0)

let debt_total_pmt = Series.const ~name:"Debt Payment" (-.monthly_payment)
let year_fracs = Series.year_frac ~name:"Year Fracs" Daycount.actual_360

let _debt_balance, (debt_interest, debt_principal) =
  Series.feedback ~name:"Loan Balance" ~default:loan_amount (fun prev_bal ->
      let interest =
        Series.named "Interest Expense"
          (Series.scale (-.interest_rate) (Series.mul prev_bal year_fracs))
      in
      let principal = Series.named "Principal" (Series.sub debt_total_pmt interest) in
      let balance = Series.cumsum ~init:loan_amount principal in
      (balance, (balance, (interest, principal))))

let debt_service = Series.named "Total Debt Service" (Series.add debt_interest debt_principal)

(* Cash Flow After Financing *)

let cfaf = Series.named "CFAF" (Series.add cfbf debt_service)

(* Statement *)

(* Statement.group with an explicit ~total uses our pre-built Series for the
   group total row. Without ~total, Statement.eval would auto-sum the children,
   which works here too but explicit totals let us reuse the same series
   (e.g. opex_total) in other calculations like NOI. *)
let pro_forma_statement =
  let open Statement in
  group "Real Estate Pro Forma"
    [
      group ~total:gross_potential_rent "Gross Potential Rent"
        [
          line "Base Rent" base_rent;
          line "Parking Income" parking;
          line "CAM Recoveries" cam_recoveries;
          line "Other Income" other_income;
        ];
      line "Less: Vacancy & Credit Loss" vacancy_loss;
      line "Effective Gross Income" egi;
      group ~total:opex_total "Operating Expenses"
        [
          line "Property Taxes" property_taxes;
          line "Insurance" insurance;
          line "Utilities" utilities;
          line "Repairs & Maintenance" repairs_maintenance;
          line "Property Management" property_management;
          line "Janitorial" janitorial;
          line "Landscaping" landscaping;
          line "Security" security;
        ];
      line "Net Operating Income (NOI)" noi;
      group ~total:capex_total "Capital Expenditures"
        [
          line "Capital Reserves" capital_reserves;
          line "Tenant Improvements" tenant_improvements;
          line "Leasing Commissions" leasing_commissions;
        ];
      line "Cash Flow Before Financing" cfbf;
      group ~total:debt_service "Debt Service"
        [ line "Interest Expense" debt_interest; line "Principal Payment" debt_principal ];
      line "Cash Flow After Financing" cfaf;
    ]

(* Output *)

let print_assumptions () =
  Printf.printf "PROPERTY ASSUMPTIONS\n";
  Printf.printf "====================\n";
  Printf.printf "Building Size:           %.0f SF\n" building_sf;
  Printf.printf "Parking Spaces:          %d\n" parking_spaces;
  Printf.printf "Purchase Price:          $%.0f ($%.0f/SF)\n" purchase_price
    (purchase_price /. building_sf);
  Printf.printf "\n";
  Printf.printf "REVENUE ASSUMPTIONS\n";
  Printf.printf "====================\n";
  Printf.printf "Year 1 Base Rent:        $%.2f/SF/year\n" base_rent_per_sf_year1;
  Printf.printf "Rent Growth:             %.1f%%/year\n" (rent_growth *. 100.0);
  Printf.printf "Parking Rate:            $%.0f/space/month\n" parking_rate_monthly;
  Printf.printf "CAM Recovery:            %.0f%% of OpEx\n" (cam_recovery_pct *. 100.0);
  Printf.printf "Vacancy/Credit Loss:     %.0f%%\n" (vacancy_rate *. 100.0);
  Printf.printf "\n";
  Printf.printf "DEBT ASSUMPTIONS\n";
  Printf.printf "====================\n";
  Printf.printf "Loan Amount:             $%.0f (%.0f%% LTV)\n" loan_amount (ltv *. 100.0);
  Printf.printf "Interest Rate:           %.2f%%\n" (interest_rate *. 100.0);
  Printf.printf "Term:                    %d years\n" loan_term_years;
  Printf.printf "Monthly Payment:         $%.2f\n" monthly_payment;
  Printf.printf "\n\n"

let () =
  print_assumptions ();
  Printf.printf "PRO FORMA CASH FLOW PROJECTION\n";
  Printf.printf "==============================\n\n";
  let results = Statement.eval tl pro_forma_statement in
  let l =
    Statement.layout ~label_width:35 ~col_width:14
      ~col_header:(fun i -> Date.to_string (Timeline.period_start tl i))
      ~pp_num:(fun ppf v -> Format.fprintf ppf "%14.0f" v)
      ~sep:'=' tl
  in
  Statement.pp l Format.std_formatter results;

  (* Dependency graph *)
  let oc = open_out "model.dot" in
  let ppf = Format.formatter_of_out_channel oc in
  Series.Deps.pp_dot ppf [ cfaf ];
  Format.pp_print_flush ppf ();
  close_out oc
