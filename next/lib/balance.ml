(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Balance: point-in-time quantities *)

type balance_hint =
  | BH_observations of (Date.t * float) list * float (* sorted obs, before_first *)
  | BH_roll_forward of { init : float; flow_eval : Timeline.t -> Flow.Materialized.query_ctx * float array }
  | BH_no_hint

type 'c t = { formula : 'c Formula.t; hint : balance_hint }

let mk ?hint formula =
  { formula; hint = (match hint with Some h -> h | None -> BH_no_hint) }

(* Constructors *)

let const ?name v = mk (Formula.const ?name v)
let init ?name f = mk (Formula.init_flow ?name f)

let of_dates ?name ?(before_first = 0.0) observations =
  let sorted =
    List.sort (fun (d1, _) (d2, _) -> Date.compare d1 d2) observations
  in
  let cache = ref None in
  let formula =
    Formula.init_tl ?name (fun tl i _period ->
        let bins =
          match !cache with
          | Some (cached_tl, bins) when cached_tl == tl -> bins
          | _ ->
              let n = Timeline.length tl in
              let bins = Array.make n 0.0 in
              let tl_start = Period.start_date (Timeline.get tl 0) in
              let tl_end = Period.end_date (Timeline.get tl (n - 1)) in
              List.iter
                (fun (d, _) ->
                  if Date.compare d tl_start < 0 || Date.compare d tl_end > 0
                  then
                    invalid_arg
                      (Printf.sprintf
                         "Balance.of_dates: observation date %s is outside the \
                          timeline"
                         (Date.to_string d)))
                sorted;
              let last_val = ref before_first in
              let obs = ref sorted in
              for j = 0 to n - 1 do
                let period_end = Period.end_date (Timeline.get tl j) in
                let rec drain = function
                  | (d, v) :: rest when Date.compare d period_end <= 0 ->
                      last_val := v;
                      drain rest
                  | remaining -> obs := remaining
                in
                drain !obs;
                bins.(j) <- !last_val
              done;
              cache := Some (tl, bins);
              bins
        in
        bins.(i))
  in
  { formula; hint = BH_observations (sorted, before_first) }

let of_observations = of_dates

(* Bridge from Flow *)

let roll_forward ?name ~init flow =
  let formula = Formula.cumsum ?name ~init (Flow.unsafe_to_formula flow) in
  let flow_eval tl =
    let m = Flow.eval tl flow in
    (Flow.Materialized.query m, Flow.Materialized.unsafe_values m)
  in
  { formula; hint = BH_roll_forward { init; flow_eval } }

let roll_forward_with ?name ~init f flow =
  mk (Formula.scan ?name ~init f (Flow.unsafe_to_formula flow))

(* Naming *)

let named name b = { formula = Formula.named name b.formula; hint = b.hint }

(* Algebra *)

let add a b = mk (Formula.add a.formula b.formula)
let sub a b = mk (Formula.sub a.formula b.formula)
let scale k s = mk (Formula.scale k s.formula)
let map ?name f s = mk (Formula.map ?name f s.formula)
let map2 ?name f a b = mk (Formula.map2 ?name f a.formula b.formula)
let mul a b = mk (Formula.mul a.formula b.formula)

(* Cross-period *)

let prev ?name src ~default = mk (Formula.prev ?name src.formula ~default)
let at_period_start ?name b ~default = prev ?name b ~default
let at_period_end b = b

(* Bridge to Flow *)

let sample ?name b =
  let s =
    match name with
    | Some n -> Formula.named n b.formula
    | None -> b.formula
  in
  Flow.unsafe_of_formula s

let change b ~default =
  let prev_s = Formula.prev b.formula ~default in
  Flow.unsafe_of_formula (Formula.sub b.formula prev_s)

(* Feedback *)

let feedback ?name ~default f =
  Formula.feedback ?name ~default (fun prev_formula ->
      let def, exposed = f { formula = prev_formula; hint = BH_no_hint } in
      (def.formula, exposed))

let fixpoint ?name ?tol ?max_iter ~guess f =
  mk
    (Formula.fixpoint ?name ?tol ?max_iter ~guess (fun var ->
         let body = f { formula = var; hint = BH_no_hint } in
         body.formula))

(* Currency conversion *)

let convert ~rate s = mk (Formula.convert ~rate s.formula)

(* Unsafe escape hatches *)

let unsafe_to_formula b = b.formula
let unsafe_of_formula s = { formula = s; hint = BH_no_hint }
let unsafe_of_array ?name arr = mk (Formula.of_array ?name arr)

(* Materialized *)

module Materialized = struct
  type balance_query =
    | BQ_observations of (Date.t * float) list * float
    | BQ_roll_forward of {
        init : float;
        flow_values : float array;
        flow_query : Flow.Materialized.query_ctx;
        timeline : Timeline.t;
      }
    | BQ_cell_based

  type interp = Step | Series

  type 'c t = {
    timeline : Timeline.t;
    values : float array;
    query : balance_query;
  }

  let make tl values =
    if Array.length values <> Timeline.length tl then
      invalid_arg
        "Balance.Materialized.make: array length does not match timeline length";
    { timeline = tl; values; query = BQ_cell_based }

  let timeline m = m.timeline
  let to_array m = Array.copy m.values
  let unsafe_values m = m.values
  let length m = Array.length m.values
  let get m i = m.values.(i)
  let period m i = Timeline.get m.timeline i

  let to_list m =
    let n = length m in
    let rec loop acc i =
      if i < 0 then acc
      else loop ((period m i, m.values.(i)) :: acc) (i - 1)
    in
    loop [] (n - 1)

  let fold f init m =
    let acc = ref init in
    for i = 0 to Array.length m.values - 1 do
      acc := f !acc (Timeline.get m.timeline i) m.values.(i)
    done;
    !acc

  let iter f m =
    for i = 0 to Array.length m.values - 1 do
      f (Timeline.get m.timeline i) m.values.(i)
    done

  let at ?(interp = Series) ?split_fn m date =
    let tl = m.timeline in
    match interp with
    | Step -> Formula.Query.interpolate ?split_fn tl m.values date
    | Series -> (
        match m.query with
        | BQ_observations (obs, before_first) ->
            (* Binary search for last observation <= date *)
            let rec search lo hi best =
              if lo > hi then best
              else
                let mid = lo + ((hi - lo) / 2) in
                let d, v = List.nth obs mid in
                if Date.compare d date <= 0 then search (mid + 1) hi v
                else search lo (mid - 1) best
            in
            search 0 (List.length obs - 1) before_first
        | BQ_roll_forward { init; flow_values; flow_query; timeline } -> (
            match Timeline.find_index timeline date with
            | None ->
                invalid_arg
                  (Printf.sprintf
                     "Balance.Materialized.at: date %s is outside the timeline"
                     (Date.to_string date))
            | Some i ->
                let prev_bal =
                  if i = 0 then init else m.values.(i - 1)
                in
                let p = Timeline.get timeline i in
                let flow_to_date =
                  match
                    Flow.Materialized.accrue_via_ctx flow_query
                      ~start_date:(Period.start_date p) ~end_date:date
                  with
                  | v -> v
                  | exception Exit ->
                      let sf =
                        match split_fn with
                        | Some f -> f
                        | None -> Formula.Query.default_split_fn
                      in
                      let before, _ =
                        sf ~start_date:(Period.start_date p)
                          ~end_date:(Period.end_date p) ~split_date:date
                          ~value:flow_values.(i)
                      in
                      before
                in
                prev_bal +. flow_to_date)
        | BQ_cell_based ->
            Formula.Query.interpolate ?split_fn tl m.values date)
end

(* Evaluation *)

let eval tl b =
  match b.hint with
  | BH_roll_forward { init; flow_eval } ->
      let bal_values = Formula.eval tl b.formula in
      let flow_query, flow_values = flow_eval tl in
      {
        Materialized.timeline = tl;
        values = bal_values;
        query =
          Materialized.BQ_roll_forward
            { init; flow_values; flow_query; timeline = tl };
      }
  | BH_observations (obs, before_first) ->
      let values = Formula.eval tl b.formula in
      {
        Materialized.timeline = tl;
        values;
        query = Materialized.BQ_observations (obs, before_first);
      }
  | BH_no_hint ->
      let values = Formula.eval tl b.formula in
      { Materialized.timeline = tl; values; query = Materialized.BQ_cell_based }

let eval_values tl b = Formula.eval tl b.formula
