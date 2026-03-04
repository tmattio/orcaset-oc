(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Balance: point-in-time quantities *)

type balance_hint =
  | BH_observations of { sorted_obs : (Date.t * float) array; before_first : float }
  | BH_roll_forward of {
      init : float;
      flow_eval : Timeline.t -> Flow.Materialized_internal.query_ctx * float array;
    }
  | BH_add of balance_hint * balance_hint
  | BH_sub of balance_hint * balance_hint
  | BH_scale of float * balance_hint
  | BH_neg of balance_hint
  | BH_map of (float -> float) * balance_hint
  | BH_map2 of (float -> float -> float) * balance_hint * balance_hint
  | BH_mul of balance_hint * balance_hint
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
  let sorted_obs = Array.of_list sorted in
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
              Array.iter
                (fun (d, _) ->
                  if Date.compare d tl_start < 0 || Date.compare d tl_end > 0
                  then
                    invalid_arg
                      (Printf.sprintf
                         "Balance.of_dates: observation date %s is outside the \
                          timeline"
                         (Date.to_string d)))
                sorted_obs;
              let last_val = ref before_first in
              let obs_idx = ref 0 in
              let obs_len = Array.length sorted_obs in
              for j = 0 to n - 1 do
                let period_end = Period.end_date (Timeline.get tl j) in
                while
                  !obs_idx < obs_len
                  && Date.compare (fst sorted_obs.(!obs_idx)) period_end <= 0
                do
                  last_val := snd sorted_obs.(!obs_idx);
                  incr obs_idx
                done;
                bins.(j) <- !last_val
              done;
              cache := Some (tl, bins);
              bins
        in
        bins.(i))
  in
  { formula; hint = BH_observations { sorted_obs; before_first } }

let of_observations = of_dates

(* Bridge from Flow *)

let roll_forward ?name ~init flow =
  let formula = Formula.cumsum ?name ~init (Flow.unsafe_to_formula flow) in
  let flow_eval tl =
    let m = Flow.eval tl flow in
    (Flow.Materialized_internal.query m, Flow.Materialized_internal.unsafe_values m)
  in
  { formula; hint = BH_roll_forward { init; flow_eval } }

let roll_forward_with ?name ~init f flow =
  mk (Formula.scan ?name ~init f (Flow.unsafe_to_formula flow))

(* Naming *)

let named name b = { formula = Formula.named name b.formula; hint = b.hint }

(* Algebra — preserves balance query provenance *)

let add a b =
  { formula = Formula.add a.formula b.formula; hint = BH_add (a.hint, b.hint) }

let sub a b =
  { formula = Formula.sub a.formula b.formula; hint = BH_sub (a.hint, b.hint) }

let scale k s =
  { formula = Formula.scale k s.formula; hint = BH_scale (k, s.hint) }

let neg s = { formula = Formula.neg s.formula; hint = BH_neg s.hint }

let map ?name f s =
  { formula = Formula.map ?name f s.formula; hint = BH_map (f, s.hint) }

let map2 ?name f a b =
  {
    formula = Formula.map2 ?name f a.formula b.formula;
    hint = BH_map2 (f, a.hint, b.hint);
  }

let mul a b =
  { formula = Formula.mul a.formula b.formula; hint = BH_mul (a.hint, b.hint) }

let div a b =
  {
    formula = Formula.div a.formula b.formula;
    hint = BH_map2 ((fun a b -> a /. b), a.hint, b.hint);
  }

let abs s =
  { formula = Formula.abs s.formula; hint = BH_map (Float.abs, s.hint) }

let min a b =
  {
    formula = Formula.min a.formula b.formula;
    hint = BH_map2 (Float.min, a.hint, b.hint);
  }

let max a b =
  {
    formula = Formula.max a.formula b.formula;
    hint = BH_map2 (Float.max, a.hint, b.hint);
  }

let clamp ~lo ~hi s =
  {
    formula = Formula.clamp ~lo ~hi s.formula;
    hint = BH_map ((fun x -> Float.min hi (Float.max lo x)), s.hint);
  }

let round digits s =
  let factor = 10.0 ** float_of_int digits in
  {
    formula = Formula.round digits s.formula;
    hint = BH_map ((fun x -> Float.round (x *. factor) /. factor), s.hint);
  }

let where ~cond ~then_ ~else_ =
  mk
    (Formula.where ~cond:(Flow.unsafe_to_formula cond) ~then_:then_.formula
       ~else_:else_.formula)

(* Cross-period *)

let prev ?name src ~default = mk (Formula.prev ?name src.formula ~default)
let at_period_start ?name b ~default = prev ?name b ~default
let at_period_end b = b

(* Bridge to Flow *)

let sample ?name b =
  let s =
    match name with Some n -> Formula.named n b.formula | None -> b.formula
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

(* Currency conversion — preserves hint *)

let convert ~rate s =
  { formula = Formula.convert ~rate s.formula; hint = BH_scale (rate, s.hint) }

(* Unsafe escape hatches *)

let unsafe_to_formula b = b.formula
let unsafe_of_formula s = { formula = s; hint = BH_no_hint }
let unsafe_of_array ?name arr = mk (Formula.of_array ?name arr)

(* Materialized *)

module Materialized = struct
  type balance_query =
    | BQ_observations of {
        sorted_obs : (Date.t * float) array;
        before_first : float;
      }
    | BQ_roll_forward of {
        init : float;
        balance_values : float array;
        flow_values : float array;
        flow_query : Flow.Materialized_internal.query_ctx;
        timeline : Timeline.t;
      }
    | BQ_add of balance_query * balance_query
    | BQ_sub of balance_query * balance_query
    | BQ_scale of float * balance_query
    | BQ_neg of balance_query
    | BQ_map of (float -> float) * balance_query
    | BQ_map2 of (float -> float -> float) * balance_query * balance_query
    | BQ_mul of balance_query * balance_query
    | BQ_cell_based

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

  (* Binary search on sorted observation array: last obs <= date *)
  let obs_at sorted_obs before_first date =
    let n = Array.length sorted_obs in
    let rec search lo hi best =
      if lo > hi then best
      else
        let mid = lo + ((hi - lo) / 2) in
        let d, v = sorted_obs.(mid) in
        if Date.compare d date <= 0 then search (mid + 1) hi v
        else search lo (mid - 1) best
    in
    search 0 (n - 1) before_first

  (* Recursive point-query through composed balance queries.
     Raises Exit on BQ_cell_based so caller can fall back. *)
  let rec at_query ?split_fn query date =
    match query with
    | BQ_observations { sorted_obs; before_first } ->
        obs_at sorted_obs before_first date
    | BQ_roll_forward
        { init; balance_values; flow_values; flow_query; timeline } -> (
        match Timeline.find_index timeline date with
        | None ->
            invalid_arg
              (Printf.sprintf
                 "Balance.Materialized.at: date %s is outside the timeline"
                 (Date.to_string date))
        | Some i ->
            let prev_bal =
              if i = 0 then init else balance_values.(i - 1)
            in
            let p = Timeline.get timeline i in
            let flow_to_date =
              match
                Flow.Materialized_internal.accrue_via_ctx flow_query
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
    | BQ_add (q1, q2) ->
        at_query ?split_fn q1 date +. at_query ?split_fn q2 date
    | BQ_sub (q1, q2) ->
        at_query ?split_fn q1 date -. at_query ?split_fn q2 date
    | BQ_scale (k, q) -> k *. at_query ?split_fn q date
    | BQ_neg q -> -.at_query ?split_fn q date
    | BQ_map (f, q) -> f (at_query ?split_fn q date)
    | BQ_map2 (f, q1, q2) ->
        f (at_query ?split_fn q1 date) (at_query ?split_fn q2 date)
    | BQ_mul (q1, q2) ->
        at_query ?split_fn q1 date *. at_query ?split_fn q2 date
    | BQ_cell_based -> raise_notrace Exit

  let at ?split_fn m date =
    match at_query ?split_fn m.query date with
    | v -> v
    | exception Exit -> (
        (* Fallback: current period value *)
        match Timeline.find_index m.timeline date with
        | None ->
            invalid_arg
              (Printf.sprintf
                 "Balance.Materialized.at: date %s is outside the timeline"
                 (Date.to_string date))
        | Some i -> m.values.(i))
end

(* Dependency graph *)

module Deps = struct
  type node = Flow.Deps.node = { id : int; name : string option; kind : string }
  type edge = Flow.Deps.edge = { src : int; dst : int }

  let graph ?named_only balances =
    let formulas = List.map (fun b -> b.formula) balances in
    let nodes, edges = Formula.Deps.graph ?named_only formulas in
    ( List.map (fun (n : Formula.Deps.node) ->
          { id = n.id; name = n.name; kind = n.kind }) nodes,
      List.map (fun (e : Formula.Deps.edge) ->
          { src = e.src; dst = e.dst }) edges )

  let pp_dot ?named_only ppf balances =
    Formula.Deps.pp_dot ?named_only ppf (List.map (fun b -> b.formula) balances)
end

(* Evaluation *)

let rec hint_to_query tl hint =
  match hint with
  | BH_observations { sorted_obs; before_first } ->
      Materialized.BQ_observations { sorted_obs; before_first }
  | BH_roll_forward { init; flow_eval } ->
      let flow_query, flow_values = flow_eval tl in
      (* Compute balance values from init + cumulative flow.
         This is correct because roll_forward is always cumsum. *)
      let n = Array.length flow_values in
      let balance_values = Array.make n 0.0 in
      let acc = ref init in
      for i = 0 to n - 1 do
        acc := !acc +. flow_values.(i);
        balance_values.(i) <- !acc
      done;
      Materialized.BQ_roll_forward
        {
          init;
          balance_values;
          flow_values;
          flow_query;
          timeline = tl;
        }
  | BH_add (h1, h2) ->
      Materialized.BQ_add (hint_to_query tl h1, hint_to_query tl h2)
  | BH_sub (h1, h2) ->
      Materialized.BQ_sub (hint_to_query tl h1, hint_to_query tl h2)
  | BH_scale (k, h) -> Materialized.BQ_scale (k, hint_to_query tl h)
  | BH_neg h -> Materialized.BQ_neg (hint_to_query tl h)
  | BH_map (f, h) -> Materialized.BQ_map (f, hint_to_query tl h)
  | BH_map2 (f, h1, h2) ->
      Materialized.BQ_map2 (f, hint_to_query tl h1, hint_to_query tl h2)
  | BH_mul (h1, h2) ->
      Materialized.BQ_mul (hint_to_query tl h1, hint_to_query tl h2)
  | BH_no_hint -> Materialized.BQ_cell_based

let eval tl b =
  let values = Formula.eval tl b.formula in
  let query = hint_to_query tl b.hint in
  { Materialized.timeline = tl; values; query }

let eval_values tl b = Formula.eval tl b.formula
