(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Shared typed AST for flows and balances.

   Design notes:
   - The AST is the single source of truth for both evaluation and queryability.
   - We keep public Flow/Balance wrappers, but remove the old hint/query shadow
     trees.
   - A dedicated [Roll_forward] node is the private typed-AST improvement that
     gives balances intrinsic point-in-time semantics without duplicating a
     second provenance structure. *)

type flow_kind
type balance_kind
type pointwise_kind

type _ kind =
  | Flow_k : flow_kind kind
  | Balance_k : balance_kind kind
  | Pointwise_k : pointwise_kind kind

type prorater = Prorater.t

let default_prorater = Prorater.actual_days

let next_id =
  let counter = Atomic.make 0 in
  fun () -> Atomic.fetch_and_add counter 1

type ('k, 'c) t = { id : int; name : string option; kind : 'k kind; node : ('k, 'c) node }
and any_series = Any_series : ('k, 'c) t -> any_series
and ('k, 'c) delay = { mutable resolved : ('k, 'c) t option; thunk : unit -> ('k, 'c) t }

and ('k, 'c) node =
  | Const of float
  | Of_array of float array
  | Init of (Timeline.t -> int -> Period.t -> float)
  | Growth_simple of {
      start_date : Date.t;
      daycount : Date.t -> Date.t -> float;
      rate : float;
      initial : float;
    }
  | Growth_compound of {
      start_date : Date.t;
      daycount : Date.t -> Date.t -> float;
      rate : float;
      initial : float;
    }
  | Year_frac of (Date.t -> Date.t -> float)
  | Events of { events : (Date.t * float) list; cache : (Timeline.t * float array) option ref }
  | Source_periods of {
      pairs : (Period.t * float) list;
      prorater : prorater;
      cache : (Timeline.t * float array) option ref;
    }
  | Observations of {
      sorted_obs : (Date.t * float) array;
      before_first : float;
      cache : (Timeline.t * float array) option ref;
    }
  | Add of ('k, 'c) t * ('k, 'c) t
  | Sub of ('k, 'c) t * ('k, 'c) t
  | Scale of float * ('k, 'c) t
  | Neg of ('k, 'c) t
  | Map of (float -> float) * ('k, 'c) t
  | Map2 of (float -> float -> float) * ('k, 'c) t * ('k, 'c) t
  | Sum of ('k, 'c) t list
  | Where of { cond : any_series; then_ : ('k, 'c) t; else_ : ('k, 'c) t }
  | Prev of { src : ('k, 'c) t; default : float }
  | Scan_flow of { init : float; f : acc:float -> x:float -> float; flow : (flow_kind, 'c) t }
  | Accumulate of { init : float; f : acc:float -> x:float -> float; flow : (flow_kind, 'c) t }
  | Roll_forward of { init : float; flow : (flow_kind, 'c) t }
  | Pointwise_of_flow of (flow_kind, 'c) t
  | Pointwise_of_balance of (balance_kind, 'c) t
  | Flow_of_pointwise of (pointwise_kind, 'c) t
  | Change_balance of { balance : (balance_kind, 'c) t; default : float }
  | Delay of ('k, 'c) delay
  | Var of float ref
  | Fixpoint of { var : ('k, 'c) t; body : ('k, 'c) t; tol : float; max_iter : int; guess : float }

type packed = Pack : ('k, 'c) t -> packed

let kind s = s.kind
let mk ?name kind node = { id = next_id (); name; kind; node }

let resolve s =
  match s.node with
  | Delay d -> (
      match d.resolved with
      | Some resolved -> resolved.node
      | None ->
          let resolved = d.thunk () in
          d.resolved <- Some resolved;
          resolved.node)
  | node -> node

let named name s = { s with name = Some name }

(* Constructors *)

let const ?name kind v = mk ?name kind (Const v)
let of_array ?name kind arr = mk ?name kind (Of_array arr)
let init_tl ?name kind f = mk ?name kind (Init f)
let init ?name kind f = init_tl ?name kind (fun _tl i p -> f i p)

let growth_simple ?name ?(daycount = Daycount.actual_360) ~start_date ~rate initial =
  mk ?name Flow_k (Growth_simple { start_date; daycount; rate; initial })

let growth_compound ?name ?(daycount = Daycount.actual_360) ~start_date ~rate initial =
  mk ?name Flow_k (Growth_compound { start_date; daycount; rate; initial })

let year_frac ?name daycount = mk ?name Flow_k (Year_frac daycount)
let of_events ?name events = mk ?name Flow_k (Events { events; cache = ref None })

let of_periods ?name ?(prorater = default_prorater) pairs =
  mk ?name Flow_k (Source_periods { pairs; prorater; cache = ref None })

let of_observations ?name ?(before_first = 0.0) observations =
  let sorted = List.sort (fun (d1, _) (d2, _) -> Date.compare d1 d2) observations in
  let sorted_obs = Array.of_list sorted in
  mk ?name Balance_k (Observations { sorted_obs; before_first; cache = ref None })

(* Pointwise combinators.  The simplified design intentionally keeps only a
   small amount of construction-time simplification. *)

let map ?name f s = mk ?name s.kind (Map (f, s))
let map2 ?name f a b = mk ?name a.kind (Map2 (f, a, b))
let add a b = mk a.kind (Add (a, b))
let sub a b = mk a.kind (Sub (a, b))
let mul a b = mk a.kind (Map2 (( *. ), a, b))
let div a b = mk a.kind (Map2 (( /. ), a, b))
let neg s = mk s.kind (Neg s)
let scale k s = if k = 1.0 then s else mk s.kind (Scale (k, s))
let abs s = map Float.abs s
let min a b = map2 Float.min a b
let max a b = map2 Float.max a b
let clamp ~lo ~hi s = map (fun x -> Float.min hi (Float.max lo x)) s

let round digits s =
  let factor = 10.0 ** float_of_int digits in
  map (fun x -> Float.round (x *. factor) /. factor) s

let sum ?name kind ss =
  match ss with
  | [] -> const ?name kind 0.0
  | [ s ] -> ( match name with Some n -> named n s | None -> s)
  | _ -> mk ?name kind (Sum ss)

let where ~cond ~then_ ~else_ = mk then_.kind (Where { cond = Any_series cond; then_; else_ })

(* Cross-period / bridges *)

let prev ?name src ~default = mk ?name src.kind (Prev { src; default })
let scan ?name ~init f flow = mk ?name Flow_k (Scan_flow { init; f; flow })
let accumulate ?name ~init f flow = mk ?name Balance_k (Accumulate { init; f; flow })
let roll_forward ?name ~init flow = mk ?name Balance_k (Roll_forward { init; flow })
let pointwise_of_flow ?name flow = mk ?name Pointwise_k (Pointwise_of_flow flow)
let pointwise_of_balance ?name balance = mk ?name Pointwise_k (Pointwise_of_balance balance)
let flow_of_pointwise ?name pointwise = mk ?name Flow_k (Flow_of_pointwise pointwise)
let change_balance ?name balance ~default = mk ?name Flow_k (Change_balance { balance; default })
let convert ~rate s = (Obj.magic (scale rate s) : (_, _) t)

(* Feedback / fixpoint *)

exception Cycle_error of { formula_name : string option; period_index : int }

exception
  Convergence_error of { formula_name : string option; period_index : int; iterations : int }

let delay ?name kind thunk = mk ?name kind (Delay { resolved = None; thunk })

let feedback ?name ~kind ~default f =
  let self_ref = ref (const kind 0.0) in
  let prev_self = delay ?name kind (fun () -> prev !self_ref ~default) in
  let definition, exposed = f prev_self in
  self_ref := definition;
  exposed

let fixpoint ?name ~kind ?(tol = 1e-10) ?(max_iter = 100) ~guess f =
  let r = ref 0.0 in
  let var = mk kind (Var r) in
  let body = f var in
  mk ?name kind (Fixpoint { var; body; tol; max_iter; guess })

(* Evaluation engine *)

type cell_state = In_progress | Done of float
type eval_ctx = { tl : Timeline.t; n : int; memo : (int, cell_state) Hashtbl.t }

let make_ctx tl =
  let n = Timeline.length tl in
  { tl; n; memo = Hashtbl.create (Stdlib.max 32 (n * 8)) }

let key_for ctx s i = (s.id * ctx.n) + i

let events_bins tl events =
  let n = Timeline.length tl in
  let bins = Array.make n 0.0 in
  List.iter
    (fun (d, v) ->
      match Timeline.find_index tl d with
      | Some idx -> bins.(idx) <- bins.(idx) +. v
      | None ->
          invalid_arg
            (Printf.sprintf "Formula.of_events: event date %s is outside the timeline"
               (Date.to_string d)))
    events;
  bins

let source_period_bins tl pairs prorater =
  let n = Timeline.length tl in
  let bins = Array.make n 0.0 in
  List.iter
    (fun (src_p, v) ->
      for i = 0 to n - 1 do
        let dst_p = Timeline.get tl i in
        let ov_start = Date.max (Period.start_date src_p) (Period.start_date dst_p) in
        let ov_end = Date.min (Period.end_date src_p) (Period.end_date dst_p) in
        if Date.compare ov_start ov_end < 0 then begin
          let frac =
            prorater ~start_date:(Period.start_date src_p) ~end_date:(Period.end_date src_p)
              ~sub_start:ov_start ~sub_end:ov_end
          in
          bins.(i) <- bins.(i) +. (v *. frac)
        end
      done)
    pairs;
  bins

let observation_bins tl sorted_obs before_first =
  let n = Timeline.length tl in
  let bins = Array.make n 0.0 in
  let tl_start = Period.start_date (Timeline.get tl 0) in
  let tl_end = Period.end_date (Timeline.get tl (n - 1)) in
  Array.iter
    (fun (d, _) ->
      if Date.compare d tl_start < 0 || Date.compare d tl_end > 0 then
        invalid_arg
          (Printf.sprintf "Balance.of_dates: observation date %s is outside the timeline"
             (Date.to_string d)))
    sorted_obs;
  let last_val = ref before_first in
  let obs_idx = ref 0 in
  let obs_len = Array.length sorted_obs in
  for i = 0 to n - 1 do
    let period_end = Period.end_date (Timeline.get tl i) in
    while !obs_idx < obs_len && Date.compare (fst sorted_obs.(!obs_idx)) period_end <= 0 do
      last_val := snd sorted_obs.(!obs_idx);
      incr obs_idx
    done;
    bins.(i) <- !last_val
  done;
  bins

let cached_bins cache tl build =
  match !cache with
  | Some (cached_tl, bins) when cached_tl == tl -> bins
  | _ ->
      let bins = build () in
      cache := Some (tl, bins);
      bins

let rec invalidate_period : type k c. eval_ctx -> (k, c) t -> int -> unit =
 fun ctx s i ->
  match s.node with
  | Const _ | Var _ | Of_array _ | Init _ | Growth_simple _ | Growth_compound _ | Year_frac _
  | Events _ | Source_periods _ | Observations _ ->
      ()
  | _ ->
      let key = key_for ctx s i in
      if Hashtbl.mem ctx.memo key then begin
        Hashtbl.remove ctx.memo key;
        match resolve s with
        | Const _ | Var _ | Of_array _ | Init _ | Growth_simple _ | Growth_compound _ | Year_frac _
        | Events _ | Source_periods _ | Observations _ ->
            ()
        | Add (a, b) | Sub (a, b) ->
            invalidate_period ctx a i;
            invalidate_period ctx b i
        | Scale (_, src) | Neg src | Map (_, src) -> invalidate_period ctx src i
        | Map2 (_, a, b) ->
            invalidate_period ctx a i;
            invalidate_period ctx b i
        | Sum ss -> List.iter (fun src -> invalidate_period ctx src i) ss
        | Where { cond = Any_series cond; then_; else_ } ->
            invalidate_period ctx cond i;
            invalidate_period ctx then_ i;
            invalidate_period ctx else_ i
        | Pointwise_of_flow src -> invalidate_period ctx src i
        | Pointwise_of_balance src -> invalidate_period ctx src i
        | Flow_of_pointwise src -> invalidate_period ctx src i
        | Change_balance { balance; _ } -> invalidate_period ctx balance i
        | Prev _ | Scan_flow _ | Accumulate _ | Roll_forward _ | Fixpoint _ | Delay _ -> ()
      end

and eval_cell : type k c. eval_ctx -> (k, c) t -> int -> float =
 fun ctx s i ->
  match s.node with
  | Const v -> v
  | Var r -> !r
  | _ -> (
      let key = key_for ctx s i in
      match Hashtbl.find_opt ctx.memo key with
      | Some (Done v) -> v
      | Some In_progress -> raise (Cycle_error { formula_name = s.name; period_index = i })
      | None ->
          Hashtbl.replace ctx.memo key In_progress;
          let v = compute ctx s i in
          Hashtbl.replace ctx.memo key (Done v);
          v)

and compute : type k c. eval_ctx -> (k, c) t -> int -> float =
 fun ctx s i ->
  match resolve s with
  | Const v -> v
  | Of_array arr -> if i < Array.length arr then arr.(i) else 0.0
  | Init f ->
      let period = Timeline.get ctx.tl i in
      f ctx.tl i period
  | Growth_simple { start_date; daycount; rate; initial } ->
      let p = Timeline.get ctx.tl i in
      initial *. (1.0 +. (rate *. daycount start_date (Period.start_date p)))
  | Growth_compound { start_date; daycount; rate; initial } ->
      let p = Timeline.get ctx.tl i in
      initial *. ((1.0 +. rate) ** daycount start_date (Period.start_date p))
  | Year_frac daycount ->
      let p = Timeline.get ctx.tl i in
      daycount (Period.start_date p) (Period.end_date p)
  | Events { events; cache } ->
      let bins = cached_bins cache ctx.tl (fun () -> events_bins ctx.tl events) in
      bins.(i)
  | Source_periods { pairs; prorater; cache } ->
      let bins = cached_bins cache ctx.tl (fun () -> source_period_bins ctx.tl pairs prorater) in
      bins.(i)
  | Observations { sorted_obs; before_first; cache } ->
      let bins =
        cached_bins cache ctx.tl (fun () -> observation_bins ctx.tl sorted_obs before_first)
      in
      bins.(i)
  | Add (a, b) -> eval_cell ctx a i +. eval_cell ctx b i
  | Sub (a, b) -> eval_cell ctx a i -. eval_cell ctx b i
  | Scale (k, src) -> k *. eval_cell ctx src i
  | Neg src -> -.eval_cell ctx src i
  | Map (f, src) -> f (eval_cell ctx src i)
  | Map2 (f, a, b) -> f (eval_cell ctx a i) (eval_cell ctx b i)
  | Sum ss -> List.fold_left (fun acc src -> acc +. eval_cell ctx src i) 0.0 ss
  | Where { cond = Any_series cond; then_; else_ } ->
      if eval_cell ctx cond i <> 0.0 then eval_cell ctx then_ i else eval_cell ctx else_ i
  | Prev { src; default } -> if i = 0 then default else eval_cell ctx src (i - 1)
  | Scan_flow { init; f; flow } ->
      let acc = if i = 0 then init else eval_cell ctx s (i - 1) in
      let x = eval_cell ctx flow i in
      f ~acc ~x
  | Accumulate { init; f; flow } ->
      let acc = if i = 0 then init else eval_cell ctx s (i - 1) in
      let x = eval_cell ctx flow i in
      f ~acc ~x
  | Roll_forward { init; flow } ->
      let acc = if i = 0 then init else eval_cell ctx s (i - 1) in
      acc +. eval_cell ctx flow i
  | Pointwise_of_flow flow -> eval_cell ctx flow i
  | Pointwise_of_balance balance -> eval_cell ctx balance i
  | Flow_of_pointwise pointwise -> eval_cell ctx pointwise i
  | Change_balance { balance; default } ->
      let prev = if i = 0 then default else eval_cell ctx balance (i - 1) in
      eval_cell ctx balance i -. prev
  | Var r -> !r
  | Fixpoint { var; body; tol; max_iter; guess } ->
      let r = match var.node with Var r -> r | _ -> assert false in
      let initial = if i = 0 then guess else eval_cell ctx s (i - 1) in
      let rec iterate x iter =
        if iter >= max_iter then
          raise
            (Convergence_error { formula_name = s.name; period_index = i; iterations = max_iter })
        else begin
          r := x;
          invalidate_period ctx var i;
          invalidate_period ctx body i;
          let x' = eval_cell ctx body i in
          if Float.abs (x' -. x) <= tol then x' else iterate x' (iter + 1)
        end
      in
      iterate initial 0
  | Delay _ -> failwith "Formula.compute: unexpected unresolved Delay"

let eval_with_ctx ctx s = Array.init ctx.n (fun i -> eval_cell ctx s i)

let eval tl s =
  let ctx = make_ctx tl in
  eval_with_ctx ctx s

let eval_many tl ss =
  let ctx = make_ctx tl in
  List.map (fun s -> eval_with_ctx ctx s) ss

let eval_many_packed tl ss =
  let ctx = make_ctx tl in
  List.map (fun (Pack s) -> eval_with_ctx ctx s) ss

(* Materialized *)

module Materialized = struct
  type ('k, 'c) t = { timeline : Timeline.t; values : float array }

  let make timeline values =
    let tl_len = Timeline.length timeline in
    let arr_len = Array.length values in
    if arr_len <> tl_len then
      invalid_arg
        (Printf.sprintf
           "Formula.Materialized.make: array length %d does not match timeline length %d" arr_len
           tl_len);
    { timeline; values }

  let timeline m = m.timeline
  let to_array m = Array.copy m.values
  let unsafe_values m = m.values
  let length m = Array.length m.values
  let get m i = m.values.(i)
  let period m i = Timeline.get m.timeline i

  let to_list m =
    List.init (Array.length m.values) (fun i -> (Timeline.get m.timeline i, m.values.(i)))

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
end

let eval_materialized tl s = Materialized.make tl (eval tl s)

(* Query helpers *)

module Query = struct
  type nonrec prorater = prorater

  let default_prorater = default_prorater

  let before_date ?(prorater = default_prorater) tl values date =
    match Timeline.find_index tl date with
    | None ->
        invalid_arg
          (Printf.sprintf "Formula.Query.before_date: date %s is outside the timeline"
             (Date.to_string date))
    | Some i ->
        let p = Timeline.get tl i in
        let frac =
          prorater ~start_date:(Period.start_date p) ~end_date:(Period.end_date p)
            ~sub_start:(Period.start_date p) ~sub_end:date
        in
        values.(i) *. frac

  let accrue ?(prorater = default_prorater) tl values ~start_date ~end_date =
    if Date.compare end_date start_date <= 0 then 0.0
    else begin
      let total = ref 0.0 in
      for i = 0 to Timeline.length tl - 1 do
        let p = Timeline.get tl i in
        let ov_start = Date.max start_date (Period.start_date p) in
        let ov_end = Date.min end_date (Period.end_date p) in
        if Date.compare ov_start ov_end < 0 then begin
          let frac =
            prorater ~start_date:(Period.start_date p) ~end_date:(Period.end_date p)
              ~sub_start:ov_start ~sub_end:ov_end
          in
          total := !total +. (values.(i) *. frac)
        end
      done;
      !total
    end
end

(* Exact query capability derivation from the AST.

   These functions intentionally describe only what is semantically exact.  The
   public Flow / Balance materialized values fall back to [Query] or to the
   enclosing period's cell value when exact querying is unavailable. *)

type query_mode = Exact | Approx

type flow_query = {
  mode : query_mode;
  accrue : prorater:prorater -> start_date:Date.t -> end_date:Date.t -> float;
}

type balance_query = { mode : query_mode; at : prorater:prorater -> Date.t -> float }

let accrue_events events ~start_date ~end_date =
  List.fold_left
    (fun acc (d, v) ->
      if Date.compare d start_date >= 0 && Date.compare d end_date < 0 then acc +. v else acc)
    0.0 events

let accrue_source_periods pairs prorater ~start_date ~end_date =
  List.fold_left
    (fun acc (src_p, v) ->
      let ov_start = Date.max start_date (Period.start_date src_p) in
      let ov_end = Date.min end_date (Period.end_date src_p) in
      if Date.compare ov_start ov_end >= 0 then acc
      else
        let frac =
          prorater ~start_date:(Period.start_date src_p) ~end_date:(Period.end_date src_p)
            ~sub_start:ov_start ~sub_end:ov_end
        in
        acc +. (v *. frac))
    0.0 pairs

let obs_at sorted_obs before_first date =
  let n = Array.length sorted_obs in
  let rec search lo hi best =
    if lo > hi then best
    else
      let mid = lo + ((hi - lo) / 2) in
      let d, v = sorted_obs.(mid) in
      if Date.compare d date <= 0 then search (mid + 1) hi v else search lo (mid - 1) best
  in
  search 0 (n - 1) before_first

let exact_flow_accrual : type c.
    (flow_kind, c) t -> (start_date:Date.t -> end_date:Date.t -> float) option =
 fun s ->
  let visiting = Hashtbl.create 32 in
  let rec build : type c. (flow_kind, c) t -> (start_date:Date.t -> end_date:Date.t -> float) option
      =
   fun s ->
    if Hashtbl.mem visiting s.id then None
    else begin
      Hashtbl.replace visiting s.id ();
      let result =
        match resolve s with
        | Events { events; _ } ->
            Some (fun ~start_date ~end_date -> accrue_events events ~start_date ~end_date)
        | Source_periods { pairs; prorater; _ } ->
            Some
              (fun ~start_date ~end_date ->
                accrue_source_periods pairs prorater ~start_date ~end_date)
        | Add (a, b) -> (
            match (build a, build b) with
            | Some fa, Some fb ->
                Some
                  (fun ~start_date ~end_date ->
                    fa ~start_date ~end_date +. fb ~start_date ~end_date)
            | _ -> None)
        | Sub (a, b) -> (
            match (build a, build b) with
            | Some fa, Some fb ->
                Some
                  (fun ~start_date ~end_date ->
                    fa ~start_date ~end_date -. fb ~start_date ~end_date)
            | _ -> None)
        | Scale (k, src) ->
            Option.map (fun f ~start_date ~end_date -> k *. f ~start_date ~end_date) (build src)
        | Neg src ->
            Option.map (fun f ~start_date ~end_date -> -.f ~start_date ~end_date) (build src)
        | Sum ss ->
            let accs = List.map build ss in
            if List.exists Option.is_none accs then None
            else
              let fs = List.map Option.get accs in
              Some
                (fun ~start_date ~end_date ->
                  List.fold_left (fun acc f -> acc +. f ~start_date ~end_date) 0.0 fs)
        | Map _ | Map2 _ | Of_array _ | Init _ | Growth_simple _ | Growth_compound _ | Year_frac _
        | Where _ | Prev _ | Scan_flow _ | Accumulate _ | Roll_forward _ | Pointwise_of_flow _
        | Pointwise_of_balance _ | Flow_of_pointwise _ | Change_balance _ | Var _ | Fixpoint _
        | Const _ | Observations _ ->
            None
        | Delay _ -> assert false
      in
      Hashtbl.remove visiting s.id;
      result
    end
  in
  build s

let approx_flow_query tl values =
  {
    mode = Approx;
    accrue =
      (fun ~prorater ~start_date ~end_date ->
        Query.accrue ~prorater tl values ~start_date ~end_date);
  }

let flow_query _tl values s =
  match exact_flow_accrual s with
  | Some accrue ->
      {
        mode = Exact;
        accrue = (fun ~prorater:_ ~start_date ~end_date -> accrue ~start_date ~end_date);
      }
  | None -> approx_flow_query _tl values

let approx_balance_query tl values =
  let outside date =
    invalid_arg
      (Printf.sprintf "Formula.balance_query: date %s is outside the timeline" (Date.to_string date))
  in
  {
    mode = Approx;
    at =
      (fun ~prorater:_ date ->
        match Timeline.find_index tl date with None -> outside date | Some i -> values.(i));
  }

let balance_query tl values s =
  let outside date =
    invalid_arg
      (Printf.sprintf "Formula.balance_query: date %s is outside the timeline" (Date.to_string date))
  in
  let visiting = Hashtbl.create 32 in
  let rec build : type c. (balance_kind, c) t -> balance_query =
   fun s ->
    if Hashtbl.mem visiting s.id then approx_balance_query tl values
    else begin
      Hashtbl.replace visiting s.id ();
      let result =
        match resolve s with
        | Const v -> { mode = Exact; at = (fun ~prorater:_ _date -> v) }
        | Observations { sorted_obs; before_first; _ } ->
            { mode = Exact; at = (fun ~prorater:_ date -> obs_at sorted_obs before_first date) }
        | Add (a, b) ->
            let qa = build a in
            let qb = build b in
            if qa.mode = Exact && qb.mode = Exact then
              {
                mode = Exact;
                at = (fun ~prorater date -> qa.at ~prorater date +. qb.at ~prorater date);
              }
            else approx_balance_query tl values
        | Sub (a, b) ->
            let qa = build a in
            let qb = build b in
            if qa.mode = Exact && qb.mode = Exact then
              {
                mode = Exact;
                at = (fun ~prorater date -> qa.at ~prorater date -. qb.at ~prorater date);
              }
            else approx_balance_query tl values
        | Scale (k, src) ->
            let q = build src in
            if q.mode = Exact then
              { mode = Exact; at = (fun ~prorater date -> k *. q.at ~prorater date) }
            else approx_balance_query tl values
        | Neg src ->
            let q = build src in
            if q.mode = Exact then
              { mode = Exact; at = (fun ~prorater date -> -.q.at ~prorater date) }
            else approx_balance_query tl values
        | Map (f, src) ->
            let q = build src in
            if q.mode = Exact then
              { mode = Exact; at = (fun ~prorater date -> f (q.at ~prorater date)) }
            else approx_balance_query tl values
        | Map2 (f, a, b) ->
            let qa = build a in
            let qb = build b in
            if qa.mode = Exact && qb.mode = Exact then
              {
                mode = Exact;
                at = (fun ~prorater date -> f (qa.at ~prorater date) (qb.at ~prorater date));
              }
            else approx_balance_query tl values
        | Sum ss ->
            let qs = List.map build ss in
            if List.for_all (fun q -> q.mode = Exact) qs then
              {
                mode = Exact;
                at =
                  (fun ~prorater date ->
                    List.fold_left (fun acc q -> acc +. q.at ~prorater date) 0.0 qs);
              }
            else approx_balance_query tl values
        | Where _ -> approx_balance_query tl values
        | Prev { src; default } ->
            let q = build src in
            if q.mode = Exact then
              {
                mode = Exact;
                at =
                  (fun ~prorater date ->
                    match Timeline.find_index tl date with
                    | None -> outside date
                    | Some i ->
                        if i = 0 then default
                        else
                          let d = Timeline.period_end tl (i - 1) in
                          q.at ~prorater d);
              }
            else approx_balance_query tl values
        | Roll_forward { init; flow } ->
            let self_values = lazy (eval tl s) in
            let flow_values = lazy (eval tl flow) in
            let flow_query = flow_query tl (Lazy.force flow_values) flow in
            {
              mode = flow_query.mode;
              at =
                (fun ~prorater date ->
                  match Timeline.find_index tl date with
                  | None -> outside date
                  | Some i ->
                      let prev_bal = if i = 0 then init else (Lazy.force self_values).(i - 1) in
                      let p = Timeline.get tl i in
                      let flow_to_date =
                        flow_query.accrue ~prorater ~start_date:(Period.start_date p) ~end_date:date
                      in
                      prev_bal +. flow_to_date);
            }
        | Delay _ -> assert false
        | Of_array _ | Init _ | Growth_simple _ | Growth_compound _ | Year_frac _ | Events _
        | Source_periods _ | Scan_flow _ | Accumulate _ | Pointwise_of_flow _
        | Pointwise_of_balance _ | Flow_of_pointwise _ | Change_balance _ | Var _ | Fixpoint _ ->
            approx_balance_query tl values
      in
      Hashtbl.remove visiting s.id;
      result
    end
  in
  let query = build s in
  {
    query with
    at =
      (fun ~prorater date ->
        match Timeline.find_index tl date with
        | None -> outside date
        | Some _ -> query.at ~prorater date);
  }

type ('k, 'c) ast_node = ('k, 'c) node

(* Dependency graph *)

module Deps = struct
  type node = { id : int; name : string option; kind : string }
  type edge = { src : int; dst : int }
  type packed = Pack : ('k, 'c) t -> packed

  let kind_of_node : type k c. (k, c) ast_node -> string = function
    | Const _ -> "const"
    | Of_array _ -> "array"
    | Init _ -> "init"
    | Growth_simple _ -> "growth_simple"
    | Growth_compound _ -> "growth_compound"
    | Year_frac _ -> "year_frac"
    | Events _ -> "events"
    | Source_periods _ -> "source_periods"
    | Observations _ -> "observations"
    | Add _ -> "add"
    | Sub _ -> "sub"
    | Scale _ -> "scale"
    | Neg _ -> "neg"
    | Map _ -> "map"
    | Map2 _ -> "map2"
    | Sum _ -> "sum"
    | Where _ -> "where"
    | Prev _ -> "prev"
    | Scan_flow _ -> "scan"
    | Accumulate _ -> "accumulate"
    | Roll_forward _ -> "roll_forward"
    | Pointwise_of_flow _ -> "pointwise_of_flow"
    | Pointwise_of_balance _ -> "pointwise_of_balance"
    | Flow_of_pointwise _ -> "flow_of_pointwise"
    | Change_balance _ -> "change_balance"
    | Delay _ -> "delay"
    | Var _ -> "var"
    | Fixpoint _ -> "fixpoint"

  let children_of_node : type k c. (k, c) ast_node -> packed list = function
    | Const _ | Of_array _ | Init _ | Growth_simple _ | Growth_compound _ | Year_frac _ | Events _
    | Source_periods _ | Observations _ | Var _ ->
        []
    | Add (a, b) | Sub (a, b) -> [ Pack a; Pack b ]
    | Scale (_, src) | Neg src | Map (_, src) -> [ Pack src ]
    | Map2 (_, a, b) -> [ Pack a; Pack b ]
    | Sum ss -> List.map (fun s -> Pack s) ss
    | Where { cond = Any_series cond; then_; else_ } -> [ Pack cond; Pack then_; Pack else_ ]
    | Prev { src; _ } -> [ Pack src ]
    | Scan_flow { flow; _ } -> [ Pack flow ]
    | Accumulate { flow; _ } -> [ Pack flow ]
    | Roll_forward { flow; _ } -> [ Pack flow ]
    | Pointwise_of_flow src -> [ Pack src ]
    | Pointwise_of_balance src -> [ Pack src ]
    | Flow_of_pointwise src -> [ Pack src ]
    | Change_balance { balance; _ } -> [ Pack balance ]
    | Delay _ -> []
    | Fixpoint { body; _ } -> [ Pack body ]

  let full_graph roots =
    let visited = Hashtbl.create 64 in
    let nodes = ref [] in
    let edges = ref [] in
    let rec visit (Pack s) =
      if not (Hashtbl.mem visited s.id) then begin
        Hashtbl.replace visited s.id ();
        let node = resolve s in
        nodes := { id = s.id; name = s.name; kind = kind_of_node node } :: !nodes;
        let deps = children_of_node node in
        List.iter
          (fun (Pack dep) ->
            edges := { src = dep.id; dst = s.id } :: !edges;
            visit (Pack dep))
          deps
      end
    in
    List.iter (fun s -> visit (Pack s)) roots;
    (List.rev !nodes, List.rev !edges)

  let collect_all roots =
    let tbl = Hashtbl.create 64 in
    let rec visit (Pack s) =
      if not (Hashtbl.mem tbl s.id) then begin
        Hashtbl.replace tbl s.id (Pack s);
        List.iter visit (children_of_node (resolve s))
      end
    in
    List.iter (fun s -> visit (Pack s)) roots;
    tbl

  let collapsed_graph roots =
    let all_nodes, _ = full_graph roots in
    let named_nodes = List.filter (fun n -> n.name <> None) all_nodes in
    let named_ids = Hashtbl.create 16 in
    List.iter (fun n -> Hashtbl.replace named_ids n.id ()) named_nodes;
    let all_inner = collect_all roots in
    let edges = ref [] in
    let seen_edges = Hashtbl.create 64 in
    List.iter
      (fun src_node ->
        let visited = Hashtbl.create 16 in
        let queue = Queue.create () in
        (match Hashtbl.find_opt all_inner src_node.id with
        | Some (Pack s) -> List.iter (fun x -> Queue.push x queue) (children_of_node (resolve s))
        | None -> ());
        while not (Queue.is_empty queue) do
          let (Pack s) = Queue.pop queue in
          if not (Hashtbl.mem visited s.id) then begin
            Hashtbl.replace visited s.id ();
            if Hashtbl.mem named_ids s.id then begin
              let edge_key = (src_node.id, s.id) in
              if not (Hashtbl.mem seen_edges edge_key) then begin
                Hashtbl.replace seen_edges edge_key ();
                edges := { src = s.id; dst = src_node.id } :: !edges
              end
            end
            else List.iter (fun x -> Queue.push x queue) (children_of_node (resolve s))
          end
        done)
      named_nodes;
    (named_nodes, List.rev !edges)

  let graph ?(named_only = false) roots =
    if named_only then collapsed_graph roots else full_graph roots

  let node_attrs = function
    | "const" | "init" | "var" | "array" -> "shape=box style=filled fillcolor=\"#E8F4FD\""
    | "prev" | "scan" | "accumulate" | "roll_forward" | "fixpoint" ->
        "shape=box style=\"filled,dashed\" fillcolor=\"#FFF3E0\""
    | "where" -> "shape=diamond style=filled fillcolor=\"#F5F5F5\""
    | _ -> "shape=ellipse style=filled fillcolor=\"#F5F5F5\""

  let pp_dot ?(named_only = false) ppf roots =
    let nodes, edges = graph ~named_only roots in
    Format.fprintf ppf "digraph {@\n";
    Format.fprintf ppf "  rankdir=TB@\n";
    if named_only then
      Format.fprintf ppf
        "  node [fontname=\"Helvetica\" fontsize=11 shape=box style=\"filled,rounded\" \
         fillcolor=\"#E8F4FD\"]@\n"
    else Format.fprintf ppf "  node [fontname=\"Helvetica\" fontsize=11]@\n";
    Format.fprintf ppf "  edge [color=\"#666666\"]@\n";
    List.iter
      (fun { id; name; kind } ->
        let label = match name with Some n -> n | None -> kind in
        if named_only then Format.fprintf ppf "  %d [label=%S]@\n" id label
        else Format.fprintf ppf "  %d [label=%S %s]@\n" id label (node_attrs kind))
      nodes;
    List.iter (fun { src; dst } -> Format.fprintf ppf "  %d -> %d@\n" src dst) edges;
    Format.fprintf ppf "}@\n"
end

module Syntax = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( *$ ) k s = scale k s
end
