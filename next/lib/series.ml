(*---------------------------------------------------------------------------
   Copyright (c) 2025 Orcaset. All rights reserved.
   SPDX-License-Identifier: SSPL-1.0
  ---------------------------------------------------------------------------*)

(* Types *)

(* Each node gets a unique id used as the (id, period_index) memoization key
   during evaluation. This avoids structural equality on the entire DAG and
   lets nodes that share subexpressions deduplicate work. *)
let next_id =
  let counter = Atomic.make 0 in
  fun () -> Atomic.fetch_and_add counter 1

type node =
  | Const of float
  | Init of (Timeline.t -> int -> Period.t -> float)
  | Map of (float -> float) * inner
  | Map2 of (float -> float -> float) * inner * inner
  | Sum of inner list
  | Prev of { src : inner; default : float }
  | Scan of { init : float; f : acc:float -> x:float -> float; flow : inner }
  | Where of { cond : inner; then_ : inner; else_ : inner }
  | Delay of { mutable resolved : node option; thunk : unit -> inner }
  | Var of float ref
  | Fixpoint of { var : inner; body : inner; tol : float; max_iter : int; guess : float }

and inner = { id : int; name : string option; node : node }

type +'c t = inner

let mk ?name node = { id = next_id (); name; node }

(* Constructors *)

let const ?name v = mk ?name (Const v)

let of_array ?name arr =
  mk ?name (Init (fun _tl i _period -> if i < Array.length arr then arr.(i) else 0.0))

let init_flow ?name f = mk ?name (Init (fun _tl _i p -> f p))
let init ?name f = mk ?name (Init (fun _tl i p -> f i p))
let init_tl ?name f = mk ?name (Init f)
let delay ?name thunk = mk ?name (Delay { resolved = None; thunk })

let growth_simple ?name ?(daycount = Daycount.actual_360) ~start_date ~rate initial =
  init ?name (fun _i p -> initial *. (1.0 +. (rate *. daycount start_date (Period.start_date p))))

let growth_compound ?name ?(daycount = Daycount.actual_360) ~start_date ~rate initial =
  init ?name (fun _i p -> initial *. ((1.0 +. rate) ** daycount start_date (Period.start_date p)))

let year_frac ?name daycount =
  init ?name (fun _i p -> daycount (Period.start_date p) (Period.end_date p))

(* Combinators *)

(* Constant folding at construction time: when inputs are unnamed Const
   nodes, we compute the result immediately rather than building a node.
   Named Const nodes are never folded away — the user named them because
   they matter and they must remain visible in the dependency graph. *)

let map ?name f s =
  match s.node with
  | Const v when s.name = None -> mk ?name (Const (f v))
  | _ -> mk ?name (Map (f, s))

let map2 ?name f a b =
  match (a.node, b.node) with
  | Const va, Const vb when a.name = None && b.name = None -> mk ?name (Const (f va vb))
  | _ -> mk ?name (Map2 (f, a, b))

let add a b =
  match (a.node, b.node) with
  | Const 0.0, _ when a.name = None -> b
  | _, Const 0.0 when b.name = None -> a
  | Const va, Const vb when a.name = None && b.name = None -> mk (Const (va +. vb))
  | _ -> mk (Map2 (( +. ), a, b))

let sub a b =
  match (a.node, b.node) with
  | _, Const 0.0 when b.name = None -> a
  | Const va, Const vb when a.name = None && b.name = None -> mk (Const (va -. vb))
  | _ -> mk (Map2 (( -. ), a, b))

let mul a b =
  match (a.node, b.node) with
  | Const 0.0, _ when a.name = None -> a
  | _, Const 0.0 when b.name = None -> b
  | Const 1.0, _ when a.name = None -> b
  | _, Const 1.0 when b.name = None -> a
  | Const va, Const vb when a.name = None && b.name = None -> mk (Const (va *. vb))
  | _ -> mk (Map2 (( *. ), a, b))

let scale k s =
  match s.node with
  | _ when k = 1.0 -> s
  | _ when k = 0.0 -> mk (Const 0.0)
  | Const v when s.name = None -> mk (Const (k *. v))
  | _ -> mk (Map (( *. ) k, s))

let neg s =
  match s.node with Const v when s.name = None -> mk (Const ~-.v) | _ -> mk (Map (( ~-. ), s))

let div a b =
  match (a.node, b.node) with
  | Const va, Const vb when a.name = None && b.name = None -> mk (Const (va /. vb))
  | _ -> mk (Map2 (( /. ), a, b))

let abs s =
  match s.node with
  | Const v when s.name = None -> mk (Const (Float.abs v))
  | _ -> mk (Map (Float.abs, s))

let min a b =
  match (a.node, b.node) with
  | Const va, Const vb when a.name = None && b.name = None -> mk (Const (Float.min va vb))
  | _ -> mk (Map2 (Float.min, a, b))

let max a b =
  match (a.node, b.node) with
  | Const va, Const vb when a.name = None && b.name = None -> mk (Const (Float.max va vb))
  | _ -> mk (Map2 (Float.max, a, b))

let clamp ~lo ~hi s = map (fun x -> Float.min hi (Float.max lo x)) s

let round digits s =
  let factor = 10.0 ** float_of_int digits in
  map (fun x -> Float.round (x *. factor) /. factor) s

(* Partitions inputs into unnamed constants (folded at construction time) and
   everything else.  Named Const nodes are kept as separate Sum children so
   they remain visible in the dependency graph. *)
let sum ?name ss =
  let const_acc = ref 0.0 in
  let non_const =
    List.filter
      (fun s ->
        match s.node with
        | Const v when s.name = None ->
            const_acc := !const_acc +. v;
            false
        | _ -> true)
      ss
  in
  let c = !const_acc in
  match non_const with
  | [] -> mk ?name (Const c)
  | non_const when c = 0.0 -> mk ?name (Sum non_const)
  | non_const -> mk ?name (Sum (mk (Const c) :: non_const))

let named name s = { s with name = Some name }

let where ~cond ~then_ ~else_ =
  match cond.node with
  | Const v when v <> 0.0 && cond.name = None -> then_
  | Const _ when cond.name = None -> else_
  | _ -> mk (Where { cond; then_; else_ })

(* Cross-Period Operators *)

let prev ?name src ~default = mk ?name (Prev { src; default })
let scan ?name ~init f flow = mk ?name (Scan { init; f; flow })
let cumsum ?name ~init flow = scan ?name ~init (fun ~acc ~x -> acc +. x) flow

(* Knot-tying via mutable ref + Delay: we create a Delay node whose thunk
   closes over self_ref, call f with a placeholder, then patch self_ref to
   point at the real definition. The Delay is only forced at eval time,
   when the ref already holds the final series. *)
let feedback ?name ~default f =
  let self_ref = ref (const 0.0) in
  let prev_self = delay ?name (fun () -> prev !self_ref ~default) in
  let definition, exposed = f prev_self in
  self_ref := definition;
  exposed

(* Var holds a mutable ref that the evaluator updates on each iteration.
   The body DAG reads from Var, so mutating the ref + invalidating cached
   cells lets us re-evaluate the body at a new guess without rebuilding
   the graph. See the Fixpoint case in [compute] for the iteration loop. *)
let fixpoint ?name ?(tol = 1e-10) ?(max_iter = 100) ~guess f =
  let r = ref 0.0 in
  let var = mk (Var r) in
  let body = f var in
  mk ?name (Fixpoint { var; body; tol; max_iter; guess })

(* Pre-bins events into period buckets on first access using find_index,
   then serves O(1) lookups per period. Total cost is O(events * log periods)
   for the initial binning pass, vs O(periods * events) with the naive scan. *)
let of_events ?name events =
  let cache = ref None in
  mk ?name
    (Init
       (fun tl i _period ->
         let bins =
           match !cache with
           | Some (cached_tl, bins) when cached_tl == tl -> bins
           | _ ->
               let n = Timeline.length tl in
               let bins = Array.make n 0.0 in
               List.iter
                 (fun (d, v) ->
                   match Timeline.find_index tl d with
                   | Some idx -> bins.(idx) <- bins.(idx) +. v
                   | None -> ())
                 events;
               cache := Some (tl, bins);
               bins
         in
         bins.(i)))

let convert ~rate s = scale rate s

(* Evaluation Engine *)

(* Per-cell memoization keyed on (series_id, period_index).  Each cell
   transitions through three states:
     absent      -> not yet visited
     In_progress -> currently being computed (cycle detection sentinel)
     Done v      -> final value cached

   Same-period cycles (A -> B -> A at the same index) are detected when
   eval_cell encounters In_progress and raise Cycle_error.  Cross-period
   cycles are structurally impossible: Prev shifts by one period and Scan
   only reads its own earlier periods, so backward references always
   decrease the period index. *)

exception Cycle_error of { series_name : string option; period_index : int }
exception Convergence_error of { series_name : string option; period_index : int; iterations : int }

type cell_state = In_progress | Done of float
type eval_ctx = { tl : Timeline.t; n : int; memo : (int, cell_state) Hashtbl.t }

(* Forces a Delay thunk at most once, caching the resolved node.  Delay
   thunks close over mutable refs (see feedback, fixpoint) that are only
   assigned after construction, so forcing is deferred until the first
   evaluation when the refs hold their final values. *)
let resolve_node s =
  match s.node with
  | Delay d -> (
      match d.resolved with
      | Some node -> node
      | None ->
          let resolved = d.thunk () in
          d.resolved <- Some resolved.node;
          resolved.node)
  | node -> node

(* Clears memoized results for a single period, walking downstream from s.
   Used by fixpoint iteration: after mutating the Var ref to a new guess,
   we must invalidate cached cells that transitively depend on it so that
   re-evaluation picks up the updated value.  Stops at cross-period
   boundaries (Prev, Scan, Fixpoint) since those read different periods. *)
let rec invalidate_period ctx (s : inner) i =
  match s.node with
  | Const _ | Var _ -> ()
  | _ ->
      let key = (s.id * ctx.n) + i in
      if Hashtbl.mem ctx.memo key then begin
        Hashtbl.remove ctx.memo key;
        match resolve_node s with
        | Const _ | Init _ | Var _ -> ()
        | Map (_, src) -> invalidate_period ctx src i
        | Map2 (_, a, b) ->
            invalidate_period ctx a i;
            invalidate_period ctx b i
        | Sum ss -> List.iter (fun src -> invalidate_period ctx src i) ss
        | Where { cond; then_; else_ } ->
            invalidate_period ctx cond i;
            invalidate_period ctx then_ i;
            invalidate_period ctx else_ i
        | Prev _ | Scan _ | Fixpoint _ | Delay _ -> ()
      end

and eval_cell ctx (s : inner) i =
  match s.node with
  | Const v -> v
  | Var r -> !r
  | _ -> (
      let key = (s.id * ctx.n) + i in
      match Hashtbl.find_opt ctx.memo key with
      | Some (Done v) -> v
      | Some In_progress -> raise (Cycle_error { series_name = s.name; period_index = i })
      | None ->
          Hashtbl.replace ctx.memo key In_progress;
          let v = compute ctx s i in
          Hashtbl.replace ctx.memo key (Done v);
          v)

and compute ctx s i =
  let node = resolve_node s in
  match node with
  | Const v -> v
  | Init f ->
      let period = Timeline.get ctx.tl i in
      f ctx.tl i period
  | Map (f, src) -> f (eval_cell ctx src i)
  | Map2 (f, a, b) -> f (eval_cell ctx a i) (eval_cell ctx b i)
  | Sum ss -> List.fold_left (fun acc s -> acc +. eval_cell ctx s i) 0.0 ss
  | Where { cond; then_; else_ } ->
      if eval_cell ctx cond i <> 0.0 then eval_cell ctx then_ i else eval_cell ctx else_ i
  | Prev { src; default } -> if i = 0 then default else eval_cell ctx src (i - 1)
  | Scan { init = init_val; f; flow } ->
      (* Self-reference: eval_cell ctx s (i-1) reads this Scan node's own
         output at the previous period.  Safe because i-1 < i guarantees
         the earlier cell is already Done (left-to-right evaluation). *)
      let acc = if i = 0 then init_val else eval_cell ctx s (i - 1) in
      let x = eval_cell ctx flow i in
      f ~acc ~x
  | Var r -> !r
  | Fixpoint { var; body; tol; max_iter; guess = default } ->
      let r = match var.node with Var r -> r | _ -> assert false in
      (* Warm start: use previous period's converged value as initial guess.
         Adjacent periods usually have similar solutions, reducing iterations. *)
      let initial = if i = 0 then default else eval_cell ctx s (i - 1) in
      let rec iterate x iter =
        if iter >= max_iter then
          raise
            (Convergence_error { series_name = s.name; period_index = i; iterations = max_iter })
        else begin
          r := x;
          invalidate_period ctx var i;
          invalidate_period ctx body i;
          let x' = eval_cell ctx body i in
          if Float.abs (x' -. x) <= tol then x' else iterate x' (iter + 1)
        end
      in
      iterate initial 0
  | Delay _ -> failwith "Series.compute: unexpected unresolved Delay"

let make_ctx tl =
  let n = Timeline.length tl in
  (* Pre-sized for ~8 series worth of cells; grows automatically. *)
  { tl; n; memo = Hashtbl.create (n * 8) }

let eval_with_ctx ctx s = Array.init ctx.n (fun i -> eval_cell ctx s i)

let eval tl s =
  let ctx = make_ctx tl in
  eval_with_ctx ctx s

(* Shared eval_ctx: if two series depend on the same inner node,
   it is computed only once per period. *)
let eval_many tl ss =
  let ctx = make_ctx tl in
  List.map (fun s -> eval_with_ctx ctx s) ss

(* Infix Operators *)

module Syntax = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( *$ ) = scale
end

(* Date Queries *)

module Query = struct
  type split_fn =
    start_date:Date.t -> end_date:Date.t -> split_date:Date.t -> value:float -> float * float

  let default_split_fn ~start_date ~end_date ~split_date ~value =
    let total_days = Date.diff end_date start_date |> float_of_int in
    if total_days = 0.0 then (0.0, 0.0)
    else
      let days_before = Date.diff split_date start_date |> float_of_int in
      let ratio = days_before /. total_days in
      let before = value *. ratio in
      (before, value -. before)

  let call_split_fn split_fn tl i ~split_date ~value =
    let start_date = Timeline.period_start tl i in
    let end_date = Timeline.period_end tl i in
    split_fn ~start_date ~end_date ~split_date ~value

  let interpolate ?(split_fn = default_split_fn) tl values date =
    match Timeline.find_index tl date with
    | None ->
        invalid_arg
          (Printf.sprintf "Series.Query.interpolate: date %s is outside the timeline"
             (Date.to_string date))
    | Some i ->
        let before, _ = call_split_fn split_fn tl i ~split_date:date ~value:values.(i) in
        before

  (* Four cases for how a period overlaps [start_date, end_date):
       fully inside  -> take the whole value
       partial left  -> starts before range; take the "after" portion
       partial right -> ends after range; take the "before" portion
       spans both    -> split twice: at start_date then at end_date *)
  let accrue ?(split_fn = default_split_fn) tl values ~start_date ~end_date =
    let n = Timeline.length tl in
    let lo = match Timeline.find_index tl start_date with Some i -> i | None -> 0 in
    let hi =
      match Timeline.find_index tl end_date with Some i -> Int.min (i + 1) (n - 1) | None -> n - 1
    in
    let total = ref 0.0 in
    for i = lo to hi do
      let p_start = Timeline.period_start tl i in
      let p_end = Timeline.period_end tl i in
      if Date.(p_end > start_date && p_start < end_date) then begin
        let starts_before = Date.(p_start < start_date) in
        let ends_after = Date.(p_end > end_date) in
        match (starts_before, ends_after) with
        | false, false -> total := !total +. values.(i)
        | true, false ->
            let _, after = call_split_fn split_fn tl i ~split_date:start_date ~value:values.(i) in
            total := !total +. after
        | false, true ->
            let before, _ = call_split_fn split_fn tl i ~split_date:end_date ~value:values.(i) in
            total := !total +. before
        | true, true ->
            let _, after_start =
              call_split_fn split_fn tl i ~split_date:start_date ~value:values.(i)
            in
            let before_end, _ =
              split_fn ~start_date ~end_date:p_end ~split_date:end_date ~value:after_start
            in
            total := !total +. before_end
      end
    done;
    !total

  let balance_at ?(split_fn = default_split_fn) tl ~balance ~flow date =
    match Timeline.find_index tl date with
    | None ->
        invalid_arg
          (Printf.sprintf "Series.Query.balance_at: date %s is outside the timeline"
             (Date.to_string date))
    | Some i ->
        let prev_balance = if i = 0 then balance.(0) -. flow.(0) else balance.(i - 1) in
        let flow_before, _ = call_split_fn split_fn tl i ~split_date:date ~value:flow.(i) in
        prev_balance +. flow_before
end

(* Dependency Graph *)

module Deps = struct
  type node = { id : int; name : string option; kind : string }
  type edge = { src : int; dst : int }

  let kind_of_node = function
    | Const _ -> "const"
    | Init _ -> "init"
    | Map _ -> "map"
    | Map2 _ -> "map2"
    | Sum _ -> "sum"
    | Prev _ -> "prev"
    | Scan _ -> "scan"
    | Where _ -> "where"
    | Fixpoint _ -> "fixpoint"
    | Var _ -> "var"
    | Delay _ -> "delay"

  let children_of_node = function
    | Const _ | Init _ | Var _ -> []
    | Map (_, src) -> [ src ]
    | Map2 (_, a, b) -> [ a; b ]
    | Sum ss -> ss
    | Prev { src; _ } -> [ src ]
    | Scan { flow; _ } -> [ flow ]
    | Where { cond; then_; else_ } -> [ cond; then_; else_ ]
    | Fixpoint { body; _ } -> [ body ]
    | Delay _ -> []

  (* Collect all inner nodes reachable from roots into an id -> inner table. *)
  let collect_inner (roots : inner list) =
    let tbl = Hashtbl.create 64 in
    let rec visit (s : inner) =
      if not (Hashtbl.mem tbl s.id) then begin
        Hashtbl.replace tbl s.id s;
        let n = resolve_node s in
        List.iter visit (children_of_node n)
      end
    in
    List.iter visit roots;
    tbl

  let full_graph (roots : inner list) =
    let visited = Hashtbl.create 64 in
    let nodes = ref [] in
    let edges = ref [] in
    let rec visit (s : inner) =
      if not (Hashtbl.mem visited s.id) then begin
        Hashtbl.replace visited s.id ();
        let n = resolve_node s in
        nodes := { id = s.id; name = s.name; kind = kind_of_node n } :: !nodes;
        let deps = children_of_node n in
        List.iter
          (fun (dep : inner) ->
            edges := { src = dep.id; dst = s.id } :: !edges;
            visit dep)
          deps
      end
    in
    List.iter visit roots;
    (List.rev !nodes, List.rev !edges)

  (* Collapse to named nodes only. For each named node, BFS through unnamed
     children to find directly reachable named dependencies. *)
  let collapsed_graph (roots : inner list) =
    let all_nodes, _ = full_graph roots in
    let named_nodes = List.filter (fun n -> n.name <> None) all_nodes in
    let named_ids =
      let tbl = Hashtbl.create 16 in
      List.iter (fun n -> Hashtbl.replace tbl n.id ()) named_nodes;
      tbl
    in
    let all_inner = collect_inner roots in
    let edges = ref [] in
    let seen_edges = Hashtbl.create 64 in
    List.iter
      (fun src_node ->
        let visited = Hashtbl.create 16 in
        let queue = Queue.create () in
        (match Hashtbl.find_opt all_inner src_node.id with
        | None -> ()
        | Some s ->
            let n = resolve_node s in
            List.iter (fun (dep : inner) -> Queue.push dep queue) (children_of_node n));
        while not (Queue.is_empty queue) do
          let (s : inner) = Queue.pop queue in
          if not (Hashtbl.mem visited s.id) then begin
            Hashtbl.replace visited s.id ();
            if Hashtbl.mem named_ids s.id then begin
              let edge_key = (src_node.id, s.id) in
              if not (Hashtbl.mem seen_edges edge_key) then begin
                Hashtbl.replace seen_edges edge_key ();
                edges := { src = s.id; dst = src_node.id } :: !edges
              end
            end
            else
              let n = resolve_node s in
              List.iter (fun (dep : inner) -> Queue.push dep queue) (children_of_node n)
          end
        done)
      named_nodes;
    (named_nodes, List.rev !edges)

  let graph ?(named_only = false) roots =
    if named_only then collapsed_graph roots else full_graph roots

  let node_attrs = function
    | "const" | "init" | "var" -> "shape=box style=filled fillcolor=\"#E8F4FD\""
    | "prev" | "scan" | "fixpoint" -> "shape=box style=\"filled,dashed\" fillcolor=\"#FFF3E0\""
    | "where" -> "shape=diamond style=filled fillcolor=\"#F5F5F5\""
    | _ -> "shape=ellipse style=filled fillcolor=\"#F5F5F5\""

  let pp_dot ?(named_only = false) ppf (roots : inner list) =
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
