Require Extraction.
Require Import Util.
Require Import List.
Require Import Coq.Lists.List.
Import ListNotations.
Require Import Coq.Program.Basics.
Require Import Coq.Strings.Ascii.
Require Import Coq.Strings.String.
Open Scope string_scope.

Module IOTree.
  Definition trace {t : Type} (x : string) (y : t) : t := y.
  Definition show {t : Type} (v : t) : string := "".

  Inductive RawIO (t : Set) : Set :=
  | RawReturn : t -> RawIO t.
  Arguments RawReturn {t} _.

  Definition IO (t : Set) : Set := RawIO t.
  Definition ret {t : Set} (x : t) : IO t := RawReturn x.

  Definition bind {t : Set} {r : Set} (x : IO t) (f : t -> IO r) : IO r :=
    match x with
    | RawReturn v => f v
    end.
  Notation "v >>= f" := (bind (v) (f)) (at level 90, left associativity).
  Notation "v >> f" := (bind (v) (fun _ => f)) (at level 90, left associativity).


  Definition NodeId : Set := nat.

  Inductive Key : Set :=
  | Meta : string -> Key
  | Data : string -> Key.

  Definition showKey (k  : Key) : string := match k with
  | Meta s => "Meta " ++ s
  | Data s => "Data " ++ s
  end.

  Inductive IOTree (t : Set) : Set :=
  | InternalNode : NodeId -> list (Key * NodeId) -> IOTree t
  | Leaf : NodeId -> list (Key * t) -> IOTree t.

  Arguments InternalNode {t} _ _.
  Arguments Leaf {t} _.

  Inductive WriteStatus (t : Set) : Set :=
  | WriteNeedFreshId : nat -> WriteStatus t
  | WriteSuccess : IOTree t -> list (IOTree t) -> WriteStatus t
  | IWriteSuccess : list (IOTree t) -> list (IOTree t) -> nat -> WriteStatus t
  | WriteError : string -> WriteStatus t.

  Arguments WriteNeedFreshId {t} _.
  Arguments WriteSuccess {t} _ _.
  Arguments IWriteSuccess {t} _ _ _.
  Arguments WriteError {t} _.

  Inductive ReadStatus (res : Set) : Set :=
  | ReadSuccess : res -> ReadStatus res
  | ReadDoesNotExist : ReadStatus res
  | ReadError : string -> ReadStatus res.

  Arguments ReadSuccess {res} _.
  Arguments ReadDoesNotExist {res}.
  Arguments ReadError {res} _.

  Definition max_node_size := 3.

  Definition invalidKey str := Meta ("__DOESNOTEXIST__" ++ str).

  Definition key_beq (k1 : Key) (k2 : Key) : bool :=
    match (k1, k2) with
      | (Meta s, Meta s') => if string_dec s s' then true else false
      | (Data s, Data s') => if string_dec s s' then true else false
      | _ => false
    end.

  Definition getNodeId {t : Set} (node : IOTree t) : NodeId :=
    match node with
    | Leaf id _ => id
    | InternalNode id _ => id
    end.

  Definition getNodeFirstKey {t : Set} (node : IOTree t) : Key :=
    let firstKey {v : Set} (kvs : list (Key * v)) : Key := match kvs with
      | ((k,v)::kvs) => k
      | _ => invalidKey "firstkey" (* Impossible? *)
    end in
    match node with
    | Leaf _ kvs => (firstKey kvs)
    | InternalNode _ kvs => firstKey kvs
    end.

  Definition option_key_beq (k1 : Key) (k2 : option Key) : bool :=
    match k2 with
    | None => false
    | Some k2' => key_beq k1 k2'
    end.

  Definition keyPath (k : Key) : string :=
    match k with
    | Meta s => s
    | Data s => s
  end.

  Definition onKeyPath (op : string -> string) (k : Key) : Key :=
    match k with
    | Meta s => Meta (op s)
    | Data s => Data (op s)
    end.

  Fixpoint countSlashes (path : string) : nat :=
    match path with
    | EmptyString => 0
    | String c path' => (if Ascii.eqb c "/" then 1 else 0) + countSlashes path'
    end.

  Definition leb_key (a : Key) (b : Key) : bool :=
    match a, b with
    | Meta s1, Meta s2 => match Nat.compare (countSlashes s1) (countSlashes s2) with
                          | Lt => true
                          | Gt => false
                          | Eq => string_leb s1 s2
                          end
    | Data s1, Data s2 => if string_leb s1 s2 then true else false
    | Meta _, Data _ => true
    | Data _, Meta _ => false
    end.

  Definition leb_option_key (a : Key) (b : option Key) : bool :=
    match b with
    | None => true
    | Some b' => leb_key a b'
    end.


  Definition leb_key_pair {t : Set} (a : Key * t) (b : Key * t) : bool :=
    leb_key (fst a) (fst b).

  Fixpoint insertSorted {t : Type} (lt : t -> t -> bool) (l : list t) (v : t) : list t :=
    match l with
    | [] => [v]
    | (h::r) =>
        if lt v h then
          v::h::r
        else
          h::insertSorted lt r v
    end.

  Fixpoint lookup {t : Set} (gas : nat) (fetchNode : NodeId -> IO (IOTree t)) (tr : IOTree t) (k : Key) : IO (ReadStatus t) :=
    match gas with
    | O => ret (ReadError "Out of gas")
    | S gas' =>
      match tr with
      | Leaf _ vs =>
          match (find (fun kv' => match kv' with (k', v) => key_beq k k' end) vs) with
            | None => ret ReadDoesNotExist
            | Some (k, v) => ret (ReadSuccess v)
          end
      | InternalNode _ ks =>
          (fix lookupList ks := match ks with
            | [] => ret (ReadError "Empty child list") (* Impossible *)
            | (k', tr')::ks' => match ks' with
                | [] => fetchNode tr' >>= fun child => lookup gas' fetchNode child k
                | ((k'', tr'')::ks'') =>
                    if negb (leb_key k'' k) then
                      fetchNode tr' >>= fun child => lookup gas' fetchNode child k
                    else
                      lookupList ks'
                end
            end
          ) ks
      end
    end.

  Fixpoint findChildId (children : list (Key * NodeId)) (k : Key) : (list (Key * NodeId)) * (Key * NodeId) * (list (Key * NodeId)) :=
    match children with
    | (candidate::children') =>
        match children' with
        | [] => ([], candidate, [])
        | ((k', id)::children'') =>
            if leb_key k' k then
              match findChildId children' k with
              | (pre, (minKey, id'), post) => (candidate::pre, (minKey, id'), post)
              end
            else
              ([], (candidate), ((k', id)::children''))
        end
    | _ => ([], (invalidKey "childId", 0), []) (* Impossible *)
    end.

  Definition splitList {t : Set} (nodes : list (Key * t)) : (list (Key * t) * list (Key * t)):=
    let splitPoint := Nat.div (List.length nodes) 2 in
    let front := List.firstn splitPoint nodes in
    let back := List.skipn splitPoint nodes in
    (front, back).

  Fixpoint insertNode {t : Set} (gas : nat) (fetchNode : NodeId -> IO (IOTree t)) (tr : IOTree t) (k : Key) (v : t) (nextId : NodeId) : IO (WriteStatus t) :=
    match gas with
    | O => ret (WriteError "Out of gas")
    | S gas' =>
      match tr with
      | Leaf id entries =>
          let newList := (insertSorted leb_key_pair entries (k, v)) in
          if negb (Nat.leb (List.length newList) max_node_size) then
            let (front, back) := splitList newList in
            let trees := [Leaf nextId front; Leaf (nextId + 1) back] in
            ret (IWriteSuccess trees [] (nextId + 2))
          else
             ret (IWriteSuccess [Leaf nextId newList] [] (nextId + 1))
      | InternalNode id children =>
        match findChildId children k with
        | (pre, (minKey, childId), post) =>
          fetchNode childId >>= fun child =>
          insertNode gas' fetchNode child k v nextId >>= fun res => match res with
          | IWriteSuccess rs ns nextId' =>
              let newList := app pre (app (map (fun r => (getNodeFirstKey r, getNodeId r)) rs) post) in
              if negb (Nat.leb (List.length newList) max_node_size) then
                let (front, back) := splitList newList in
                let trees := [InternalNode nextId' front; InternalNode (nextId' + 1) back] in
                ret (IWriteSuccess trees (rs ++ ns) (nextId' + 2))
              else
                ret (IWriteSuccess [InternalNode nextId' newList] (rs ++ ns) (nextId' + 1))
          | status => ret status
          end
        end
      end
    end.

  Definition insert {t : Set} (fetchNode : NodeId -> IO (IOTree t)) (tr : IOTree t) (k : Key) (v : t) (nextId : NodeId) : IO (WriteStatus t) :=
    insertNode 99 (* Height of tree *) fetchNode tr k v nextId >>= fun res =>
    match res with
    | IWriteSuccess [] nodes nextId' => ret (WriteError "No root provided by insertNode")
    | IWriteSuccess [tr'] nodes nextId' => ret (WriteSuccess tr' nodes)
    | IWriteSuccess [root1; root2] nodes nextId' =>
            ret (WriteSuccess (InternalNode nextId
                              [(getNodeFirstKey root1, getNodeId root1); (getNodeFirstKey root2, getNodeId root2)])
                              (root1::root2::nodes))
    | r => ret r
    end.

  Fixpoint removeNode {t : Set} (fetchNode : NodeId -> IO (IOTree t)) (tr : IOTree t) (k : Key) (nextId : NodeId) : IO (WriteStatus t) :=
    ret (WriteError "Unimplemented").

  Definition remove {t : Set} (fetchNode : NodeId -> IO (IOTree t)) (tr : IOTree t) (k : Key) (nextId : NodeId) : IO (WriteStatus t) :=
    match removeNode fetchNode tr k nextId with
    | status => status
    end.


End IOTree.

Extraction Language Haskell.
Require Import Coq.extraction.ExtrHaskellBasic.
Require Import Coq.extraction.ExtrHaskellString.
Require Import Coq.extraction.ExtrHaskellNatInt.
Extract Inlined Constant fst => "Prelude.fst".
Extract Inlined Constant map => "Prelude.map".
Extract Inlined Constant IOTree.trace => "trace".
Extract Inlined Constant IOTree.show => "Prelude.show".
Extract Inlined Constant IOTree.NodeId => "Prelude.Int".
(*Extract Inductive IOTree.RawIO => "IOSHOULDNTBEUSED" [ "RETURNSHOULDNTBEUSED" ].*)
Extract Inlined Constant IOTree.ret => "Prelude.return".
Extract Constant IOTree.IO "t" => "Prelude.IO t".
Extract Inlined Constant IOTree.bind => "( Prelude.>>= )".
(*Extract Inlined Constant IOTree.trace => "(\ x y -> y)".*)
(*Extract Inlined Constant IOTree.show => "(\ _ -> """")".*)
Extraction "IOBtreeCoq.hs" IOTree.
