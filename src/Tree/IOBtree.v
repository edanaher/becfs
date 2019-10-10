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

  Arguments InternalNode {t} _.
  Arguments Leaf {t} _.

  Inductive WriteStatus (t : Set) : Set :=
  | WriteNeedFreshId : nat -> WriteStatus t
  | WriteNeedNode : NodeId -> WriteStatus t
  | WriteSuccess : IOTree t -> list (IOTree t) -> WriteStatus t
  | IWriteSuccess : list (IOTree t) -> list (IOTree t) -> list NodeId -> WriteStatus t
  | WriteError : string -> WriteStatus t.

  Arguments WriteNeedFreshId {t} _.
  Arguments WriteNeedNode {t} _.
  Arguments WriteSuccess {t} _ _.
  Arguments IWriteSuccess {t} _ _.
  Arguments WriteError {t} _.

  Inductive ReadStatus (res : Set) : Set :=
  | ReadNeedNode : NodeId -> ReadStatus res
  | ReadRepeatOn : list NodeId -> ReadStatus res
  | ReadSuccess : res -> ReadStatus res
  | ReadDoesNotExist : ReadStatus res
  | ReadError : string -> ReadStatus res.

  Arguments ReadNeedNode {res} _.
  Arguments ReadRepeatOn {res} _.
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


End IOTree.

Extraction Language Haskell.
Require Import Coq.extraction.ExtrHaskellBasic.
Require Import Coq.extraction.ExtrHaskellString.
Require Import Coq.extraction.ExtrHaskellNatInt.
Extract Inlined Constant fst => "Prelude.fst".
Extract Inlined Constant map => "Prelude.map".
Extract Inlined Constant IOTree.trace => "trace".
Extract Inlined Constant IOTree.show => "Prelude.show".
(*Extract Inlined Constant IOTree.trace => "(\ x y -> y)".*)
(*Extract Inlined Constant IOTree.show => "(\ _ -> """")".*)
Extraction "IOBtreeCoq.hs" IOTree.
