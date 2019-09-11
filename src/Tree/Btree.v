Require Extraction.
Require Import Util.
Require Import List.
Require Import Coq.Lists.List.
Import ListNotations.
Require Import Coq.Program.Basics.
Require Import Coq.Strings.Ascii.
Require Import Coq.Strings.String.
Open Scope string_scope.

Module Tree.
  Definition trace {t : Type} (x : string) (y : t) : t := y.
  Definition show {t : Type} (v : t) : string := "".

  Definition NodeId : Set := nat.

  Inductive Key : Set :=
  | Meta : string -> Key
  | Data : string -> Key.

  Inductive Tree (t : Set) : Set :=
  | InternalNode : NodeId -> list (option Key * NodeId) -> Tree t
  | Leaf : NodeId -> list (Key * t) -> Tree t.

  Arguments InternalNode {t} _.
  Arguments Leaf {t} _.

  Inductive WriteStatus (t : Set) : Set :=
  | WriteNeedFreshId : nat -> WriteStatus t
  | WriteNeedNode : NodeId -> WriteStatus t
  | WriteSuccess : Tree t -> list (Tree t) -> WriteStatus t
  | IWriteSuccess : list (Tree t) -> list (Tree t) -> list NodeId -> WriteStatus t
  | WriteError : string -> WriteStatus t.

  Arguments WriteNeedFreshId {t} _.
  Arguments WriteNeedNode {t} _.
  Arguments WriteSuccess {t} _ _.
  Arguments IWriteSuccess {t} _ _.
  Arguments WriteError {t} _.

  Definition max_node_size := 3.

  Definition invalidKey := Meta "__DOESNOTEXIST__".

  Definition key_beq (k1 : Key) (k2 : Key) : bool :=
    match (k1, k2) with
      | (Meta s, Meta s') => if string_dec s s' then true else false
      | (Data s, Data s') => if string_dec s s' then true else false
      | _ => false
    end.

  Definition getNodeId {t : Set} (node : Tree t) : NodeId :=
    match node with
    | Leaf id _ => id
    | InternalNode id _ => id
    end.

  Definition getNodeFirstKey {t : Set} (node : Tree t) : option Key :=
    let firstKeyLeaf kvs := match kvs with
      | ((k,v)::kvs) => k
      | _ => Meta "" (* Impossible? *)
    end in
    let firstKeyNode kvs := match kvs with
      | ((k,v)::kvs) => k
      | _ => None (* Impossible? *)
    end in
    match node with
    | Leaf _ kvs => Some (firstKeyLeaf kvs)
    | InternalNode _ kvs => firstKeyNode kvs
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

  Definition leb_key (a : Key) (b : Key) : bool :=
    match a, b with
    | Meta s1, Meta s2 => if string_leb s1 s2 then true else false
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

  (*Fixpoint treeWithoutValues {t : Set} (tr : Tree t) : Tree unit :=
    match tr with
    | Leaf l => Leaf (map (fun kv => (fst kv, tt)) l)
    | InternalNode ts => InternalNode (map (fun kt => (fst kt, treeWithoutValues (snd kt))) ts)
    end.*)


  Definition empty {t : Set} : Tree t := Leaf 0 [].

  (*Fixpoint lastOpt {t : Set} (l : list t) : option t := match l with
  | [] => None
  | [h] => Some h
  | h::r => lastOpt r
  end.*)

  (*Fixpoint firstKey {t : Set} (tr : Tree t) : Key := match tr with
  | Leaf ((k, _)::_) => k
  | InternalNode (_::(k,v) =>
      (fix lastNode ns' := match ns' with
        | [] => invalidKey (* Impossible *)
        | [(k, tr')] => lastKey tr'
        | h::r => lastNode r
        end) ns
  | _ => invalidKey (* Impossible *)
  end.*)

  (*Fixpoint lastKey {t : Set} (tr : Tree t) : Key := match tr with
  | Leaf kvs =>
      let justs := map (fun kv => Some kv) kvs in
      match lastOpt kvs with
      | None => Meta "__DOESNOTEXIST__" (* Impossible *)
      | Some kv => fst kv
      end
  | InternalNode ns =>
      (fix lastNode ns' := match ns' with
        | [] => Meta "__DOESNOTEXIST__" (* Impossible *)
        | [(k, tr')] => lastKey tr'
        | h::r => lastNode r
        end) ns

  end.*)

  (*Definition addTailKeys {t : Set} (trs : list (Tree t)) : list (option Key * Tree t) :=
    match lastOpt trs with
      | None => [] (* Impossible *)
      | Some lastTree => (map (fun tr' => (Some (lastKey tr'), tr')) (removelast trs) ++ [(None, lastTree)])
    end.*)

  Fixpoint findChildId (ks : list (option Key * NodeId)) (k : Key) : (list (option Key * NodeId)) * (option Key * NodeId) * (list (option Key *NodeId)) :=
    match ks with
    | ((None, id)::[]) => ([], (None, id), [])
    | ((Some k', id)::ks') =>
        if leb_key k' k then
          match findChildId ks' k with
          | (pre, (maxKey, id'), post) => (app pre [(Some k', id)], (maxKey, id'), post)
          end
        else
          ([], (Some k', id), ks')
    | _ => ([], (None, 0), []) (* Impossible *)
    end.

  (* They key passed up is the minimum key in the subtree *)
  Fixpoint insertNode {t : Set} (tr : Tree t) (k : Key) (v : t) (nodes : list (Tree t)) (freshIds : list NodeId) : WriteStatus t :=
    match tr with
    | Leaf id entries =>
        let newList := (insertSorted leb_key_pair entries (k, v)) in
        if negb (Nat.leb (List.length newList) max_node_size) then
          match freshIds with
          | (id1::id2::freshIds') =>
            let splitPoint := Nat.div (List.length newList) 2 in
            let front := List.firstn splitPoint newList in
            let back := List.skipn splitPoint newList in
            let trees := [Leaf id1 front; Leaf id2 back] in
            IWriteSuccess trees [] freshIds'
          | _ => WriteNeedFreshId 3 (* We need two for the children, plus there'll be at least one new parent.*)
          end
        else
          match freshIds with
          | (id1::freshIds') => IWriteSuccess [Leaf id1 newList] [] freshIds'
          | [] => WriteNeedFreshId 1
        end
    | InternalNode id ks =>
        match findChildId ks k with
        | (pre, (maxKey, childId), post) =>
          match nodes with
          | [] => WriteNeedNode childId
          | (child::nodes') =>
              if Nat.eqb (getNodeId child) childId then
                match insertNode child k v nodes' freshIds with
                | IWriteSuccess rs ns freshIds' =>
                    match freshIds' with
                    | [] => WriteNeedFreshId 1
                    | (id1::freshIds'') =>
                        IWriteSuccess [(InternalNode id1 (app pre (app (map (fun r => (maxKey, getNodeId r)) rs) post)))]
                                      (rs ++ ns) freshIds''
                    end
                | status => status
                end
              else
                WriteError ("Wrong child next in list " ++ show id ++ "/" ++ show childId)
          end
        end
    end. (*
        let children :=
          ((fix insertList (ks : list (option Key * Tree t)) : list (option Key * Tree t) :=
            match ks with
            | [] => [] (* Impossibile *)
            | ((Some k', tr')::ns) =>
                if leb_key k k' then
                  match insertNode tr' k v with
                  | [] => [] (* Impossible *)
                  | [n] => (Some (lastKey n), n)::ns
                  | ns' => map (fun n => (Some (lastKey n), n)) ns' ++ ns
                  end
                else
                  (Some k', tr')::insertList ns
            | ((None, tr')::ns) =>
                match insertNode tr' k v with
                | [] => [] (* Impossible *)
                | [n] => (None, n)::ns
                | ns' => addTailKeys ns'
                end
            end
          ) ks) in
        if Nat.ltb max_node_size (List.length children) then
          let splitPoint := Nat.div (List.length children) 2 in
          let front := List.firstn splitPoint children in
          let back := List.skipn splitPoint children in
          [InternalNode front; InternalNode back]
        else
          [InternalNode children]

    end.*)

  Definition insert {t : Set} (tr : Tree t) (k : Key) (v : t) (nodes : list (Tree t)) (freshIds : list NodeId) : WriteStatus t :=
    match insertNode tr k v nodes freshIds with
    | IWriteSuccess [] nodes _ => WriteError "No roots provided"
    | WriteSuccess _ _ => WriteError "External WriteSuccess provided by insertNode"
    | IWriteSuccess [root] nodes [] => WriteSuccess root nodes
    | IWriteSuccess [root1; root2] nodes [] => WriteNeedFreshId 1
    | IWriteSuccess [root1; root2] nodes [id1] =>
            WriteSuccess (InternalNode id1 [(getNodeFirstKey root2, getNodeId root1); (None, getNodeId root2)])
                         (root1::root2::nodes)
    | IWriteSuccess (root::_) nodes _ => WriteError "Extra freshIds provided"
    | x => x
    end.

  Definition insertMeta {t : Set} (tr : Tree t) (s : string) (v : t) : list (Tree t) -> list NodeId -> WriteStatus t := insert tr (Meta s) v.
  Definition insertData {t : Set} (tr : Tree t) (s : string) (v : t) : list (Tree t) -> list NodeId -> WriteStatus t := insert tr (Data s) v.

  Fixpoint remove {t : Set} (tr : Tree t) (k : Key) : WriteStatus t :=
    WriteSuccess empty [].
    (*match tr with
    | Leaf vs => Leaf (filter (fun kv' => match kv' with pair k' _ => negb (key_beq k k') end) vs)
    | InternalNode trs => InternalNode ((fix removeList trs :=
        match trs with
        | [] => []
        | (k', tr')::trs' =>
            if leb_option_key k k' then
              let tr'' := remove tr' k in
              if option_key_beq k k' then
                (Some (lastKey tr''), tr'')::trs' (* Fix k'*)
              else
                (k', tr'')::trs' (* Fix k'*)
            else
              (k', tr')::removeList trs'
        end
      ) trs)
    end.*)

  Definition removeMeta {t : Set} (tr : Tree t) (s : string) : Tree.WriteStatus t := remove tr (Meta s).
  Definition removeData {t : Set} (tr : Tree t) (s : string) : Tree.WriteStatus t := remove tr (Data s).

  Definition update {t : Set} (tr : Tree t) (k : Key) (v : t) (freshIds : list NodeId) : WriteStatus t :=
    match remove tr k with
    | WriteSuccess root nodes => insert root k v [] []
    | _ => WriteSuccess empty []
    end.

  Definition updateMeta {t : Set} (tr : Tree t) (s : string) (v : t) : list NodeId -> WriteStatus t := update tr (Meta s) v.
  Definition updateData {t : Set} (tr : Tree t) (s : string) (v : t) : list NodeId -> WriteStatus t := update tr (Data s) v.

  Fixpoint lookup {t : Set} (tr : Tree t) (k : Key) : option t :=
    None.
    (*
    (*trace ("Looking up " ++ show (treeWithoutValues tr))*)
    match tr with
    | Leaf vs =>
        match (find (fun kv' => match kv' with (k', v) => key_beq k k' end) vs) with
          | None => None
          | Some (k, v) => Some v
        end
    | InternalNode ks => (fix lookupList ks := match trace ("Looking up in " ++ show (map (fun kv => fst kv) ks)) ks with
        | [] => None (* Impossible *)
        | (None, tr')::ks' => lookup tr' k
        | (Some k', tr')::ks' =>
            if leb_key k k' then
              lookup tr' k
            else
              lookupList ks'
        end
      ) ks
    end. *)

  Definition lookupMeta {t : Set} (tr : Tree t) (s : string) : option t := lookup tr (Meta s).
  Definition lookupData {t : Set} (tr : Tree t) (s : string) : option t := lookup tr (Data s).

  Fixpoint children {t : Set} (tr : Tree t) (s : string) : list Key :=
    [].
    (*let k := Meta s in
    let prefixSlash := add_final_slash (keyPath k) in
    let getSuffix := fun mpath => match mpath with
          | Meta path => match strip_prefix prefixSlash path with
              | None => None
              | Some s => Some (Meta s)
              end
          | Data path => match strip_prefix prefixSlash path with
              | None => None
              | Some s => Some (Data s)
              end
          end
    in
      match tr with
      | Leaf vs => filter (compose no_slashes keyPath) (get_justs (map (compose getSuffix fst) vs))
      | InternalNode trs =>  (* TODO: Only search appropriate nodes, not the whole tree *)
          List.concat (map (fun ktr => children (snd ktr) s) trs)
      (* This is better, but wrong - on [ [/a/b, /a/b] [/a/c]], wouldn't find /a/c because /a/b > /a.
       * We need to compare the prefix. Also should only recurse if the next node starts after /a. *)
      (* Regardless, reading the whole tree from disk swamps the runtime of this :( *)
      (*| InternalNode trs => (fix childrenList trs :=
          match trs with
          | [] => []
          | ((k', tr')::trs') =>
              let hereChildren :=
                children tr' k
              in
              let restChildren :=
                if leb_option_key k k' then
                  []
                else
                  childrenList trs'
              in
              hereChildren ++ childrenList trs'
          end) trs *)
      end.*)

End Tree.

Extraction Language Haskell.
Require Import Coq.extraction.ExtrHaskellBasic.
Require Import Coq.extraction.ExtrHaskellString.
Require Import Coq.extraction.ExtrHaskellNatInt.
Extract Inlined Constant fst => "Prelude.fst".
Extract Inlined Constant map => "Prelude.map".
Extract Inlined Constant Tree.trace => "trace".
Extract Inlined Constant Tree.show => "Prelude.show".
(*Extract Inlined Constant Tree.trace => "(\ x y -> y)".*)
(*Extract Inlined Constant Tree.show => "(\ _ -> """")".*)
Extraction "BtreeCoq.hs" Tree.
