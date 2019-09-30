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

  Definition showKey (k  : Key) : string := match k with
  | Meta s => "Meta " ++ s
  | Data s => "Data " ++ s
  end.

  Inductive Tree (t : Set) : Set :=
  | InternalNode : NodeId -> list (Key * NodeId) -> Tree t
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

  Inductive ReadStatus (t : Set) (res : Set) : Set :=
  | ReadNeedNode : NodeId -> ReadStatus t res
  | ReadRepeatOn : list NodeId -> ReadStatus t res
  | ReadSuccess : res -> ReadStatus t res
  | ReadDoesNotExist : ReadStatus t res
  | ReadError : string -> ReadStatus t res.

  Arguments ReadNeedNode {t} {res} _.
  Arguments ReadRepeatOn {t} {res} _.
  Arguments ReadSuccess {t} {res} _.
  Arguments ReadDoesNotExist {t} {res}.
  Arguments ReadError {t} {res} _.

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

  Definition getNodeFirstKey {t : Set} (node : Tree t) : Key :=
    let firstKey {v : Set} (kvs : list (Key * v)) : Key := match kvs with
      | ((k,v)::kvs) => k
      | _ => invalidKey (* Impossible? *)
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
    | _ => ([], (invalidKey, 0), []) (* Impossible *)
    end.

  Definition splitList {t : Set} (nodes : list (Key * t)) : (list (Key * t) * list (Key * t)):=
    let splitPoint := Nat.div (List.length nodes) 2 in
    let front := List.firstn splitPoint nodes in
    let back := List.skipn splitPoint nodes in
    (front, back).


  Fixpoint insertNode {t : Set} (tr : Tree t) (k : Key) (v : t) (nodes : list (Tree t)) (freshIds : list NodeId) : WriteStatus t :=
    match tr with
    | Leaf id entries =>
        let newList := (insertSorted leb_key_pair entries (k, v)) in
        if negb (Nat.leb (List.length newList) max_node_size) then
          match freshIds with
          | (id1::id2::freshIds') =>
            let (front, back) := splitList newList in
            let trees := [Leaf id1 front; Leaf id2 back] in
            IWriteSuccess trees [] freshIds'
          | _ => WriteNeedFreshId 2
          end
        else
          match freshIds with
          | (id1::freshIds') => IWriteSuccess [Leaf id1 newList] [] freshIds'
          | [] => WriteNeedFreshId 1
        end
    | InternalNode id children =>
        match findChildId children k with
        | (pre, (minKey, childId), post) =>
          match nodes with
          | [] => WriteNeedNode childId
          | (child::nodes') =>
              if Nat.eqb (getNodeId child) childId then
                match insertNode child k v nodes' freshIds with
                | IWriteSuccess rs ns freshIds' =>
                    match freshIds' with
                    | [] => WriteNeedFreshId 1
                    | (id1::freshIds'') =>
                        let newList := app pre (app (map (fun r => (getNodeFirstKey r, getNodeId r)) rs) post) in
                        if negb (Nat.leb (List.length newList) max_node_size) then
                          match freshIds'' with
                          | (id1::id2::freshIds''') =>
                            let (front, back) := splitList newList in
                            let trees := [InternalNode id1 front; InternalNode id2 back] in
                            IWriteSuccess trees (rs ++ ns) freshIds'''
                          | _ => WriteNeedFreshId 2
                          end
                        else
                          IWriteSuccess [InternalNode id1 newList] (rs ++ ns) freshIds''
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
            WriteSuccess (InternalNode id1 [(getNodeFirstKey root1, getNodeId root1); (getNodeFirstKey root2, getNodeId root2)])
                         (root1::root2::nodes)
    | IWriteSuccess (root::_) nodes _ => WriteError "Extra freshIds provided"
    | x => x
    end.

  (* TODO: combine small nodes with neighbors *)
  Fixpoint removeNode {t : Set} (tr : Tree t) (k : Key) (nodes : list (Tree t)) (freshIds : list NodeId) : WriteStatus t :=
    match tr with
    | Leaf id vs =>
        let tr' := Leaf id (filter (fun kv' => match kv' with pair k' _ => negb (key_beq k k') end) vs) in
        IWriteSuccess [tr'] []  freshIds
    | InternalNode id children =>
        match findChildId children k with
        | (pre, (minKey, childId), post) =>
          match nodes with
          | [] => WriteNeedNode childId
          | (child::nodes') =>
              if Nat.eqb (getNodeId child) childId then
                match removeNode child k nodes' freshIds with
                | IWriteSuccess rs ns freshIds' =>
                    match freshIds' with
                    | [] => WriteNeedFreshId 1
                    | (id1::freshIds'') =>
                        let newList := (pre ++ map (fun r => (getNodeFirstKey r, getNodeId r)) rs ++ post)%list in
                        if negb (Nat.leb (List.length newList) max_node_size) then
                          match freshIds'' with
                          | (id1::id2::freshIds''') =>
                            let (front, back) := splitList newList in
                            let trees := [InternalNode id1 front; InternalNode id2 back] in
                            IWriteSuccess trees (rs ++ ns) freshIds'''
                          | _ => WriteNeedFreshId 2
                          end
                        else
                          IWriteSuccess [InternalNode id1 newList] (rs ++ ns) freshIds''
                    end
                | status => status
                end
              else
                WriteError ("Wrong child next in list " ++ show id ++ "/" ++ show childId)
          end
        end
    end.

  Definition remove {t : Set} (tr : Tree t) (k : Key) (nodes : list (Tree t)) (freshIds : list NodeId) : WriteStatus t :=
    match removeNode tr k nodes freshIds with
    | IWriteSuccess [] nodes _ => WriteError "No roots provided"
    | WriteSuccess _ _ => WriteError "External WriteSuccess provided by removeNode"
    | IWriteSuccess [root] nodes [] => WriteSuccess root nodes
    | IWriteSuccess [root1; root2] nodes [] => WriteNeedFreshId 1 (* Impossible? *)
    | IWriteSuccess [root1; root2] nodes [id1] => (* Impossible? *)
            WriteSuccess (InternalNode id1 [(getNodeFirstKey root1, getNodeId root1); (getNodeFirstKey root2, getNodeId root2)])
                         (root1::root2::nodes)
    | IWriteSuccess (root::_) nodes _ => WriteError "Extra freshIds provided"
    | x => x
    end.

  Definition update {t : Set} (tr : Tree t) (k : Key) (v : t) (nodes : list (Tree t)) (freshIds : list NodeId) : WriteStatus t :=
    WriteSuccess empty [].
    (*match remove tr k with
    | WriteSuccess root nodes => insert root k v [] []
    | _ => WriteSuccess empty []
    end.*)

  Definition updateMeta {t : Set} (tr : Tree t) (s : string) (v : t) : list (Tree t) -> list NodeId -> WriteStatus t := update tr (Meta s) v.
  Definition updateData {t : Set} (tr : Tree t) (s : string) (v : t) : list (Tree t) -> list NodeId -> WriteStatus t := update tr (Data s) v.

  Fixpoint lookup {t : Set} (tr : Tree t) (k : Key) (nodes : list (Tree t)) : ReadStatus t t :=
    match (*trace ("Looking up in node" ++ show (getNodeId tr))*) tr with
    | Leaf _ vs =>
        match (find (fun kv' => match kv' with (k', v) => key_beq k k' end) vs) with
          | None => ReadDoesNotExist
          | Some (k, v) => ReadSuccess v
        end
    | InternalNode _ ks =>
      let recurse childId :=
        match nodes with
        | [] => ReadNeedNode childId
        | (child::nodes') =>
            if Nat.eqb (getNodeId child) childId then
              lookup child k nodes'
            else
              ReadError ("Wrong child next in list " ++ show (getNodeId child) ++ "/" ++ show childId)
        end
      in
      (fix lookupList ks := match (*trace ("Looking up in " ++ show (map (fun kv => fst kv) ks))*) ks with
        | [] => ReadError "Empty child list" (* Impossible *)
        | (k', tr')::ks' => match ks' with
            | [] => recurse tr'
            | ((k'', tr'')::ks'') =>
                if negb (leb_key k'' k) then
                  recurse tr'
                else
                  lookupList ks'
            end
        end
      ) ks
    end.

  (* Compare for children of a slash-suffixed directory, in metadata order.  So
   * - All children must have the same number of slashes as the prefix.
   *   Otherwise, that determines whether a node is before or after the children.
   * - If the number of slashes match, then the directory must be a prefix of the child.
   *)
  Fixpoint childCmpString (prefix : string) (value : string) : comparison :=
    let fix aux prefix value := match (prefix, value) with
        | (EmptyString, EmptyString) => Eq
        | (EmptyString, _) => Eq
        | (_, EmptyString) => Lt
        | (String pc p', String vc v') => match Nat.compare (Ascii.nat_of_ascii vc) (Ascii.nat_of_ascii pc) with
            | Eq => aux p' v'
            | result => result
            end
        end in
    match Nat.compare (countSlashes value) (countSlashes prefix) with
    | Gt => Gt
    | Lt => Lt
    | Eq => aux prefix value
    end.

  (* Data is always too big; for Meta, compare the string. *)
  Definition childCmp (prefix : string) (k : Key) : comparison :=
    match k with
    | Meta s => childCmpString prefix s
    | Data _ => Gt
    end.

  Definition consSuccess {t : Set} {res : Set} (e : res) (status : ReadStatus t (list res)) : ReadStatus t (list res) := match status with
  | ReadSuccess l => ReadSuccess (e::l)
  | _ => status
  end.

  Fixpoint childEntries {t : Set} (tr : Tree t) (s : string) : (ReadStatus t (list (Key * t))) :=
    match tr with
    | Leaf _ es =>
        let fix findMatches es := match es with
            | [] => ReadSuccess []
            | ((k, v)::es') => match childCmp s k with
                | Lt => findMatches es'
                | Eq => consSuccess (k, v) (findMatches es')
                | Gt => ReadSuccess []
              end
            end in
        findMatches es
    | InternalNode _ es =>
        let fix getChildCandidates es :=
            match es with
            | [] => []
            | ((k, childId)::es') => match childCmp s k with
              | Gt => [] (* We've gone too far; give up *)
              | _ => match es' with (* Check the next firstKey to see if we're big enough. *)
                | [] => [childId] (* Last one; descend *)
                | ((k', _)::_) => match childCmp s k' with
                  | Lt => getChildCandidates es'  (* We're too small; skip us *)
                  | _ => (childId::getChildCandidates es') (* We're a candidate; descend *)
                  end
                end
              end
            end in
        ReadRepeatOn (getChildCandidates es)
    end.
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
