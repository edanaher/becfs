Require Extraction.
Require Import Util.
Require Import List.
Require Import Coq.Lists.List.
Import ListNotations.
Require Import Coq.Program.Basics.
Require Import Coq.Strings.Ascii.
Require Import Coq.Strings.String.

Module Tree.
  Definition trace {t : Type} (x : string) (y : t) : t := y.
  Definition show {t : Type} (v : t) : string := "".

  Inductive Key : Set :=
  | Meta : string -> Key
  | Data : string -> Key.

  Definition max_node_size := 3.

  Definition key_beq (k1 : Key) (k2 : Key) : bool :=
    match (k1, k2) with
      | (Meta s, Meta s') => if string_dec s s' then true else false
      | (Data s, Data s') => if string_dec s s' then true else false
      | _ => false
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

  Inductive Tree (t : Set) : Set :=
  | InternalNode : list (option Key * (Tree t)) -> Tree t
  | Leaf : list (Key * t) -> Tree t.

  Arguments InternalNode {t} _.
  Arguments Leaf {t} _.

  Fixpoint treeWithoutValues {t : Set} (tr : Tree t) : Tree unit :=
    match tr with
    | Leaf l => Leaf (map (fun kv => (fst kv, tt)) l)
    | InternalNode ts => InternalNode (map (fun kt => (fst kt, treeWithoutValues (snd kt))) ts)
    end.


  Definition empty {t : Set} : Tree t := Leaf [].

  Fixpoint lastOpt {t : Set} (l : list t) : option t := match l with
  | [] => None
  | [h] => Some h
  | h::r => lastOpt r
  end.

  Fixpoint lastKey {t : Set} (tr : Tree t) : Key := match tr with
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

  end.

  Definition addTailKeys {t : Set} (trs : list (Tree t)) : list (option Key * Tree t) :=
    match lastOpt trs with
      | None => [] (* Impossible *)
      | Some lastTree => (map (fun tr' => (Some (lastKey tr'), tr')) (removelast trs) ++ [(None, lastTree)])
    end.

  Fixpoint insertNode {t : Set} (tr : Tree t) (k : Key) (v : t) : list (Tree t) :=
    match tr with
    | Leaf vs =>
        let newList := (insertSorted leb_key_pair vs (k, v)) in
        if Nat.eqb (List.length vs) max_node_size then
          let splitPoint := Nat.div (List.length newList) 2 in
          let front := List.firstn splitPoint newList in
          let back := List.skipn splitPoint newList in
          [Leaf front; Leaf back]
        else
          [Leaf newList]
    | InternalNode ks =>
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

    end.

  Definition insert {t : Set} (tr : Tree t) (k : Key) (v : t) : Tree t :=
    match insertNode tr k v with
    | [] => empty (* Impossible *)
    | [tr'] => tr'
    | ns => InternalNode (addTailKeys ns)
    end.

  Fixpoint remove {t : Set} (tr : Tree t) (k : Key) : Tree t :=
    match tr with
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
    end.

  Definition update {t : Set} (tr : Tree t) (k : Key) (v : t) : Tree t :=
    insert (remove tr k) k v.

  Fixpoint lookup {t : Set} (tr : Tree t) (k : Key) : option t :=
    trace ("Looking up " ++ show (treeWithoutValues tr))
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
    end.

  Fixpoint children {t : Set} (tr : Tree t) (k : Key) : list Key :=
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
          List.concat (map (fun ktr => children (snd ktr) k) trs)
      (* This is better, but wrong - on [ [/a/b, /a/b] [/a/c]], wouldn't find /a/c because /a/b > /a.
       * We need to compare the prefix. Also should only recurse if the next node starts after /a. *)
      (* Regardless, reading the whole tree from disk swamps the runtime of this :( *)
      | InternalNode trs => (fix childrenList trs :=
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
      end.

  Definition sampleTree_a := insert empty (Meta "a") "av"%string.
  Definition sampleTree_c := insert sampleTree_a (Meta "c") "cv"%string.
  Definition sampleTree_b := insert sampleTree_c (Meta "b") "bv"%string.
  Definition sampleTree_d := insert sampleTree_b (Meta "d") "dv"%string.
  Definition sampleTree_ab := insert sampleTree_d (Meta "ab") "abv"%string.
  Definition sampleTree_z := insert sampleTree_ab (Meta "z") "zv"%string.
  Definition sampleTree_q := insert sampleTree_z (Meta "q") "qv"%string.
  Definition sampleTree_r := insert sampleTree_q (Meta "r") "rv"%string.
  Definition sampleTree_aa := insert sampleTree_r (Meta "aa") "aav"%string.
  Definition sampleTree := sampleTree_aa.

  Definition sampleHier_a := insert empty (Meta "/a") "av"%string.
  Definition sampleHier_b := insert sampleHier_a (Meta "/b") "av"%string.
  Definition sampleHier_ac := insert sampleHier_b (Meta "/a/c") "av"%string.
  Definition sampleHier_ad := insert sampleHier_ac (Meta "/a/d") "av"%string.
  Definition sampleHier_be := insert sampleHier_ad (Meta "/b/e") "av"%string.
  Definition sampleHier_bef := insert sampleHier_be (Meta "/b/e/f") "av"%string.
  Definition sampleHier_beg := insert sampleHier_bef (Meta "/b/e/g") "av"%string.
  Definition sampleHier := sampleHier_beg.
End Tree.

Extraction Language Haskell.
Require Import Coq.extraction.ExtrHaskellBasic.
Require Import Coq.extraction.ExtrHaskellString.
Extract Inlined Constant fst => "Prelude.fst".
Extract Inlined Constant map => "Prelude.map".
(*Extract Inlined Constant Tree.trace => "trace".
Extract Inlined Constant Tree.show => "Prelude.show". *)
Extract Inlined Constant Tree.trace => "(\ x y -> y)".
Extract Inlined Constant Tree.show => "(\ _ -> """")".
Extraction "MonoBtreeCoq.hs" Tree.
