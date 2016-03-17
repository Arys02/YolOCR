(* general tree implementation with records *)
type 'a t_tree = {
  mutable key: 'a;
  mutable sons: 'a t_tree list;
}

(* create a new tree *)
let create x = {key = x; sons = []}

let get_list tree = tree.sons

(* get nb sons *)
let sons tree = List.length (tree.sons)

(* get the nth son of the tree *)
let nth_son tree n = List.nth (tree.sons) n

(* add son x of 'a t_tree to tree *)
let add_son x tree = tree.sons <- tree.sons@[x]

(* apply f to all nodes profondeur *)
let rec parc_prof f tree =
  f tree.key;
  for i = 0 to (sons tree) - 1 do
    parc_prof f (nth_son tree i)
  done

(* parc_prof only on leafs *)
let rec parc_prof_leaf f tree =
  if sons tree = 0 then
    f tree.key
  else
    for i = 0 to (sons tree - 1) do
      parc_prof_leaf f (nth_son tree i)
    done
