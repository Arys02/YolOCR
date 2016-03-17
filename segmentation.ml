(* Character type *)
type 'a character =
  Return
| Space
| Letter of 'a

(* extract characters into trees *)
(* check if line is completely white *)
let is_line_white matrix j =
  let (w,h) = Matrix.dim matrix in
  let i = ref 0 in
  while !i < w && Matrix.get matrix !i j = 0 do
    i := !i + 1;
  done;
  (!i = w)

(* check if column is completely white *)
let is_column_white matrix block i =
  let (oi,oj) = block.Block.origin in
  let j = ref 0 in
  while !j < block.Block.height && Matrix.get matrix i (!j + oj) = 0 do
    j := !j + 1;
  done;
  (!j = block.Block.height)

(* average width of horizontal white space *)
let white_columns_average matrix block =
  let (oi,oj) = block.Block.origin in
  let nb = ref 0 and
      sum = ref 0 and
      i = ref 0 in
  while !i < block.Block.width do
      if is_column_white matrix block (!i + oi) then
        begin
          nb := !nb + 1;
          while !i < block.Block.width
                && is_column_white matrix block (!i + oi) do
            i := !i + 1;
            sum := !sum + 1;
          done;
        end
      else
        i := !i + 1;
  done;
  ((float !sum) /. (float !nb))

(* crop white borders of a line *)
let crop_line matrix block =
  let first_pixel = ref block.Block.width and
      last_pixel = ref 0 in
  let (oi, oj) = block.Block.origin in
  let i = ref oi and
      j = ref oj in
  while !j < block.Block.height + oj do
    while !i < block.Block.width && Matrix.get matrix !i !j = 0 do
      i := !i + 1;
    done;
    if !i < block.Block.width && !i < !first_pixel then
      first_pixel := !i;
    i := oi;
    j := !j + 1;
  done;
  j := oj;
  i := block.Block.width - 1;
  while !j < block.Block.height + oj do
    while !i > 0 && Matrix.get matrix !i !j = 0 do
      i := !i - 1;
    done;
    if !i > 0 && !i > !last_pixel then
      last_pixel := !i;
    i := block.Block.width - 1;
    j := !j + 1;
  done;
  Block.create !first_pixel  oj
    (block.Block.width + 1 - (!first_pixel - oi)
     - (block.Block.width - !last_pixel + 1))
    block.Block.height

(* get line block *)
let get_line start_line matrix =
  let (w,h) = Matrix.dim matrix in
  let jmin = ref 0 and
      jmax = ref 0 and
      j = ref start_line in
  while !j < h && is_line_white matrix !j do
    j := !j + 1;
  done;
  if (!j < h) then
    begin
      jmin := !j;
      while !j < h && not(is_line_white matrix !j) do
        j := !j + 1;
      done;
      jmax := !j;
    end;
    Block.create 0 !jmin w (!jmax - !jmin)

(* get tree with lines *)
let get_lines matrix old_line tree =
  let (w,h) = Matrix.dim matrix in
  let current_line = ref old_line in 
  while !current_line < h do
    let line y =
      let block = get_line y matrix in
      let (ox,oy) = block.Block.origin in
          (oy,oy + block.Block.height)
   in
    let (f,l) = line !current_line in
    if l - f > 0 then
      begin
        Tree.add_son (Tree.create (crop_line matrix (Block.create 0 f w (l - f)))) tree;
        current_line := l;
      end
    else
      current_line := !current_line + 1
  done

(* get blocks from other blocks on a tree *)
let get_blocks matrix block =
  let (oi,oj) = block.Block.origin in
  let k = ref 0 and
      i = ref 0 in
  let blocks_tree = ref (Tree.create block) in
  while !k < block.Block.width do
    i := !k;
    while !i < block.Block.width && not(is_column_white matrix block (!i+oi)) do
      i := !i + 1;
    done;
    let new_block = Block.create (oi + !k) oj (!i - !k) block.Block.height in
    if Block.isValid new_block then
    Tree.add_son (Tree.create (new_block)) !blocks_tree;
      while !i < block.Block.width && is_column_white matrix block (!i + oi) do
        i := !i + 1;
      done;
      k := !i + 1;
  done;
  !blocks_tree

(* get blocks from a tree *)
let get_blocks_tree matrix tree =
  tree.Tree.sons <- (get_blocks matrix (tree.Tree.key)).Tree.sons 

(* rlsa words from a tree of phrases *)
let rlsa_words matrix tree =
    let block = tree.Tree.key in
    let block_matrix = Block.matrix_of_block matrix block in
    let rlsad_matrix = Rlsa.rlsa_phrases_matrix block_matrix in
      Block.set_matrix matrix block rlsad_matrix

(* get final tree from matrix *)
let get_tree matrix realmatrix =
  let (w,h) = Matrix.dim matrix in
  let tree = Tree.create (Block.create 0 0 w h) in
  get_lines matrix 0 tree;
  let word_matrix = Matrix.copy realmatrix in (*rlsa the words *)
  for i = 0 to (Tree.sons tree) - 1 do
    let line = Tree.nth_son tree i in
    get_blocks_tree matrix line;
    for j = 0 to (Tree.sons line) - 1 do
      let phrase = Tree.nth_son line j in
      rlsa_words word_matrix phrase;
      get_blocks_tree word_matrix phrase;
      for k = 0 to (Tree.sons phrase) - 1 do
        let word = Tree.nth_son phrase k in
        get_blocks_tree realmatrix word;
      done;
    done;
  done;
  tree

(* debug only - draw border on blocks *)
let rec draw_black_border matrix tree =
  if Tree.sons tree = 0 then
    Block.draw_border matrix (tree.Tree.key)
  else
    for i = 0 to (Tree.sons tree) - 1 do
      draw_black_border matrix (Tree.nth_son tree i)
    done


(* debut only - get images by words *)
let rec save_to_image matrix tree image =
  let folder = ref "" in
  let file = ref "" in
  for i = 0 to (Tree.sons tree) - 1 do
    let line = Tree.nth_son tree i in
    for j = 0 to (Tree.sons line) - 1 do
      folder := string_of_int j;
      let phrase = Tree.nth_son line j in
      for k = 0 to (Tree.sons phrase) - 1 do
        let word = Tree.nth_son phrase k in
        for l = 0 to (Tree.sons word) - 1 do
          file := string_of_int l;
          let letter = Tree.nth_son word l in
          let letter_matrix = Block.matrix_of_block matrix letter.Tree.key in
          let adjusted_matrix = Adjust.adjust_matrix letter_matrix 30 in
          let surface = Sdl_tools.blank_surface image
                        (Matrix.width adjusted_matrix)
                        (Matrix.height adjusted_matrix) in
          let final_image = Matrix.matrix_to_image surface adjusted_matrix in
          Sdlvideo.save_BMP final_image ((string_of_int i) ^ (string_of_int j) ^ (string_of_int k) ^ (string_of_int l))
        done
      done
    done
  done
