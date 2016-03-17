type t_block = {
  origin: int*int;
  width: int;
  height: int;
}

(* create a new block *)
let create ox oy w h =
  { origin = (ox,oy); width = w; height = h}

let isValid block = 
  let (ox,oy) = block.origin in
    (block.height > 0 && block.width > 0) && (ox > 0 && oy > 0)

    (* extract the matrix from a block *) 
let matrix_of_block matrix block = 
  let (oi,oj) = block.origin in 
  let new_matrix = Matrix.init block.width block.height in 
  let (neww,newh) = Matrix.dim new_matrix in 
    for j = 0 to newh - 1 do 
      for i = 0 to neww - 1 do 
        let pixel = Matrix.get matrix (i + oi) (j + oj) in 
          Matrix.set new_matrix i j pixel; 
      done; 
    done; 
    new_matrix

(* set a new extracted matrix into a bigger one *)
let set_matrix matrix block new_matrix =
  let (oi,oj) = block.origin in
  for j = 0 to block.height - 1 do
    for i = 0 to block.width - 1 do
      let pixel = Matrix.get new_matrix i j in
      Matrix.set matrix (i + oi) (j + oj) pixel;
    done;
  done

(* draw a border of the block *)
let draw_border matrix block =
  let (oi,oj) = block.origin in
  for i = 0 to block.width - 1 do
    Matrix.set matrix (i + oi) 0 1;
    Matrix.set matrix (i + oi) (oj + block.height - 1) 1;
  done;
  for j = 0 to block.height - 1 do
    Matrix.set matrix 0 (j + oj) 1;
    Matrix.set matrix (oi + block.width - 1) (j + oj) 1;
  done


