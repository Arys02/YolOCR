let first_pixel_of_line matrix line =
    let w = Matrix.width matrix in
    let i = ref 0 in
      while !i < w && (Matrix.get matrix !i line = 0) do
        i := !i + 1;
      done;
      !i
      
let last_pixel_of_line matrix line =
  let w = Matrix.width matrix in
  let i = ref w in
    while !i > 0 && (Matrix.get matrix (!i - 1) line = 0) do
      i := !i - 1;
    done;
    !i
      
let first_pixel_of_col matrix col =
    let h = Matrix.height matrix in
    let j = ref 0 in
     while !j < h && (Matrix.get matrix col !j = 0) do
       j := !j + 1;
     done;
     !j

     
let last_pixel_of_col matrix col =
  let h = Matrix.height matrix in
  let j = ref h in
    while !j > 0 && (Matrix.get matrix col (!j - 1) = 0) do
      j := !j - 1;
    done;
    !j  

let matrix_bounds matrix =
 let (w,h) = Matrix.dim matrix in
  let first_pixel_col = ref (h - 1) in
  let last_pixel_col = ref 0 in
    for i = 0 to (w - 1) do
      let new_first_pixel = first_pixel_of_col matrix i and
          new_last_pixel = last_pixel_of_col matrix i in
      first_pixel_col := min !first_pixel_col new_first_pixel;
      last_pixel_col := max !last_pixel_col new_last_pixel;
    done;
  let first_pixel_line = ref (w-1) in
  let last_pixel_line = ref 0 in
    for j = 0 to (h - 1) do
      let new_first_pixel = first_pixel_of_line matrix j and
          new_last_pixel = last_pixel_of_line matrix j in
       first_pixel_line := min !first_pixel_line new_first_pixel;
       last_pixel_line := max !last_pixel_line new_last_pixel;
    done;
    (!first_pixel_line, !last_pixel_line, !first_pixel_col, !last_pixel_col) 

let crop_matrix matrix =
    let (fl, ll, fc, lc) = matrix_bounds matrix in
    let (neww,newh) = (ll - fl), (lc - fc) in
    let new_matrix = Matrix.init neww newh in
      for i = 0 to neww - 1 do
        for j = 0 to newh - 1 do
          let pixel = Matrix.get matrix (i + fl) (j + fc) in
          Matrix.set new_matrix i j pixel;
        done;
      done;
      new_matrix

let crop_image image =
  let matrix = Matrix.image_to_matrix image in
  let cropped_matrix = crop_matrix matrix in
  let (neww,newh) = Matrix.dim cropped_matrix in
  let blank = Sdl_tools.blank_surface image neww newh in
  (Matrix.matrix_to_image blank cropped_matrix)
