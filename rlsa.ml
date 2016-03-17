(* Run Length Smooth Algorithm (AND) for text segmentation *)

(* horisontal RLSA *)
let rlsa_matrix_h matrix thresh = 
  let (w,h) = Matrix.dim matrix in
  let i = ref 0 and
      j = ref 0 and
      first_white = ref 0 and
      sum = ref 0 in
  while !j < h do
    while !i < w do
      if Matrix.get matrix !i !j = 0 then
        begin
          first_white := !i;
          while !i < w && Matrix.get matrix !i !j = 0 do
            sum := !sum + 1;
            i := !i + 1
          done;

          if !sum <= thresh then
            begin
              for k = !first_white to (!i - 1) do
                Matrix.set matrix k !j 1
              done
            end;
          
          sum := 0;
          first_white := 0;
        end;
        i := !i + 1;
    done;
    i := 0;
    j := !j + 1;
  done;
  matrix

(* vertical RLSA *)
let rlsa_matrix_v matrix thresh = 
  let (w,h) = Matrix.dim matrix in
  let i = ref 0 and
      j = ref 0 and
      first_white = ref 0 and
      sum = ref 0 in
  while !i < w do
    while !j < h do
      if Matrix.get matrix !i !j = 0 then
        begin
          first_white := !j;
          while !j < h && Matrix.get matrix !i !j = 0 do
            sum := !sum + 1;
            j := !j + 1
          done;

          if !sum <= thresh then
            begin
              for k = !first_white to (!j - 1) do
                Matrix.set matrix !i k 1
              done
            end;
          
          sum := 0;
          first_white := 0;
        end;
        j := !j + 1;
    done;
    j := 0;
    i := !i + 1;
  done;
  matrix
 
(* clean lines = 1px *)
let clean_matrix matrix =
  let (w,h) = Matrix.dim matrix in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      if Matrix.get matrix i j = 1 then
        begin
          if Matrix.isValid matrix i (j - 1) && Matrix.get matrix i (j - 1) = 0
          && Matrix.isValid matrix i (j + 1) && Matrix.get matrix i (j + 1) = 0
          then
            Matrix.set matrix i j 0;
        end;
    done;
  done;
  matrix


(* apply segmentation to get lines *)
let rlsa_matrix matrix =
  let other_matrix = Matrix.copy matrix in
  let h_matrix = rlsa_matrix_h matrix 20 and
      v_matrix = rlsa_matrix_v other_matrix 160 in
  let and_matrix = Matrix.logic_and h_matrix v_matrix in
  let new_matrix = clean_matrix and_matrix in
  new_matrix
  
let rlsa_image image =
  let matrix = Matrix.image_to_matrix image in
  let (w,h) = Matrix.dim matrix in
  let new_matrix = rlsa_matrix matrix in 
  let blank = Sdl_tools.blank_surface image w h in
  let new_image = Matrix.matrix_to_image blank new_matrix in
  new_image


(* apply segmentation to get words *)
let rlsa_phrases_matrix matrix =
  let other_matrix = Matrix.copy matrix in
  let h_matrix = rlsa_matrix_h matrix 10 and
      v_matrix = rlsa_matrix_v other_matrix 6 in
  let and_matrix = Matrix.logic_and h_matrix v_matrix in
  let h_and_matrix = rlsa_matrix_h and_matrix 4 in
  let new_matrix = clean_matrix h_and_matrix in
  new_matrix

(* useless functions *)
let blocks_average_height matrix =
  let (w,h) = Matrix.dim matrix in
  let nb = ref 0 and
      sum = ref 0 and
      i = ref 0 and
      j = ref 0 in
  while !i < w do
    while !j < h do
      if Matrix.get matrix !i !j = 1 then
        begin
          sum := !sum + 1;
          nb := !nb + 1;
          while !j < h && Matrix.get matrix !i !j = 1 do
            sum := !sum + 1;
            j := !j + 1;
          done;
        end
      else
        j := !j + 1;
    done;
    j := 0;
    i := !i + 1;
  done;
  ((float !sum) /. (float !nb))

let remove_images matrix =
  let (w,h) = Matrix.dim matrix in
  let average = blocks_average_height matrix in
  let first_black = ref 0 and
      j = ref 0 and
      hblock = ref 0 in
  for i = 0 to w - 1 do
    while !j < w do
      if Matrix.get matrix i !j = 1 then
        begin
          first_black := !j;
          while !j < h && Matrix.get matrix i !j = 1 do
            hblock := !hblock + 1;
            j := !j + 1;
          done;
          
          if (float !hblock) > average *. 2. then
            begin
              for k = !first_black to !j do
                Matrix.set matrix i k 0;
              done;
            end;
            first_black := 0;
            hblock := 0;
        end
      else
        j := !j + 1;
    done;
    j := 0;
  done;
  matrix
