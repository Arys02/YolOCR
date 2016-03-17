(* init the matrix at 0 *)
let init dimx dimy = Bigarray.Array2.create Bigarray.int
                     Bigarray.c_layout dimx dimy

let init_float dimx dimy = Bigarray.Array2.create Bigarray.float32
                             Bigarray.c_layout dimx dimy

let width matrix = Bigarray.Array2.dim1 matrix

let height matrix = Bigarray.Array2.dim2 matrix

let dim matrix = Bigarray.Array2.dim1 matrix , Bigarray.Array2.dim2 matrix

let center matrix = (float_of_int (width matrix) /. 2.,
float_of_int (height matrix) /. 2.)

(* check coordinates *)
let isValid matrix x y =
	(x < width matrix && y < height matrix) && (x >= 0 && y >= 0)

(* get element at x y *)
let get matrix x y = Bigarray.Array2.get matrix x y

(*get element, if valid -> element else 0*)
let set_ifValid matrix x y = 
  if ((x < 0)||(y < 0)||(x >= width matrix)||(y >= height matrix)) then
    0
  else 
    (get matrix x y)

(* set element at x y *)
let set matrix x y element = 
    if isValid matrix x y then
    Bigarray.Array2.set matrix x y element

(* set all matrix elements with 'element' *)
let fill matrix element = Bigarray.Array2.fill matrix element

          
(* convert an image to a matrix *)
let image_to_matrix image =
    let (w,h) = Sdl_tools.get_size image in
	let matrix = init w h in
	  for i = 0 to (width matrix)-1 do
	    for j = 0 to (height matrix)-1 do
          let pixel = Sdlvideo.get_pixel_color image i j in
            set matrix i j (Pixel.color pixel);
		done;
	   done;
	   matrix

(* print matrix on screen - debug only *)
let print matrix =
	for i = 0 to (width matrix)-1 do
	  for j = 0 to (height matrix)-1 do
	    Printf.printf " %i" (get matrix i j);
	  done;
      print_newline();
	done

(* create an image from a binarized matrix *)
let matrix_to_image dst matrix =
    let (w,h) = (width matrix),(height matrix) in
      for i = 0 to (w - 1) do
        for j = 0 to (h - 1) do
          let new_pixel = Pixel.int_to_rgb (get matrix i j) in
          Sdlvideo.put_pixel_color dst i j new_pixel;
        done;
      done;
      dst

let deg2rad angle = angle *. 3.14159265359 /. 180.0

(* rotate matrix by angle in degree *)
let rotate matrix angle =
  let angle = deg2rad angle in
  let (cosa,sina) = (cos angle),(sin angle) in
  let (centerx,centery) = center matrix in
  let (w,h) = float_of_int (width matrix),
              float_of_int (height matrix) in
  let (neww, newh) = abs_float(sina *. h +. cosa *. w)
                    ,abs_float(sina *. w +. cosa *. h) in
  let new_matrix = init (int_of_float neww) (int_of_float newh) in
    fill new_matrix 0;
    for i = 0 to (int_of_float neww) - 1 do
      for j = 0 to (int_of_float newh) - 1 do
        let (fi, fj) = (float_of_int i), (float_of_int j) in
        let (x,y) =
          int_of_float (centerx -. (cosa *. (centerx -. fi)) -.
          (sina *. (centery -. fj))),
          int_of_float (centery +. (sina *. (fi -. centerx)) +.
          (cosa *. (centery -. fj))) in
        if (isValid matrix x y) then
          let pixel = get matrix x y in
            set new_matrix (i) ((int_of_float newh)-j) pixel;
     done;
    done;
    new_matrix

(* resize with float casting to int *)
let lossy_resize matrix scale =
    let (w,h) = dim matrix in
    let (fw,fh) = (float w),(float h) in
    let (neww, newh) = (fw *. scale),(fh *. scale) in
    let new_matrix = init (int_of_float neww) (int_of_float newh) in
    for i = 0 to (int_of_float neww) - 1 do
      for j = 0 to (int_of_float newh) - 1 do
        let (fi, fj) = (float i), (float j) in
        let (x, y) = int_of_float (fi /. scale), int_of_float (fj /. scale) in
        if (isValid matrix x y) then
          let pixel = get matrix x y in
            set new_matrix i j pixel;
      done;
    done;
    new_matrix

(* convert an array of array to matrix*)
let array2matrix array = 
  let new_matrix = init_float (Array.length array) (Array.length array.(0)) in
    for i = 0 to (width new_matrix) - 1 do
      for j = 0 to (height new_matrix) - 1 do
        set new_matrix i j (array.(i).(j));
      done;
    done;
      new_matrix

(* return a copy of matrix *)
let copy matrix =
  let (w,h) = dim matrix in
  let new_matrix = init w h in
    for i = 0 to (width matrix)-1 do
	  for j = 0 to (height matrix)-1 do
        let new_elt = get matrix i j in
          set new_matrix i j new_elt;
      done;
	done;
    new_matrix

(* apply a logic AND between two matrix *)  
let logic_and m1 m2 =
  for i = 0 to (width m1) - 1 do
    for j = 0 to (height m1) - 1 do
      let e1 = get m1 i j and
          e2 = get m2 i j in
            if e1 <> e2 then
               set m1 i j 0
    done;
  done;
  m1

(* apply a logic OR between two matrix *)
let logic_or m1 m2 =
  for i = 0 to (width m1) - 1 do
    for j = 0 to (height m1) - 1 do
      let e1 = get m1 i j and
          e2 = get m2 i j in
            if (e1 = e2) then
               set m1 i j e1
            else
               set m1 i j 1
    done;
  done;
  m1

(* extract a sub-matrix from another matrix *)
let extract matrix origin width height =
  let (w,h) = dim matrix in
  let (oi,oj) = origin in
  let new_matrix = init w h in
  for i = 0 to width - 1 do
    for j = 0 to height - 1 do
      let pixel = get matrix (i + oi) (j + oj) in
      set new_matrix i j pixel;
    done;
  done;
  new_matrix
(********************USELESS FUNCTIONS********************)

(* apply matrix rotation transformation by center *)
let rotate_bad matrix angle =
  let angle = deg2rad angle in (*convert to radians*)
    let (centerx, centery) = center matrix in (*get the center*)
      let (mw, mh) = (width matrix), (height matrix)     in
      let new_matrix = init mw mh in
          fill new_matrix 0; (*create a matrix and fill it     with 0*)
        for i = 0 to (width new_matrix)-1 do
            for j = 0 to (height new_matrix)-1 do
            if (get matrix i j) = 1 then (*modify only black pixels*)
            begin
                        let newx = int_of_float (
                            (float_of_int (i - int_of_float(centerx))) *.
                            (cos angle) -.
                            (float_of_int (j - int_of_float(centery))) *.
                            (sin angle) +.
                            (centerx) ) in
                        let newy = int_of_float (
                            (float_of_int (i - int_of_float(centerx))) *.
                            (sin angle) +.
                            (float_of_int (j - int_of_float(centery))) *.
                            (cos angle) +.
                            (centery) ) in
                        set new_matrix newx newy (get matrix i j);
                        end
            done;
        done;
        new_matrix
