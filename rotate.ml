let deg2rad angle = angle *. 3.14159265359 /. 180.0

(* apply image rotation using Sdlgfx *)
let rotateSdl image angle = 
    let img = ref image in
    img := Pixel.invert_image image;
    img := Sdlgfx.rotozoomSurfaceXY !img angle 1.0 1.0 true;
    img := Pixel.invert_image !img;
    !img

(* rotate image using matrix rotation *)                
let rotate_image image angle =
    let matrix = Matrix.image_to_matrix image in
    let new_matrix = Matrix.rotate matrix angle in
    let (neww, newh) = Matrix.dim new_matrix in
    let blank_image = (Sdl_tools.blank_surface image neww newh) in
    let final_image = Matrix.matrix_to_image blank_image new_matrix in
      final_image

(* get number of black pixels per line *)
let number_of_black_pixels matrix j =
    let sum = ref 0 in
      for i = 0 to (Matrix.width matrix)-1 do
        if (Matrix.get matrix i j = 1) then
          sum:= !sum +1;
      done;
      !sum

(* get average variance of an array *)
let array_variance myarray average =
  let length = Bigarray.Array1.dim myarray in
  let sum = ref 0.0 in
    for i = 0 to (length - 1) do
      sum := !sum +. abs_float(average -. myarray.{i});
    done;
    (!sum /. float_of_int(length))

(* get variance from the matrix *) 
let get_variance_matrix matrix =
  let (w,h) = Matrix.dim matrix in
  let variance_array = Bigarray.Array1.create
                        Bigarray.float32
                        Bigarray.c_layout h
  in
  let sum = ref 0.0 in
    for i = 0 to (h - 1) do
      let value = float_of_int(number_of_black_pixels matrix i) in
        Bigarray.Array1.set variance_array i value;
        sum := !sum +. value;
    done;
    let average = (!sum /. (float_of_int h)) in
      sum := 0.0;
      for i = 0 to (h -1) do
      sum := !sum +.
          (abs_float(Bigarray.Array1.get variance_array i -. average));
      done;
      (!sum /. (float_of_int h))

(* find the right angle for rotation !!!TO OPTIMIZE!! *) 
let find_angle_matrix matrix = 
   let limit = 15  in
   let best_angle_variance = ref (0.0,0.0) in
    for i = -(limit) to limit do
      let mat = ref matrix in
      mat := Crop.crop_matrix (Matrix.rotate !mat (float_of_int i));
      let variance = get_variance_matrix !mat in
      let (best_angle,best_variance) = !best_angle_variance in
        if variance > best_variance then
          best_angle_variance := (float_of_int i),(variance);
    done;
    let (best_angle,best_variance) = !best_angle_variance in
      best_angle

(* find the decimals of the right angle !!!TO OPTIMIZE!!  *)
let find_angle_matrix_decimal matrix =
   let limit = 10  in
   let best_angle_variance = ref (0.0,0.0) in
    for i = -(limit) to limit do
      let mat = ref matrix in
      mat := Matrix.rotate !mat ((float_of_int i) /. 10.);
      let variance = get_variance_matrix !mat in
      let (best_angle,best_variance) = !best_angle_variance in
        if variance > best_variance then
          best_angle_variance := ((float_of_int i) /. 10.),(variance);
    done;
    let (best_angle,best_variance) = !best_angle_variance in
      best_angle

(* find the decimals of the right angle !!!TO OPTIMIZE!!  *)
let find_angle_matrix_decimal2 matrix =
   let limit = 10  in
   let best_angle_variance = ref (0.0,0.0) in
    for i = -(limit) to limit do
      let mat = ref matrix in
      mat := Matrix.rotate !mat ((float_of_int i) /. 100.);
      let variance = get_variance_matrix !mat in
      let (best_angle,best_variance) = !best_angle_variance in
        if variance > best_variance then
          best_angle_variance := ((float_of_int i) /. 100.),(variance);
    done;
    let (best_angle,best_variance) = !best_angle_variance in
      best_angle

(* returns the right angle *)
let find_angle image =
  let matrix = Matrix.image_to_matrix image in
  let angle = find_angle_matrix matrix in
    let rotated_matrix = Matrix.rotate matrix angle in
      let decimal_angle = (find_angle_matrix_decimal rotated_matrix) in
        let rotated_matrix_decimal = Matrix.rotate rotated_matrix decimal_angle
        in

      (angle +.
      (find_angle_matrix_decimal rotated_matrix) +. 
      (find_angle_matrix_decimal2 rotated_matrix_decimal))

(* rotate by bilinear interpolation *)
let rotate_bilinear image angle =
   let matrix = Matrix.image_to_matrix image in
   let angle = Matrix.deg2rad angle in
   let (cosa,sina) = (cos angle),(sin angle) in
   let (centerx,centery) = Matrix.center matrix in
   let (w,h) = float_of_int (Matrix.width matrix),
               float_of_int (Matrix.height matrix) in
   let (neww, newh) = abs_float(sina *. h +. cosa *. w)
                     ,abs_float(sina *. w +. cosa *. h) in
   let new_img = Sdl_tools.blank_surface image
                 (int_of_float neww) (int_of_float newh) in
     for i = 0 to (int_of_float neww) - 1 do
       for j = 0 to (int_of_float newh) - 1 do
         let (fi, fj) = (float_of_int i), (float_of_int j) in
         let (x,y) = (centerx -. (cosa *. (centerx -. fi)) -.
           (sina *. (centery -. fj))),
                     (centery +. (sina *. (fi -. centerx)) +.
           (cosa *. (centery -. fj))) in
 
           let (ax,ay) = int_of_float (x),int_of_float (y) in
           let (bx,by) = int_of_float (x) + 1,int_of_float (y) in
           let (cx,cy) = int_of_float (x),int_of_float (y) + 1 in
           let (dx,dy) = int_of_float (x) + 1,int_of_float (y) + 1 in
           let ca = ref 0 and cb = ref 0 and cc = ref 0 and cd = ref 0
           in
 
           if (Matrix.isValid matrix ax ay) then
             ca := (Matrix.get matrix ax ay) * 255;
           if (Matrix.isValid matrix bx by) then
             cb := (Matrix.get matrix bx by) * 255;
           if (Matrix.isValid matrix cx cy) then
             cc := (Matrix.get matrix cx cy) * 255;
           if (Matrix.isValid matrix dx dy) then
             cd := (Matrix.get matrix dx dy) * 255;
   
           let (xf,yf) = x -. (float ax), y -. (float ay) in
  
           let grey = int_of_float (
                      ((1. -. xf) *. (1. -. yf) *. (float !ca)) +.
                      ( xf *. (1. -. yf) *. (float !cb)) +.
                      ((1. -. xf) *. yf *. (float !cc)) +.
                      (xf *. yf *. (float !cd)))
                      in
         
           let pixel = (255-grey,255-grey,255-grey) in
              Sdlvideo.put_pixel_color new_img i
              ((int_of_float newh) - 1 - j) pixel;
       done;
     done;
     new_img

(* how many black pixels around center point *)
let black_pixels_nb matrix i j =
  let nb = ref 0 in
  for l = j - 1 to j + 1 do
    for k = i - 1 to i + 1 do
      if l <> j && k <> i then (* do not test central pixel *)
        if Matrix.isValid matrix k l then
          nb := !nb + (Matrix.get matrix k l);
    done;
  done;
  !nb


(* smoothen matrix after rotation *)
let smooth matrix = 
  let (w,h) = Matrix.dim matrix in
  let new_matrix = Matrix.init w h in
  for j = 0 to h do
    for i = 0 to w do
      if black_pixels_nb matrix i j >= 4 then
        Matrix.set new_matrix i j 1
      else
        Matrix.set new_matrix i j 0
    done;
  done;
  new_matrix

(* rotate image using matrix rotation *)                
let rotate_bad image angle =
    let matrix = Matrix.image_to_matrix image in
    let new_matrix = Matrix.rotate_bad matrix angle in
    let (neww, newh) = (Matrix.width new_matrix),(Matrix.height new_matrix) in
    let blank_image = (Sdl_tools.blank_surface image neww newh) in
    let final_image = Matrix.matrix_to_image blank_image new_matrix in
      final_image
