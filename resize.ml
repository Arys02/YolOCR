(* resize an image *)

(* find resize scale *)
let find_scale image =
  let newh = 300 in
  let h = Sdl_tools.get_height image in
    ((float newh) /. (float h))

(* resize image *)
let resize_image image scale =
  let matrix = Matrix.image_to_matrix image in
  let new_matrix = Matrix.lossy_resize matrix scale in
  let (neww,newh) = Matrix.dim new_matrix in
  let blank_image = Sdl_tools.blank_surface image neww newh in
  let final_image = Matrix.matrix_to_image blank_image new_matrix in
    final_image

(* resize image by bilinear interpolation *)
let resize_bilinear image scale =
  let matrix = Matrix.image_to_matrix image in
  let (w,h) = float_of_int (Matrix.width matrix),
              float_of_int (Matrix.height matrix) in
  let (neww, newh) = (w *. scale),(h *. scale) in
  let new_img = Sdl_tools.blank_surface image
                (int_of_float neww) (int_of_float newh) in
    for i = 0 to (int_of_float neww) - 1 do
      for j = 0 to (int_of_float newh) - 1 do
        let (fi, fj) = float i, float j in
        let (x,y) = fi /. scale, fj /. scale in
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
                      j pixel;
      done;
    done;
    new_img

(* resize matrix by bilinear interpolation *)
let resize_bilinear_matrix matrix scale =
  let (w,h) = Matrix.dim matrix in
  let blank = Sdl_tools.blank_surface "truc" w h in
  let image = Matrix.matrix_to_image blank matrix in
  let r_image = resize_bilinear image scale in
  Otsu.binarize r_image r_image;
  let r_matrix = Matrix.image_to_matrix r_image in
  r_matrix
