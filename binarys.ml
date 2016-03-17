let sum_pixel = ref 0

let level (r,g,b) = 
    (0.3 *. float_of_int(r) 
    +. 0.59 *. float_of_int(g) 
    +. 0.11 *. float_of_int(b))/. 255.


let get_coord x y scale =
  ((x - scale/2),(y - scale/2),(x + scale/2),(y + scale/2))


(*binarisation*)
let sauvola args = 
  int_of_float (args.(1) *. (1. +. 0.2 *. ((args.(0) /. 128.) -. 1.)))


(*Calcule image intégral et opération*)
let integral_image src = 
  let (w,h) = Sdl_tools.get_size src in
  let matrix = Matrix.init w h in
    Matrix.set matrix 0 0 (Sdl_tools.get_grey 
                             (Sdlvideo.get_pixel_color src 0 0)); 
    for x = 1 to w - 1 do 
      Matrix.set matrix x 0 ((Matrix.get matrix (x - 1) 0) + 
                             Sdl_tools.get_grey 
                               (Sdlvideo.get_pixel_color src x 0))
    done;
    for y = 1 to h - 1 do
      Matrix.set matrix 0 y ((Matrix.get matrix 0 (y - 1)) +
                              Sdl_tools.get_grey
                                 (Sdlvideo.get_pixel_color src 0 y))
    done;
    for x = 1 to w - 1 do 
        for y = 1 to h - 1 do 
            Matrix.set matrix x y ((Sdl_tools.get_grey 
                               (Sdlvideo.get_pixel_color src x y)) + 
                            (Matrix.get matrix (x - 1) y) +
                            (Matrix.get matrix x (y - 1)) -
                            (Matrix.get matrix (x - 1) (y - 1)))
        done
    done;
      matrix

let average_integral_coordA matrix x y i j =
  ((Matrix.set_ifValid matrix (x - 1) (y - 1))
  +(Matrix.set_ifValid matrix  (x + i - 1) (y + j - 1))
  -(Matrix.set_ifValid matrix  (x + i - 1) (y - 1))
  -(Matrix.set_ifValid matrix  (x - 1) (y + j - 1)))



let average_integral_coordB matrix x y i j =
   (Matrix.set_ifValid matrix  x  y )
  +(Matrix.set_ifValid matrix  i  j )
  -(Matrix.set_ifValid matrix  i  y )
  -(Matrix.set_ifValid matrix  x  j )


let ecart_type src matrix x y i j = 
  let average = average_integral_coordB matrix x y i j in
  let sum = ref 0 and gmax = ref 0 and gmin = ref 255 in
    for x1 = 0 to i - 1 do
      for y1 = 0 to j - 1 do
        let color = Sdl_tools.get_grey (Sdlvideo.get_pixel_color src (x + x1) (y + y1)) in
          sum := !sum + (color - average)*(color - average);
          gmax := max !gmax color;
          gmin := min !gmin color;
      done;
    done;
      [|
        sqrt (float (!sum/(i*j)));  (*0*)
        float average;              (*1*)
        float !gmin;                (*2*)
        float !gmax;                (*3*)
        0.;                         (*4*)
        float x;                    (*5*)
        float y;                    (*6*)    
        float i;                    (*7*)
        float j                     (*8*)
      |]

(*Apply Sauvola*)
let apply_sauvola matrix x y scale src =
  let (nx, ny, ni, nj) = get_coord x y scale in
    sauvola (ecart_type src matrix nx ny ni nj)

(*Apply average integral*)
let get_average_integral matrix x y scale src =
  let (nx,ny,ni,nj) = get_coord x y scale in
    (average_integral_coordB matrix nx ny ni nj) / (scale * scale)

(*set the image to grey scale*)
let color2grey (r,g,b) = 
    let colorpixel = int_of_float ((level (r,g,b)) *. 255.) in
            (colorpixel, colorpixel, colorpixel)

let image2grey src dst =
    let (w,h) = (Sdl_tools.get_size src) in
        for y = 0 to h - 1 do
            for x = 0 to w - 1 do
                Sdlvideo.put_pixel_color dst x y 
                (color2grey (Sdlvideo.get_pixel_color src x y));
            done
        done

let sum_pixel_inc src = 
    let (w,h) = Sdl_tools.get_size src in
        for y = 0 to h - 1 do
            for x = 0 to w - 1 do 
                sum_pixel := !sum_pixel + Sdl_tools.get_grey (Sdlvideo.get_pixel_color src x y) ;
            done
        done

let grey_average src = int_of_float((float !sum_pixel) /. (float (Sdl_tools.get_nbpixel src)))

let grey_average_integral src =
  let matrix = integral_image src in
  let (w,h) = (Matrix.width matrix, Matrix.height matrix) in
  let average =
    int_of_float((float ((Matrix.get matrix 0 0) +
                          (Matrix.get matrix (w - 1) (h - 1)) -
                          (Matrix.get matrix (w - 1) 0 ) -
                          (Matrix.get matrix 0 (h - 1) ))) /.
                            (float (Sdl_tools.get_nbpixel src))) in
    average



let binarys_local f src dst scale =
  let matrix = integral_image src in 
    let (w,h) = (Matrix.width matrix, Matrix.height matrix) in 
      for x = (scale / 2 + 1) to w - ( scale /2 + 1) do
        for y = (scale / 2 + 1) to h - (scale / 2 + 1) do 
          let color = f matrix x y scale src in
            if (Sdl_tools.get_grey (Sdlvideo.get_pixel_color src x y)) 
                 < color then
              Sdlvideo.put_pixel_color dst x y (0,0,0)
            else
              Sdlvideo.put_pixel_color dst x y (255,255,255);
        done
      done
    

                         



let binarys_global src dst = 
    (*sum_pixel_inc dst;*)

    (*let average = int_of_float((float (grey_average dst)) -. ((float (grey_average dst)) *. 20. /. 100. ))in 
     *)
  let new_average = grey_average_integral src in
  let average = int_of_float ((float new_average) -. ((float new_average) *. 20. /. 100.)) in
  let (w,h) = Sdl_tools.get_size dst in 
            for y = 0 to h- 1 do
                for x = 0 to w - 1 do
                    if (Sdl_tools.get_grey (Sdlvideo.get_pixel_color dst x y) < average) then
                        Sdlvideo.put_pixel_color dst x y (0,0,0)
                    else
                        Sdlvideo.put_pixel_color dst x y (255,255,255)
                done
            done
