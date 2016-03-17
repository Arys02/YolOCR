let sum_pixel = ref 0

let sum_pixel_inc matrix = 
  for i = 0  to 2 do 
    for j = 0 to 2 do
      sum_pixel := !sum_pixel + Bigarray.Array2.get matrix i j
    done
  done

let sum_pixel_reset = sum_pixel := 0

let average_matrix_int matrix = int_of_float ((float !sum_pixel) /. 9.)


let mat_fun = function
  |3 ->
      Matrix.array2matrix [|
        [|0.0625 ; 0.125 ; 0.0625|];
        [|0.1250 ; 0.250 ; 0.1250|];
        [|0.0625 ; 0.125 ; 0.0625|]
      |]
  |5 ->
      Matrix.array2matrix [|
        [|0.003663004 ; 0.014652015 ; 0.025641026 ; 0.014652015 ; 0.003663004|];
        [|0.014652015 ; 0.058608059 ; 0.095238095 ; 0.058608059 ; 0.014652015|];
        [|0.025641026 ; 0.095238095 ; 0.150183150 ; 0.095238095 ; 0.025641026|];
        [|0.014652015 ; 0.058608059 ; 0.095238095 ; 0.058608059 ; 0.014652015|];
        [|0.003663004 ; 0.014652015 ; 0.025641026 ; 0.014652015 ; 0.003663004|]
      |]
  |7 ->
      Matrix.array2matrix [|
        [|0.00000067 ; 0.00002292 ; 0.00019117 ; 0.00038771 ; 0.00019117 ; 0.00002292 ; 0.00000067|];
        [|0.00002292 ; 0.00078633 ; 0.00655965 ; 0.01330373 ; 0.00655965 ; 0.00078633 ; 0.00002292|];
        [|0.00019117 ; 0.00655965 ; 0.05472157 ; 0.11098164 ; 0.05472157 ; 0.00655965 ; 0.00019117|];
        [|0.00038771 ; 0.01330373 ; 0.11098164 ; 0.22508352 ; 0.11098164 ; 0.01330373 ; 0.00038771|];
        [|0.00019117 ; 0.00655965 ; 0.05472157 ; 0.11098164 ; 0.05472157 ; 0.00655965 ; 0.00019117|];
        [|0.00002292 ; 0.00078633 ; 0.00655965 ; 0.01330373 ; 0.00655965 ; 0.00078633 ; 0.00002292|];
        [|0.00000067 ; 0.00002292 ; 0.00019117 ; 0.00038771 ; 0.00019117 ; 0.00002292 ; 0.00000067|]
      |]
  |_ -> failwith "mauvais rayon de matrice gaussienne"


let mat_num = function
     3 -> 1
    |5 -> 2
    |7 -> 3
    |_ -> failwith "mauvais rayon de matrice gaussienne"


let get_image2matrix src matrix x y =  
  Matrix.set matrix 0 0 (Sdl_tools.get_grey 
                           (Sdlvideo.get_pixel_color src (x - 1) (y - 1)));
  Matrix.set matrix 0 1 (Sdl_tools.get_grey
                           (Sdlvideo.get_pixel_color src  x      (y - 1)));
  Matrix.set matrix 0 2 (Sdl_tools.get_grey
                           (Sdlvideo.get_pixel_color src (x + 1) (y - 1)));
  Matrix.set matrix 1 0 (Sdl_tools.get_grey
                           (Sdlvideo.get_pixel_color src (x - 1)  y));
  Matrix.set matrix 1 1 (Sdl_tools.get_grey
                           (Sdlvideo.get_pixel_color src x       y));
  Matrix.set matrix 1 2 (Sdl_tools.get_grey
                           (Sdlvideo.get_pixel_color src (x + 1)  y));
  Matrix.set matrix 2 0 (Sdl_tools.get_grey
                           (Sdlvideo.get_pixel_color src (x - 1) (y + 1)));
  Matrix.set matrix 2 1 (Sdl_tools.get_grey 
                           (Sdlvideo.get_pixel_color src x      ( y + 1;)));
  Matrix.set matrix 2 2 (Sdl_tools.get_grey
                           (Sdlvideo.get_pixel_color src (x + 1) (y + 1)));
  matrix

let apply_filter matrix filter = 
  let value = ref 0 in
    if (Matrix.width matrix <> Matrix.width filter) ||
       (Matrix.height matrix <> Matrix.height filter)then
      failwith "Error: matrix and filter haven't the same width or heigth."
    else
      (
        for i = 0 to (Matrix.width matrix) - 1 do
            for j = 0 to (Matrix.height matrix) - 1 do 
                value := 
                    int_of_float
                      ((float !value) +. 
                       (float (Matrix.get matrix i j)) *.
                       (Matrix.get filter i j))
            done
        done;
        !value
      )

let convolution src dst matrix =
  let (w,h) = Sdl_tools.get_size src in
  let (decalX, decalY) = 
    (-(Matrix.width matrix)/2,-(Matrix.height matrix)/2 ) in
    for x = 0 to w - 1 do
      for y = 0 to h - 1 do
        let result = ref 0. in
          for i = 0 to Matrix.width matrix - 1 do
            for j = 0 to Matrix.height matrix - 1 do
              let color = 
                Sdl_tools.get_grey (Sdlvideo.get_pixel_color 
                  src 
                  ((x + i + decalX) mod w)
                  ((y + j + decalY) mod h)) in
                result := !result +. (Matrix.get matrix i j) *. (float color);
            done;
          done;
          let c = (int_of_float !result) in
            Sdlvideo.put_pixel_color dst x y (c,c,c)
      done
    done


let gauss_blur src dst gauss_mat_size = 
  convolution src dst (mat_fun gauss_mat_size)

let flou src dst gauss_int = 
  let (w,h) = Sdl_tools.get_size src in
    for y = (mat_num gauss_int) to h - (mat_num gauss_int)+1 do 
      for x = (mat_num gauss_int) to w - (mat_num gauss_int)+1 do
        let new_matrix = Matrix.init gauss_int gauss_int in 
            let new_matrix = get_image2matrix src new_matrix x y in  
            let new_color = apply_filter new_matrix (mat_fun gauss_int)in
                Sdlvideo.put_pixel_color dst x y 
                (new_color, new_color, new_color);
      done
    done


let median src dst = 
  let (w,h) = Sdl_tools.get_size dst in 
    for y = 1 to h - 2 do
      for x = 1 to w - 2 do
        let new_matrix = Matrix.init 3 3 in 
        let new_matrix = get_image2matrix src new_matrix x y in
        let median = ref [] in 
          begin
            for i = 0 to 2 do
              for j = 0 to 2 do 
                median := 
                Matrix.get new_matrix i j::!median;
              done
            done;
            let new_median = 
              List.sort 
                (fun x y -> if x > y then 1 else -1) 
                !median in
            let new_color = List.nth new_median 4 in
              Sdlvideo.put_pixel_color dst x y (new_color,new_color,new_color)
          end
      done
    done;



