(* Adjust letter in order to get similar images for the neuronal network *)

let adjust_matrix matrix size =
  let cropped_matrix = Crop.crop_matrix matrix in (* crop white borders *)
  let (w,h) = Matrix.dim cropped_matrix in
  let scale = (float size) /. (float h) in (* get resize scale *)
  let scaled_matrix = Resize.resize_bilinear_matrix cropped_matrix scale in
  let adjusted_matrix = Matrix.init size size in (* final matrix *)
  let h_border_size = (size - Matrix.width scaled_matrix) / 2 in
  let v_border_size = (size - Matrix.height scaled_matrix) / 2 in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      if Matrix.isValid scaled_matrix
      (i - h_border_size) (j - v_border_size) then
        begin
        let pixel = Matrix.get scaled_matrix (i - h_border_size) (j -
        v_border_size) in
        Matrix.set adjusted_matrix i j pixel
      end
      else
        Matrix.set adjusted_matrix i j 0;
    done;
  done;
adjusted_matrix

let adjust image size =
  let matrix = Matrix.image_to_matrix image in
  let adjusted_matrix = adjust_matrix matrix size in
  let blank = Sdl_tools.blank_surface image size size in
  let adjusted_image = Matrix.matrix_to_image blank adjusted_matrix in
  adjusted_image
  
