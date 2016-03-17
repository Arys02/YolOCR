let usage = "YolOCR"

exception MissingArg of string

let main() = 
      let path = ref "" and
        use_global = ref false and
        use_otsu = ref false and
        use_local9 = ref false and
        use_local11 = ref false and
        use_gauss = ref false and
        use_bilinear = ref false and
        use_good = ref false and
        use_bad = ref false and
        use_line = ref false and
        use_scale = ref false and
        use_adjust = ref false
         in

    let arguments = [
      ("-i", Arg.Set_string path, "input image");
      ("-global", Arg.Set use_global, "Use global threshold");
      ("-otsu", Arg.Set use_otsu, "Use ostu threshold");
      ("-local9", Arg.Set use_local9, "Use local threshold 9");
      ("-local11", Arg.Set use_local11, "Use local threshold 11");
      ("-gauss", Arg.Set use_gauss, "Use gussien blur");
      ("-bilinear", Arg.Set use_bilinear, "Use bilinear rotation");
      ("-good", Arg.Set use_good, "Use inverse rotation");
      ("-bad", Arg.Set use_bad, "Use bad rotation");
      ("-line", Arg.Set use_line, "Use line detection");
      ("-scale", Arg.Set use_scale, "Use scale by 0.5");
      ("-adjust", Arg.Set use_adjust, "Use adjust");

    ] in
    
    begin
      Arg.parse arguments (fun _ -> ()) usage;

      let file = !path in

    Sdl.init [`VIDEO];
    let image = Sdlloader.load_image file in
    Printf.printf "Image loaded. \n";
    let treated_image = ref image in
    let screen = Sdlvideo.set_video_mode
                (Sdl_tools.get_width image)
                (Sdl_tools.get_height image)
                [`DOUBLEBUF] in

    Sdl_tools.display image screen; 
    Sdl_tools.wait_key();

    Printf.printf "Applying grayscale \n";
    Binarys.image2grey image image;

    if !use_gauss then begin
      Printf.printf "Applying gaussian filter \n";
      Filters.gauss_blur image image 7;
    end;

    if !use_global then begin
      Printf.printf "Applying global binarisation \n";
      Binarys.binarys_global image image;
    end;

    if !use_otsu then begin
      Printf.printf "Applying otsu binarisation \n";
      Otsu.binarize image image;
    end;

    if !use_local9 then begin
      Printf.printf "Applying local (51) binarisation \n";
      Binarys.binarys_local Binarys.apply_sauvola image image 21;
    end;
    
    if !use_local11 then begin   
      Printf.printf "Applying local (11) binarisation \n";
      Binarys.binarys_local Binarys.get_average_integral image image 21;
    end;
   
    
    Sdl_tools.display image screen; 
    Sdl_tools.wait_key();
    
    let new_image = ref image in
    
    if(!use_bilinear || !use_good || !use_bad) then 
   
      begin
      
      let scale = Resize.find_scale !new_image in   
      Printf.printf "Scaling image by %f \n" scale;
      let scaled_image = Resize.resize_image image scale in

      Printf.printf "Searching for angle... \n";
      let angle = Rotate.find_angle scaled_image in    
      Printf.printf "Best angle: %f \n" angle;
      
      if !use_bilinear then begin
        Printf.printf "Rotating image using bilinear interpolation... \n";
        new_image := Rotate.rotate_bilinear image angle;
        Otsu.binarize !new_image !new_image;
      end;

      if !use_good then begin
        Printf.printf "Rotating image using inverse rotation... \n";
        new_image := Rotate.rotate_image image angle;
      end;
   
      if !use_bad then begin     
        Printf.printf "Rotating image using classic rotation... \n";
        new_image := Rotate.rotate_bad image (-.angle);
      end;
      
      treated_image := !new_image; 

      let screen = Sdlvideo.set_video_mode
                  (Sdl_tools.get_width !new_image)
                  (Sdl_tools.get_height !new_image)
                  [`DOUBLEBUF] in

      Sdl_tools.display !new_image screen;  
      Sdl_tools.wait_key();
   
      end;

    Sdlvideo.save_BMP !new_image "output.bmp";
      if !use_line then
        begin
          new_image := Rlsa.rlsa_image !new_image;
          let matrix = Matrix.image_to_matrix !new_image in
          let realmatrix = Matrix.image_to_matrix !treated_image in
          let tree = Segmentation.get_tree matrix realmatrix in
          Segmentation.save_to_image realmatrix tree !new_image;
          Segmentation.draw_black_border realmatrix tree;
          new_image := Matrix.matrix_to_image !new_image realmatrix;
          Sdl_tools.display !new_image screen; 
          Sdl_tools.wait_key(); 
      end;

     if (!use_scale) then
        begin
          let scale = 2. in
          let n_image = Resize.resize_bilinear !new_image scale in
          Binarys.binarys_global n_image n_image;
          let other = Resize.resize_image !new_image scale in
          Sdl_tools.display n_image screen;
          Sdl_tools.wait_key();
          Sdl_tools.display other screen;
          Sdl_tools.wait_key();
          (*
          let scale = Resize.find_scale !new_image in
          Printf.printf "Scaling image by %f \n" scale; 
          new_image := Resize.resize_image !new_image scale; 
          Sdl_tools.display !new_image screen;
          Sdl_tools.wait_key();
          *)
        end;

     if (!use_adjust) then
        begin
          new_image := Adjust.adjust !new_image 30;
        
          let screen = Sdlvideo.set_video_mode
                    (Sdl_tools.get_width !new_image)
                    (Sdl_tools.get_height !new_image)
                    [`DOUBLEBUF] in

          Sdl_tools.display !new_image screen;
          Sdl_tools.wait_key();
        end;
     Printf.printf "Saving image...\n";
     end;

     at_exit Sdl.quit;;

let _= main();


