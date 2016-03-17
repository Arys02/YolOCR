let color pixel = match pixel with
    |(255,255,255) -> 0
    |_ -> 1

let int_to_rgb i = match i with
    |1 -> (0,0,0)
    |_ -> (255,255,255)

let invert pixel = match pixel with
    |(0,0,0) -> (255,255,255)
    |_ -> (0,0,0)

(* invert all pixels of an image *)
let invert_image image =
    let (w,h) = Sdl_tools.get_size image in
    let newimg = Sdl_tools.blank_surface image w h in
      for i = 0 to (w-1) do
        for j = 0 to (h-1) do
          Sdlvideo.put_pixel_color newimg i j
          (invert (Sdlvideo.get_pixel_color image i j));
        done;
      done;
      newimg
