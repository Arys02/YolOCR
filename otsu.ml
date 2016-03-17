let get_histogram image =
  let (w,h) = Sdl_tools.get_size image in
  let histogram = Array.make 256 0 in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      let red = Sdl_tools.get_grey (Sdlvideo.get_pixel_color image i j) in
      histogram.(red) <- histogram.(red) + 1;
    done;
  done;
  histogram

let get_threshold img = 
        let (width,height) = Sdl_tools.get_size img in
        let sum = ref 0 and sumB = ref 0 in
        let wB = ref 0 and wF = ref 0 in
        let varMax = ref 0. and treshold = ref 0 in
        let hist = ref [||] in
        hist := get_histogram img;
        for i = 0 to 255 do 
                sum := !sum + i * !hist.(i);
        done;
        let break = ref false in
        for i = 0 to 255 do
                if (not !break) then
                begin
                        wB := !wB + !hist.(i);
                        if (!wB <> 0) then
                        begin
                                wF := (width*height) - !wB;
                                if (!wF = 0) then
                                begin
                                        break := true;
                                end
                                else
                                begin
                                        sumB := !sumB + (i * !hist.(i));
                                   let mB = (float)(!sumB/(!wB)) and mF = (float)((!sum-(!sumB))/(!wF)) in
                                   let varB = (float) !wB *.(float) !wF *.(mB-.mF) *.(mB-.mF) in
                                   treshold := if varB > !varMax then i else !treshold;
                                   varMax := if varB > !varMax then varB else !varMax;
                                end;
                        end;
                end;
        done;
    !treshold

let binarize src dst =
  let threshold = get_threshold src in
  let (w,h) = Sdl_tools.get_size src in
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
        if (Sdl_tools.get_grey (Sdlvideo.get_pixel_color src x y) < threshold) then
          Sdlvideo.put_pixel_color dst x y (0,0,0)
        else
          Sdlvideo.put_pixel_color dst x y (255,255,255)
        done
      done

