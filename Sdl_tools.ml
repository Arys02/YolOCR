let get_size src =
    ((Sdlvideo.surface_info src).Sdlvideo.w,   
    (Sdlvideo.surface_info src).Sdlvideo.h);;

let sdl_init () =
         begin
            Sdl.init [`EVERYTHING];
            Sdlttf.init ();
            Sdlevent.enable_events Sdlevent.all_events_mask;
        end;;
                                   
let rec wait_key () =
    let e = Sdlevent.wait_event () in
    match e with
         Sdlevent.KEYDOWN _ -> ()
        | _ -> wait_key ();;   

(* display image on dst surface *)
let display image dst =
  Sdlvideo.blit_surface ~src:image ~dst:dst ();
  Sdlvideo.update_rect dst

(* get image width in pixels *)
let get_width image = (Sdlvideo.surface_info image).Sdlvideo.w

(* get image height in pixels *)
let get_height image = (Sdlvideo.surface_info image).Sdlvideo.h    

let get_nbpixel image = (get_width image) * (get_height image)

let get_grey = function  
    (grey,_,_) -> grey

let get_oneColor src x y = get_grey(Sdlvideo.get_pixel_color src x y)

let blank_type = Sdlloader.load_image "images/blank.jpg"

let blank_surface img w h = Sdlvideo.create_RGB_surface_format blank_type [] w h

