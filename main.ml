(* This program aims to display fractales *)
(* #require "camlimages.png";; *)

type complex_number = {x: float; y:float};;

let rgba_mode = false;;

let g_size_x = 2000;;
let g_size_y = 2000;;
let g_filename = "out.png";;
let g_max_range = 200;;
(* Float handle *)
let zero = 0.0;;
let un = 1.0;;
let deux = 2.0;;
let fplus = ( +. );;
let fopposed = ( ~-. );;
let fminus = ( -. );;
let ftime = ( *. );;
let fdiv = ( /. );;
let fsqrt = sqrt ;;
let fpuiss = ( ** );;

(*
let zero = Num.num_of_int 0;;
let fplus = Num.add_num;;
let fopposed = Num.minus_num;;
let fminus = Num.sub_num;;
let fmult = Num.mult_num;;
let fsqrt = Num.square_num;;
let fdiv = Num.div;;
*)

(* Complex number handle *)
let czero = { x = zero; y = zero} ;;
let cplus a b = {x = fplus a.x b.x; y = fplus a.y b.y};;
let cminus a b = {x = fminus a.x b.x; y = fminus a.y b.y};;
let ctime a b = { x = (fminus (ftime a.x b.x) (ftime a.y b.y) );
		  y = (fplus (ftime a.y b.x) (ftime a.x b.y) ) };;
let cdiv a b = { x = fdiv ( fplus (ftime a.x b.x) (ftime a.y b.y) ) (fplus (ftime b.x b.x) (ftime b.y b.y) );
		 y = fdiv (fminus (ftime a.y b.x) (ftime a.x b.y)) (fplus (ftime b.x b.x) (ftime b.y b.y))};;
let ccarre a = { x = (fminus (ftime a.x a.x) (ftime a.y a.y));
		 y = (ftime 2.0 (ftime a.x a.y) )};;
let cabscarre a = (fplus (ftime a.x a.x ) (ftime a.y a.y));;

(* Diverses fonctions *)

type color = Color.rgba;;


(* Comment/uncomment the good rgb mode if you want or not rgba *)
(* let get_rgba ?a:(a=255) r g b = { Color.color = {Color.r = r; Color.g = g ; Color.b = b} ; alpha = a} ;; *)

let get_rgba r g b = {Color.r = r; Color.g = g ; Color.b = b};;

(* Mode de couleur *)
let get_color_from_int max min n =
  get_rgba 255 ((n-min)*255/(1 + max - min)) 0
;;

(* Moteur *)

(* Change this one too if you want or not rgba *)
(* let get_empty_image ?color:(color = get_rgba 0 0 0) x y = (Rgba32.make x y color );; *)

let get_empty_image ?color:(color = get_rgba 0 0 0) x y = (Rgb24.make x y color );;


let save_image filename img = Png.save filename [] img;;

let compute_int_mandelbrot x y =
  let z = ref czero in
  let k = ref 0 in
  let c = {x = x; y = y} in
  while !k < g_max_range && cabscarre (!z) < 4.
  do
    z := (cdiv {x = 1.2 ; y = 0.} (cplus (ctime !z !z) (c)));
    incr k;
  done;
  (* Printf.printf "%d\n" !k; *)
  !k
;;

let fill_img img x0 y0 dx dy compute_int =
  (* And this one for rgba *)
  let (x,y) = (img.Rgb24.width, img.Rgb24.height) in
  (* let (x,y) = (img.Rgba32.width, img.Rgba32.height) in *)
  let int_img = Array.make_matrix x y 0 in
  (* Dans un premier temps on rempli un tableau avec des entiers *)
  let max = ref 0 in
  let min = ref max_int in
  for i = 0 to x - 1
  do
    for j = 0 to y - 1
    do
      let value = (compute_int (x0 +. (float_of_int i) *. dx) (y0 +. (float_of_int j) *. dy)) in
      int_img.(i).(j) <- value;

      if value > !max then max := value
      else if value < !min then min := value;
      
    done;
  done;
  (* On convertit ensuite ce tableau en couleur *)
  (* Printf.printf "Conversion..."; *)
  for i = 0 to x - 1
  do
    for j = 0 to y - 1
    do
      (* And this one... for rgba *)
      Rgb24.set img i j ( get_color_from_int !max !min int_img.(i).(j) );
      (* Rgba32.set img i j ( get_color_from_int !max !min int_img.(i).(j) ) *)
    done;
  done;
;;

let fractal_coord (x1,y1) (x2,y2) ?size_y:(size_y = -1) size_x =
  let size_y = if size_y <> -1 then size_y
    else (int_of_float ((float_of_int size_x) *. (y2 -. y1) /. (x2 -. x1))) in
  let img = get_empty_image size_x size_y in
  let dx = (x2 -. x1) /. (float_of_int size_x) in
  let dy = (y2 -. y1) /. (float_of_int size_y) in
  fill_img img x1 y1 dx dy compute_int_mandelbrot;
  img;
;;

let save_frac img =
  (* And this one for rgba *)
  (* save_image g_filename (Images.Rgba32 img); *)
  save_image g_filename (Images.Rgb24 img);
;;


let display_frac (x1, y1) (x2, y2) res =
  let x1 = ref x1 in
  let x2 = ref x2 in
  let y1 = ref y1 in
  let y2 = ref y2 in
  let img = ref (fractal_coord (!x1, !y1) (!x2, !y2) res) in
  Printf.printf "Displaying";
  Graphics.open_graph "";
  (* And this one for rgba *)
  (* Graphic_image.draw_image (Images.Rgba32 img) 0 0; *)
  while true
  do
    Graphics.open_graph "";
    Graphic_image.draw_image (Images.Rgb24 !img) 0 0;
    Printf.printf "Rectangle; res x = (%f, %f) (%f, %f) %d\n" !x1 !y1 !x2 !y2 res;
    Graphics.draw_string (Printf.sprintf "Rectangle; res x = (%f, %f) (%f, %f) %d" !x1 !y1 !x2 !y2 res);
    let (size_x,size_y) = (!img.Rgb24.width, !img.Rgb24.height) in
    let status = Graphics.wait_next_event [Graphics.Button_down ; Graphics.Key_pressed] in
    let (px1,py1) = (status.Graphics.mouse_x, status.Graphics.mouse_y) in
    let status = Graphics.wait_next_event [Graphics.Button_down ; Graphics.Key_pressed] in
    let (px2,py2) = (status.Graphics.mouse_x, status.Graphics.mouse_y) in
    let dx = (!x2 -. !x1) /. (float_of_int size_x) in
    let dy = (!y2 -. !y1) /. (float_of_int size_y) in
    x1 := !x1 +. (float_of_int px1) /. (float_of_int size_x) *. (!x2 -. !x1);
    x2 := !x1 +. (float_of_int px2) /. (float_of_int size_x) *. (!x2 -. !x1);
    y1 := !y1 +. (1. -. (float_of_int py1) /. (float_of_int size_y)) *. (!y2 -. !y1);
    y2 := !y1 +. (1. -. (float_of_int py2) /. (float_of_int size_y)) *. (!y2 -. !y1);
    
    img := fractal_coord (!x1, !y1) (!x2, !y2) res;
    
  done;
;;


save_frac( fractal_coord (-1.5, -1.) (1.2,1.) 100);;

let f = fractal_coord (-1.5, -1.) (1.2,1.) 100;;
display_frac (-1.5, -1.) (1.2,1.) 500;;


(* let () = *)
  (* let img = get_empty_image g_size_x g_size_y in *)
  (* fill_img img (-0.2) (-0.5) 0.00001 0.00001 compute_int_mandelbrot; *)
  (* fill_img img (-1.401155) (0.) 0.00001 0.00001 compute_int_mandelbrot; *)
  (* fill_img img (0.) (0.) 0.00001 0.00001 compute_int_mandelbrot; *)

  (* Sympa *)
  (* fill_img img (-1.401155) (0.) 0.001 0.001 compute_int_mandelbrot; *)
  (* fill_img img (-1.401155) (-1.) 0.001 0.001 compute_int_mandelbrot; *)
  
  (* fill_img img (-1.401155) (0.) 0.1 0.1 compute_int_mandelbrot; *)
  (* fill_img img (-3.401155) (6.) 0.01 0.01 compute_int_mandelbrot; *)
  (* save_image g_filename (Images.Rgba32 img); *)
(* ;; *)
