(* This program aims to display fractales *)
(* #require "camlimages.png";; *)

type complex_number = {x: float; y:float};;

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

let get_rgba ?a:(a=255) r g b = { Color.color = {Color.r = r; Color.g = g ; Color.b = b} ; alpha = a} ;;

(* Mode de couleur *)
let get_color_from_int max min n =
  get_rgba 255 ((n-min)*255/(1 + max - min)) 0
;;

(* Moteur *)

let get_empty_image ?color:(color = get_rgba 0 0 0) x y = (Rgba32.make x y color );;

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
  let (x,y) = (img.Rgba32.width, img.Rgba32.height) in
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
  Printf.printf "Conversion...";
  for i = 0 to x - 1
  do
    for j = 0 to y - 1
    do
      (* Printf.printf "max : %d; min : %d; nb : %d\n" !max !min int_img.(i).(j); *)
      let color = get_color_from_int !max !min int_img.(i).(j) in
      (* Printf.printf "color : %d %d %d\n" color.Color.color.Color.r color.Color.color.Color.g color.Color.color.Color.b; *)
      Rgba32.set img i j ( get_color_from_int !max !min int_img.(i).(j) )
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
  save_image g_filename (Images.Rgba32 img);
;;


fractal_coord (-1.5, -1.) (1.2,1.) 1000;;

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
