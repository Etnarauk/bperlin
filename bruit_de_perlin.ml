(* GENERATION ALEATOIRE DE TERRAIN EN UTILISANT L'ALGORITHME DE PERLIN *)

Random.self_init();;
let cree_tab() =
let a = Array.make_matrix 100 100 0 in a
;;

let valeurs_aleatoires tableau frequence hauteur_max =
  let n = Array.length tableau in
    for i = 0 to (n - 1)/frequence do
      for j = 0 to (n - 1)/frequence do
        tableau.(i*frequence).(j*frequence) <- Random.int hauteur_max
      done;
    done;
  for i = 0 to (n - 1)/frequence do
    tableau.(i*frequence).(n - 1) <- Random.int hauteur_max;
    tableau.(n - 1).(i*frequence) <- Random.int hauteur_max;
  done;
  tableau.(n - 1).(n - 1) <- Random.int hauteur_max;
;;

(* PREMIÈRE APPROCHE *)

let calcul_hauteur1 x a b hauteur_A hauteur_B =
  if a = b then hauteur_A else
  ((x - a)*hauteur_B + (b - x)*hauteur_A)/(b - a)
;;

let max_grille i frequence n=
  if i > n - frequence - 1 then n - 1 else
    ((i/frequence) + 1) * frequence
;;

let hauteurs_terrain tableau frequence calcul_hauteur =
  let n = Array.length tableau in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let x_min_calque = (i/frequence) * frequence in
      let x_max_calque = max_grille i frequence n in
      let y_min_calque = (j/frequence) * frequence in
      let y_max_calque = max_grille j frequence n in
      let hauteur00 = tableau.(x_min_calque).(y_min_calque) in
      let hauteur10 = tableau.(x_max_calque).(y_min_calque) in
      let hauteur01 = tableau.(x_min_calque).(y_max_calque) in
      let hauteur11 = tableau.(x_max_calque).(y_max_calque) in
      let hauteur_1 = calcul_hauteur i x_min_calque x_max_calque hauteur00 hauteur10 in
      let hauteur_2 = calcul_hauteur i x_min_calque x_max_calque hauteur01 hauteur11 in
      let hauteur_3 = calcul_hauteur j y_min_calque y_max_calque hauteur_1 hauteur_2 in
      tableau.(i).(j) <- hauteur_3
    done;
  done;
;;

let print_valeurs_int tableau =
  let n = Array.length tableau in
  let m = Array.length tableau.(0) in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      (
      print_int tableau.(i).(j); print_string ".0";
      print_endline "")
    done;
  done;
;;

(* let tab = Array.make_matrix 100 100 0 in
valeurs_aleatoires tab 9 50;
hauteurs_terrain tab 9 calcul_hauteur1;
print_valeurs_int tab;; *)

let genere1() =
  let tableau = Array.make_matrix 3601 3601 0 in
  valeurs_aleatoires tableau 100 50;
  hauteurs_terrain tableau 100 calcul_hauteur1;
  tableau
;;

let print_valeurs_float_float tableau =
  let n = Array.length tableau in
  let m = Array.length tableau.(0) in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      (let x, y = tableau.(i).(j) in
      (print_int i ;
      print_string ", ";
      print_int j;
      print_string ", (";
      print_float x;
      print_string ", ";
      print_float y;
      print_string ")";
      print_endline ""))
    done;
  done;
;;

(* DEUXIÈME APPROCHE *)

(* Définition des types *)

type perlin_noise = {
  grid: (float * float) array array;
  width: int;
  height: int;
}

(* Fonctions auxiliaires *)

(* Fonction de génération de gradient aléatoire *)
let gradient() : float * float =
  let angle = Random.float (2.0 *. Float.pi) in
  (cos angle, sin angle)
;;

(* Fonction pour calculer la norme d'un vecteur 3D *)
let calcul_norme (x, y, z) =
  (x*.x +. y*.y +. z*.z)**0.5
;;

let rec vecteur_aleatoire_sphere_unite() =
let a = 1. -. Random.float 2. in let b = 1. -. Random.float 2. in let c = 1. -. Random.float 2. in
let norme = calcul_norme (a, b, c) in if norme <= 1. then (a/.norme, b/.norme, c/.norme) else vecteur_aleatoire_sphere_unite()
;;

let initialise_grille n m k=
  let tab = Array.make_matrix ((n/k) + 1) ((m/k) + 1) (0., 0.) in
  for i = 0 to n/k do
    for j = 0 to m/k do
      let a = Random.float (2.0 *. Float.pi) in
      tab.(i).(j) <- (cos a, sin a)
    done
  done;
  tab
;;

let produit_scalaire_3d (x1, x2, x3) (y1, y2, y3) =
  x1*.y1 +. x2*.y2 +. x3*.y3
;;

let produit_scalaire (x1, x2) (y1, y2) =
  x1*.y1 +. x2*.y2
;;

  let interpole x y t=
    x +. t*.(y-.x)
;;

let s_courbe0 x =
  x
;;

let s_courbe1 x =
  (3.0*.(x**2.) -. 2.0*.(x ** 3.))
;;

let bruit_de_perlin2 tab_bruit n m x y k s_courbe =
  let x_grille = if x / k > n / k - 1 then n/k - 2 else x / k in
  let y_grille = if y / k > m / k - 1 then m/k - 2 else y/k in
  let d_x = float_of_int (x mod k) /. float_of_int k in
  let d_y = float_of_int (y mod k) /. float_of_int k in
  let p00 = tab_bruit.(x_grille).(y_grille) in
  let p01 = tab_bruit.(x_grille + 1).(y_grille) in
  let p10 = tab_bruit.(x_grille).(y_grille + 1) in
  let p11 = tab_bruit.(x_grille + 1).(y_grille + 1) in
  let s = produit_scalaire p00 (d_x, d_y) in
  let t = produit_scalaire p01 (d_x -. 1.0, d_y) in
  let u = produit_scalaire p10 (d_x, d_y -. 1.0) in
  let v = produit_scalaire p11 (d_x -. 1.0, d_y -. 1.0) in
  let sx = s_courbe d_x in
  let sy = s_courbe d_y in
  let a = interpole s t sx in
  let b = interpole u v sx in
  interpole a b sy
;;

  let genere_map n m k s_courbe hauteur_max =
    let bruit = initialise_grille n m k in
    let hauteurs = Array.make_matrix n m 0 in
    for i = 0 to n - k - 1 do
      for j = 0 to m - k - 1 do
        let perlin = bruit_de_perlin2 bruit n m i j k s_courbe in
        let hauteur = int_of_float ((perlin +. 1.0) /. 2.0 *. float_of_int hauteur_max) in
        hauteurs.(i).(j) <- hauteur
      done;
    done;
    hauteurs
  ;;

(* Fonction pour lire un fichier .hgt et retourner une matrice d'entiers *)
let lit_hgt nom =
  let ic = open_in_bin nom in
  let size = in_channel_length ic in
  let n = int_of_float (sqrt (float_of_int size /. 2.)) in
  let data = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let high_byte = input_byte ic in
      let low_byte = input_byte ic in
      let value = (high_byte lsl 8) lor low_byte in
      data.(i).(j) <- if value = 0x8000 then -32768 else value
    done
  done;
  close_in ic;
  data
;;

let moyenne matrice =
  let n = Array.length matrice in
  let m = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      m := !m + matrice.(i).(j);
    done;
  done;
  !m/(n * n)
;;

let generate_code matrix filename =
  let oc = open_out filename in
  let n = Array.length matrix in
  let m = moyenne matrix in
  Printf.fprintf oc "";
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      Printf.fprintf oc "%d.0" ((matrix.(i).(j) - m)/50); Printf.fprintf oc "\n";
    done;
  done;
;;

(* Fonction pour lire tous les fichiers .hgt dans un répertoire et les traiter *)
let process_hgt_files dir i nom_fichier =
  let files = Sys.readdir dir in
  let tab1 = files.(i) in
  if Filename.check_suffix tab1 ".hgt" then
  let filepath = Filename.concat dir tab1 in
  let height_matrix = lit_hgt filepath in
  generate_code height_matrix nom_fichier
;;

let random_mat n =
  let t = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      t.(i).(j) <- Random.int 50
    done;
  done;
  t
;;

let distance1 matrice1 matrice2 =
  let d = ref 0 in
  let n = Array.length matrice1 in
  let m = Array.length matrice1.(0) in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      d := !d + (abs (matrice1.(i).(j) - matrice2.(i).(j)))
    done;
  done;
  float_of_int !d
;;

let mesures_carte_knn tableau map fonction_d_evaluation k valide =
  let t = Array.length tableau in
  let plus_proches = Array.make k 0 in
  let distances = Array.make t 0. in
  let indice = ref 0 in
  for j = 0 to t - 1 do
    distances.(j) <- (fonction_d_evaluation tableau.(j) map) ;
    if distances.(j) < distances.(!indice) then indice := j
  done;
  plus_proches.(0) <- !indice;
  distances.(!indice) <- max_float;
  for i = 1 to k - 1 do
  indice := 0;
    for j = 0 to t - 1 do
      if distances.(j) < distances.(!indice) then indice := j
    done;
      plus_proches.(i) <- !indice;
      distances.(!indice) <- max_float;
  done;
  let compte = ref 0 in
  for i = 0 to k - 1 do
    if valide plus_proches.(i) then compte := !compte + 1;
  done;
  if !compte < k/2 then false else true
;;