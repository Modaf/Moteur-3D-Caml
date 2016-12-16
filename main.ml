#open "graphics";;
let coord f = match f with |(a, b, c) -> (a, c);;
let draw a b c d = fill_poly[|coord a; coord b; coord c; coord d|];;
let c1 f = match f with |(a, b, c) -> a;;
let c2 f = match f with |(a, b, c) -> b;;
let c3 f = match f with |(a, b, c) -> c;;
let soustraction e f = match (e, f) with |((a, b, c), (aa, bb, cc)) -> (a-aa, b-bb, c-cc);;
let prefix -! e f = soustraction e f;;
let multiplication scal f = match f with |(a, b, c) -> (((float_of_int a) *. scal), ((float_of_int b) *. scal), ((float_of_int c) *. scal));;
let mfloat scal f = match f with |(a, b, c) -> ((a *. scal), (b *. scal), (c *. scal));;
let prefix *! scal f = multiplication scal f;;
let i_o_f f = match f with |(a, b, c) -> (int_of_float a, int_of_float b, int_of_float c);;





(*Fonctions graphiques :*)


let intersection oeil a b c d point =
	let coeff = point -! oeil in
	(*la droite est oeil + t * coeff*)
	(*Le plan est ax + by + cz + d = 0*)

	(*Resolution : on a
		ax + by + cz + d = 0
		x = x1 + x2*t y = ect ...

		donc : on a a(x1 + t*x2) + b(y1 ..) + d = 0
		d'où ax1 + by1 + cz1 +d = t(-ax2 - by2 - cz2)
		la sol t0 est t0 = (ax1 + by1 + cz1 + d) / (-ax2 - by2 - cz2)
		et le point voulu est x = x1 + t0 * x2, y = ect..
	*)
	let x1 = c1 oeil in
	let y1 = c2 oeil in
	let z1 = c3 oeil in
	let x2 = c1 coeff in
	let y2 = c2 coeff in
	let z2 = c3 coeff in
	let tFinal = (-1.) *. ((float_of_int (a*x1 + b*y1 + c*z1 + d)) /. (float_of_int (a*x2 + b*y2 + c*z2))) in
	let aFinal = x1 + int_of_float (tFinal *. (float_of_int (x2))) in
	let bFinal = y1 + int_of_float (tFinal *. (float_of_int (y2))) in
	let cFinal = z1 + int_of_float (tFinal *. (float_of_int (z2))) in
	(aFinal, bFinal, cFinal);;

let projectionAffichage v oeil a b c d =
	let n = vect_length v in
	let res = make_vect n (0, 0) in
	for i=0 to n-1 do
		res.(i) <- coord (intersection oeil a b c d v.(i))
	done;
	res;;

let projection v oeil a b c d =
	let n = vect_length v in
	let res = make_vect n (0, 0) in
	for i=0 to n-1 do
		res.(i) <- (intersection oeil a b c d v.(i)) (*La diff est ici : ya pas le coord*)
	done;
	res;;

(*On a une liste des trucs à dessiner qui constitue toute la scène : certains en recouvrent d'autre suivant le y de projection*)
(*Certains poly peuvent se couper*)
(*On suppose qu'on à affaire qu'à des parallélogrammes*)
(*On veut une fonction auxiliaire qui à partir de deux para renvoit l'intersection visibles des deux*)

let intersectionDeuxPara l1 l2 = (*Renvoit une liste de coord à partir de deux de ces listes*)
	(*On ne gère pas le cas si un des deux para est inclus dans l'autre*)
	(*Pour chaque couple (i, i+1) de l2 on regarde si c'est dans un des coupes de l1*)
	let auxInterParaSegment para p1 p2 = 
		let a = c1 p1 - c1 p2 and b = c2 p1 - c2 p2 and c = c3 p1 - c3 p2 in
		(*Les points du segments sont t*(a, b, c) + (p2) avec t dans [0, 1]*)
		(*On veut aussi une équation du parallélogramme*)
		(*C'est un vect des deux segments h1 h2 et h1 h3 si h1 est le premier de la liste*)
		let h1 = hd para and h2 = hd (hd para) and h3 = hd (hd (hd para)) in
		let eq1 = h1 -! h2 and eq2 = h1 -! h3 in
		(*Le para est donc (t1*eq1 + h2) + (t2*eq2 + h3) avec t1 t2 dans [0, 1]*)
		(*Existe il alors t t1 t2 tel que l'on ait :  t*(a, b, c) + (p2) = (t1*eq1 + h2) + (t2*eq2 + h3) avec tous dans [0, 1]*)
		(*On résout le système : on regarde si c'est dans [0, 1] : si c'est le cas ya collision; sinon non*)
		
		
	let auxInterParaListeSegment para listeSeg premierPoint = match listeSeg with
		|[] -> [] |[h] -> 
		|h::q -> 

;;let rec dessin l oeil a b c d = let rec aux l = match l with
	|[] -> ()
	|h::q -> fill_poly(projectionAffichage h oeil a b c d); aux q in match l with
	|[] -> ()
	|h::q -> set_color (snd h); aux (fst h); dessin q oeil a b c d;;

let cube x y z coter = (*x, y, z = en bas à gauche du cube*)
	let devant = [|(x, y, z); (x+coter, y, z); (x+coter, y, z+coter); (x, y, z+coter)|] in
	let derriere = [|(x, y+coter, z); (x+coter, y+coter, z); (x+coter, y+coter, z+coter); (x, y+coter, z+coter)|] in
	let gauche = [|(x, y, z); (x, y, z+coter); (x, y+coter, z+coter); (x, y+coter, z)|] in
	let droite = [|(x+coter, y, z); (x+coter, y, z+coter); (x+coter, y+coter, z+coter); (x+coter, y+coter, z)|] in
	let sol = [|(x, y, z); (x+coter, y, z); (x+coter, y+coter, z); (x, y+coter, z)|] in
	let plafond = [|(x, y, z+coter); (x+coter, y, z+coter); (x+coter, y+coter, z+coter); (x, y+coter, z+coter)|] in
	[devant; gauche; droite; sol; plafond; derriere];;

let clear x = set_color white; fill_poly [|0, 0; 1000, 0; 1000, 1000; 0, 1000|];;

let rekt x y tx ty color = set_color color; fill_poly [|x, y; x+tx, y; x+tx, y+ty; x, y+ty|];; 

let rec creationCouloir epaisseurCouloir epaisseurCube nombreCubeHauteur nombreCubeLongueur x y z couleur = (*Renvoit une liste de (cube x y z coter, couleur)*)
	match nombreCubeLongueur with |0 -> [] |n -> 
		let tmp r =
		let res = ref r in
		for i=0 to nombreCubeHauteur do
			res := !res @ [cube x y (z+epaisseurCube*i) epaisseurCube, couleur; cube (x+epaisseurCube+epaisseurCouloir) y (z+epaisseurCube*i) epaisseurCube, couleur]
		done; !res; in
		(tmp (creationCouloir epaisseurCouloir epaisseurCube nombreCubeHauteur (nombreCubeLongueur-1) x (y+epaisseurCube) z couleur));;


(*
Touches utiles :
z q s d --> Se deplacer
i k --> Avancer/Reculer
o l --> Tourner la tête droite/gauche
m p --> Tourner la tête haut/bas
n --> Sauter
*)
let game entities =
	(*Fonctions de mises à jour des variables par les inputs*)
	let actx x i = if (i=`q`) then x-2 else if (i=`d`) then x+2 else x in
	let acty y i = if (i=`k`) then y-2 else if (i=`i`) then y+2 else y in
	let actz z i = if (i=`s`) then z-.2. else if (i=`z`) then z+.2. else z in
	let actr1 r i = if (i=`l`) then r-.0.01 else if (i=`o`) then r+.0.01 else r in
	let actr2 r i = if (i=`m`) then r-.0.01 else if (i=`p`) then r+.0.01 else r in
	let actenSaut r i = if (i=`n`) then 1 else r in
	
	
	(*Initialisation des variables*)
	let x = ref 100 in
	let y = ref 50 in
	let z = ref 100. in
	let r1 = ref 0. in
	let r2 = ref 0. in
	let enSaut = ref 0 in
	let pourcentageSaut = ref 0 in (* int : de 0 à duréeSaut*)
	let dureeSaut = 50 in (*Le nombre de gameLoop*)
	let graviter = 0.1 in
	let dir = (0., 1., 0.) in (*unitaire => float*)
	
	
	(*Game loop*)
	for i=0 to 100000 do
		
		
		(*Mise à jour des variables*)
		let input = (wait_next_event [Key_pressed]).key in
		x := actx !x input;
		y := acty !y input;
		z := actz !z input;
		r1 := actr1 !r1 input;
		r2 := actr2 !r2 input;
		enSaut := actenSaut !enSaut input;
		
		
		(*Calcul necessaire pour l'affichage*)
		let dir = mfloat (sqrt (1./.(1.+.((sin !r1) *. (sin !r1) *. (sin !r2) *. (sin !r2))))) (sin !r1, (cos !r1) *. (cos !r2), sin !r2) in
		(* unitaire car sin1² + sin2² + cos1² cos2² = s1²+s2² + (1-s1²)(1-s2²) = 1 + s1²s2²*)
		clear 2;
		if (!enSaut = 1) then begin (*On décremente de g de plus en plus, à chaque s l'acc est g donc la vitesse est gt et la pos est gt2*)
			z := !z -. (graviter *. (float_of_int (dureeSaut/2 - !pourcentageSaut)));
			pourcentageSaut := !pourcentageSaut + 1;
			if (!pourcentageSaut = dureeSaut) then begin pourcentageSaut := 0;	enSaut := 0; end
		end;
		
		
		(*Affichage des objets*)
		let prec = 1000. in (*la precision dans les virgules car faut des int*)
		dessin entities (!x, !y, int_of_float !z) (int_of_float(prec*.(c1 dir))) (int_of_float(prec*.(c2 dir))) (int_of_float(prec*.(c3 dir))) (int_of_float(prec*.1.));
		
		
		(*Affichage des barres de controlle*)
		rekt 300 350 (!x/5) 10 blue;
		rekt 300 335 (!y/5) 10 blue;
		rekt 300 320 ((int_of_float !z)/5) 10 blue;
		rekt 300 305 (int_of_float (!r1 *. 20.)) 10 blue;
		rekt 300 290 (int_of_float (!r2 *. 20.)) 10 blue;
		rekt 300 275 !pourcentageSaut 10 blue;
	done;;



open_graph "";;
(*creationCouloir epaisseurCouloir epaisseurCube nombreCubeHauteur nombreCubeLongueur x y z couleur*)
let trucADraw = [(cube 100 100 100 50), black; (cube 150 150 150 25), red];;
let trucADraw = (creationCouloir 100 50 2 7 (-200) 100 100 black) @ (creationCouloir 100 50 2 7 000 100 100 red) @ (creationCouloir 100 50 2 7 200 100 100 blue);;
game trucADraw;;
(*
x : de gauche à droite
y : profondeur
z : hauteur
*)

(*LISTE FONCTIONS GRAPHIQUES DISPONIBLES*)

(*
Renvoit le point d'intersection entre le plan ax+by+cz+d=0 et la droite qui passe par oeil et point
intersection oeil a b c d point : int * int * int -> int -> int -> int -> int -> int * int * int -> int * int * int

Calcule l'intersection d'une liste de points
projectionAffichage v oeil a b c d : (int * int * int) vect -> int * int * int -> int -> int -> int -> int -> (int * int) vect

Dessine une liste (couleur, liste) avec l'intersection de oeil et du plan ax6by+cz+d=0
dessin l oeil a b c d : ((int * int * int) vect list * color) list -> int * int * int -> int -> int -> int -> int -> unit

Renvoit la liste des coordonées pour un cube d'origine en bas à gauche x y z
cube x y z coter : int -> int -> int -> int -> (int * int * int) vect list

Remplit un rectangle de coord x y en bas à gauche
rekt x y tx ty color : int -> int -> int -> int -> color -> unit

Permet de visualiser des objets
game entities : ((int * int * int) vect list * color) list -> unit

Permet de creer un couloir à partir de cube : creationCouloir epaisseurCouloir epaisseurCube nombreCubeHauteur nombreCubeLongueur x y z couleur
creationCouloir : int -> int -> int -> int -> int -> int -> int -> 'a -> ((int * int * int) vect list * 'a) list
*)






