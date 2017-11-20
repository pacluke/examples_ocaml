(*******************************************)
(**** 	Example 01: strongly typed		****)
(*******************************************)


let sum (num01: int) (num02: int) : int =
	num01 + num02
;;

Printf.printf "\nsum 10 10 = %d\n\n" (sum 10 10);;

(*
	l1(OCaml): Bop(Num(10), Sum, Num(10))
*)

(*******************************************)
(**** 	Example 02: polymorphism		****)
(*******************************************)


 let rec count_elements (ls) : int =
	(match ls with
		|[] -> 0
		|hd::tl ->  1 + count_elements tl
	)
;;

Printf.printf "\nint list lenght = %d\n\n " (count_elements [1; 2; 3; 4; 5]) ;;

(* Printf.printf "\nchar list lenght = %d\n\n " (count_elements ['a'; 'b';'c'; 'd'; 'e']);;
Printf.printf "\nfloat list lenght = %d\n\n " (count_elements [1.1; 1.2; 1.3; 1.4; 1.5]);;
Printf.printf "\nbool list lenght = %d\n\n " (count_elements [true; false; true; false; true]);; *)

(***************************************************)
(**** 	Example 03: new types and constructors  ****)
(***************************************************)

type power_moons = int;;

type purple_coins = int;; 

type kingdom = (power_moons * purple_coins);;

type smo_kingdoms =
	Cap 			of kingdom
	| Cascade 		of kingdom
	| Sand 			of kingdom
	| Lake 			of kingdom
	| Wooded 		of kingdom
;;

let cap_kingdom = Cap(31, 50);;
let cascade_kingdom = Cascade(40, 50);;
let sand_kingdom = Sand(89, 100);;
let lake_kingdom = Lake(42, 50);;
let wooded_kingdom = Wooded(74, 100);;

(********************************************************)
(**** 	Example 04: expressions and pattern matching ****)
(********************************************************)

let missed_coins (current_kingdom: smo_kingdoms) (current_coins: purple_coins): purple_coins = 
	match current_kingdom with
	| Cap(pm, pc) -> pc- current_coins
	| Cascade(pm, pc) -> pc - current_coins
	| Sand(pm, pc) -> pc - current_coins
	| Lake(pm, pc) -> pc - current_coins
	| Wooded(pm, pc) -> pc - current_coins
;;

let missed_moons (current_kingdom: smo_kingdoms) (current_moons: power_moons): power_moons = 
	match current_kingdom with
	| Cap(pm, pc) -> pm - current_moons
	| Cascade(pm, pc) -> pm - current_moons
	| Sand(pm, pc) -> pm - current_moons
	| Lake(pm, pc) -> pm - current_moons
	| Wooded(pm, pc) -> pm - current_moons
;;

Printf.printf "\n\nMissed %d moons\n" (missed_moons cap_kingdom 12);;
Printf.printf "\n\nMissed %d coins\n\n" (missed_coins cap_kingdom 37);;


(*******************************************)
(**** 	Example 05: lists 				****)
(*******************************************)

let five_kingdoms = [cap_kingdom; cascade_kingdom; sand_kingdom; lake_kingdom; wooded_kingdom];;

let rec total_missed_coins (kingdom_list: smo_kingdoms list) (current_total_coins: purple_coins): purple_coins = 
	match kingdom_list with
	| [] -> - current_total_coins
	| hd::tl -> (missed_coins hd 0) + (total_missed_coins tl current_total_coins)
;;

let rec total_missed_moons (kingdom_list: smo_kingdoms list) (current_total_moons: power_moons): power_moons = 
	match kingdom_list with
	| [] -> - current_total_moons
	| hd::tl -> (missed_moons hd 0) + (total_missed_moons tl current_total_moons)
;;

Printf.printf "\n\nTotal missed coins = %d\n\n" (total_missed_coins five_kingdoms 23);;
Printf.printf "\n\nTotal missed moons = %d\n\n" (total_missed_moons five_kingdoms 51);;

(*******************************************)
(**** 	Example 06: exceptions 			****)
(*******************************************)

exception Not_enough_purple_coins of purple_coins;;

let buy_item (item_price: purple_coins)(current_coins: purple_coins): purple_coins =
	let new_current = current_coins - item_price in
		(if new_current >= 0
			then new_current
			else raise (Not_enough_purple_coins current_coins))
;;

let coins = 15;;

let purchase01 = buy_item 15 coins;;

(* let purchase02 = buy_item 20 coins;; *)

(* 	try
		buy_item 20 coins
	with
		| (Not_enough_purple_coins coins) -> coins
	;;
*)


(*
Printf.printf "\n\nAfter purchase01: %d coins\n\n" purchase01;;
Printf.printf "\n\nAfter purchase02: %d coins\n\n" purchase02;; 
*)

(*******************************************)
(**** 	Example 07: printing info		****)
(*******************************************)

let print_kingdom_info (smo_k: smo_kingdoms): unit =
	 match smo_k with
	| Cap(pm, pc) -> Printf.printf "\n\nThe Cap Kingdom has %d Power Moons and %d Purple Coins\n\n" pm pc
	| Cascade(pm, pc) -> Printf.printf "\n\nThe Cascade Kingdom has %d Power Moons and %d Purple Coins\n\n" pm pc
	| Sand(pm, pc) -> Printf.printf "\n\nThe Sand Kingdom has %d Power Moons and %d Purple Coins\n\n" pm pc
	| Lake(pm, pc) -> Printf.printf "\n\nThe LAke Kingdom has %d Power Moons and %d Purple Coins\n\n" pm pc
	| Wooded(pm, pc) -> Printf.printf "\n\nThe Wooded Kingdom has %d Power Moons and %d Purple Coins\n\n" pm pc
;; 

print_kingdom_info cap_kingdom;;
print_kingdom_info cascade_kingdom;;
print_kingdom_info sand_kingdom;;
print_kingdom_info lake_kingdom;;
print_kingdom_info wooded_kingdom;;













