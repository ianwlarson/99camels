#!/usr/bin/env ocaml

(* Problem 1 *)
let rec last = function
	| [] -> None
	| [x] -> Some x
	| _ :: xs -> last xs;;


assert (last [1;2;3;4;5;6] = Some 6);;
assert (last [] = None);;

(* Problem 2 *)
let rec last_two = function
	| [] -> None
	| [_] -> None
	| x :: [y] -> Some (x, y)
	| _ :: xs -> last_two xs;;
	
	
assert (last_two [1;2;3;4;5] = Some (4, 5));;
assert (last_two [] = None);;
assert (last_two [1] = None);;

(* Problem 3 *)
let rec at k = function
	| [] -> None
	| h :: t -> if k = 0 then Some h else at (k-1) t;;
	
assert (at 3 [1;2;3;4;5;6] = Some 4);;
assert (at 0 [1;2;3;4] = Some 1);;
assert (at 7 [1;2] = None);;
assert (at 3 [] = None);;

(* Problem 4 *)
let length l =
	let rec _length acc = function
		| [] -> acc
		| _ :: xs -> _length (acc+1) xs in
	_length 0 l;;
	
assert (length [1;2;3;4] = 4);;
assert (length [] = 0);;

(* Problem 5 *)

let rev list =
	let rec _rev acc = function
		| [] -> acc
		| x :: xs -> _rev (x :: acc) xs in
		_rev [] list;;
	
assert (rev [1;2;3] = [3;2;1]);;
assert (rev [] = []);;

(* Problem 6 *)

let is_palindrome l =
	l = rev l;;
	
assert (is_palindrome ['x';'a';'m';'a';'x']);;

(* Problem 7 *)
type 'a node =
	| One of 'a 
	| Many of 'a node list;;


let flatten l =
	let rec _flatten acc = function
		| [] -> acc
		| One x :: xs -> _flatten (x :: acc) xs
		| Many x :: xs -> _flatten (_flatten acc x) xs in
	rev (_flatten [] l);;
		
assert (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a";"b";"c";"d";"e"]);;

(* Problem 8 *)
let rec compress = function
	| a :: ( b :: _ as t) -> if a = b then compress t else a :: compress t
	| smaller -> smaller;;
	

assert (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a";"b";"c";"a";"d";"e"]);;

(* Problem 9 *)

let pack list = 
	let rec _pack curr acc = function
		| [] -> []
		| [x] -> (x :: curr) :: acc
		| a :: (b :: _ as t) ->
			if a = b then
				_pack (a :: curr) acc t
			else _pack [] ((a :: curr) :: acc) t
	in rev (_pack [] [] list);;
	
assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];["e"; "e"; "e"; "e"]]);;

(* Problem 10 *)

let encode list =
	let rec _encode count acc = function
		| [] -> []
		| [x] -> (count+1, x) :: acc
		| a :: (b :: _ as t) ->
			if a = b then
				_encode (count+1) acc t
			else
				_encode 0 ((count+1, a) :: acc) t
	in rev (_encode 0 [] list);;
	
assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;

(* Problem 11 *)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;
    
let encode list =
	let rle_encode count elem = 
		if count = 1 then
			One elem
		else
			Many (count, elem)
	in
	let rec _encode count acc = function
		| [] -> []
		| [x] -> (rle_encode (count + 1) x) :: acc
		| a :: (b :: _ as t) ->
			if a = b then
				_encode (count+1) acc t
			else
				_encode 0 ((rle_encode (count + 1) a) :: acc) t
	in rev (_encode 0 [] list);;
	
assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);;

(* Problem 12 *)

let rec many acc n x =
	if n = 0 then
		acc
	else
		many (x :: acc) (n - 1) x

let decode list =
	let rec _decode acc = function
		| [] -> acc
		| Many (x, y) :: xs -> _decode (many acc x y) xs
		| One x :: xs -> _decode (many acc 1 x) xs
	
	in rev (_decode [] list);;

assert (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);;

(* Problem 13 *)

let encode list =
	let rec rle_encode count var = if count = 0 then One var else Many (count + 1, var) in
	
	let rec _encode count acc = function
		| [] -> []
		| [x] -> rle_encode count x :: acc
		| a :: (b :: _ as t) -> 
			if a = b then
				_encode (count + 1) acc t
			else
				_encode 0 (rle_encode count a :: acc) t in
				
	rev (_encode 0 [] list);;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);;