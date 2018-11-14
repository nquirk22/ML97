(* 
** Nathan Quirk
** CSC 345
** ML Homework
** Submitted 11.15.18
*)


(*********** FLIP *********)
(*
 
Flips alternate elements of any list 

*)

fun flip (nil)      = nil
  | flip ([x])      = [x]
  | flip (x::y::zs) = y::x::flip(zs);


(********* DELETEITH *********)
(* 

Returns a list with the ith memeber removed. 
If i is greater than the length of the list,
    the list is returned unchanged.

*)

fun deleteIth (x::xs, 1) = xs
  | deleteIth (x::xs, i) = x::deleteIth (xs, i - 1)
  | deleteIth (nil, i)   = nil;


(******** PIGLATINIZE ********)
(*
 
Converts any word into its piglatin version. 

*)
					     
(* rotates a list once *)
fun rotate (nil) = nil
  | rotate (x::xs) = xs @ [x]; 

(* checks if first char in list  matches a vowel *)
fun isVowel (#"a"::_) = true
  | isVowel (#"e"::_) = true
  | isVowel (#"i"::_) = true
  | isVowel (#"o"::_) = true
  | isVowel (#"u"::_) = true
  | isVowel (_)       = false;

(* on first run, if first char is vowel, concatenates imploded list with "yay"
    otherwise, recurses on a rotated list with suffix "ay" *)
fun piglatinizeaux (x, suffix) = if isVowel (x) then (implode(x) ^ suffix)
				 else piglatinizeaux (rotate (x), "ay");	

(* accepts a string, explodes it into chars, and passes it to helper
    piglatinaux with initial suffix *)
fun piglatinize (x) = piglatinizeaux ((explode x), "yay");

