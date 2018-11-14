(* 
** Nathan Quirk
** CSC 345
** ML Homework
** Submitted 11.15.18
*)


(*********** Flip *********)
(* 
    "Flips alternate elements of any list" 
*)

fun flip (nil)      = nil
  | flip ([x])      = [x]
  | flip (x::y::zs) = y::x::flip(zs);


(********* DeleteIth *********)
(* 
    "Returns a list with the ith memeber removed. 
    If i is greater than the length of the list,
    the list is returned unchanged." 
*)

fun deleteIth (x::xs, 1) = xs
  | deleteIth (x::xs, i) = x::deleteIth (xs, i - 1)
  | deleteIth (nil, i)   = nil;


(******** PigLatinize ********)
(* 
    "Converts any word into its piglatin version." 
*)

fun piglatinize (x) = piglatinizeaux ((explode x), "yay")				     
and piglatinizeaux (x, suffix) = if isVowel (x) then (implode(x) ^ suffix)
				 else piglatinizeaux (rotate (x), "ay")						     
and rotate (nil)   = nil
  | rotate (x::xs) = xs @ [x]			      
and isVowel (#"a"::_) = true
  | isVowel (#"e"::_) = true
  | isVowel (#"i"::_) = true
  | isVowel (#"o"::_) = true
  | isVowel (#"u"::_) = true
  | isVowel (_)       = false;

