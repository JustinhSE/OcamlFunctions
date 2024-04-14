(* Every even natural number greater than 2 can be expressed as the sum of two prime numbers. It has been verified computationally for all integers less than 4 × 10^18. *) 

let rec isPrime num a =
if a < 2 then true 
else if (num mod a) = 0 then false
else isPrime num (a-1);;
(* ^^ Helper function ^^ *)

let goldbach_conjecture num =
let rec helper x = 
if num <= 2 then (-1,-1)
else if isPrime (x) (x-1) == true && isPrime(num-x) (num-x-1) then (x, num-x)
else helper (x+1)
in helper 2;;

(* The powerset of S is the set of all subsets of S, including the empty set and S itself. *) 

let allCombos head tail =
let rec helper lst tail = match tail with
| [] -> lst
| h::t -> if (size t) >= 2 then helper (lst @ [[head;h]] @ [[head] @ t] ) t
else helper (lst @ [[head;h]]) t in helper [] tail;;
(* ^^ Helper Function ^^ *) 

let power_sets lst = if size lst = 2 then [lst;[(List.hd lst)];(List.tl lst);[]] else if size lst = 1 then [lst] else 
let rec aux result lst = match lst with
| [] -> result @ [[]]
| h::t -> 
if (size t) > 2 then aux (result @ [[h]] @ [t] @ (allCombos h t)) t 
else aux (result @ [[h]] @ (allCombos h t)) t in aux [lst] lst;;

