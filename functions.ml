(* Every even natural number greater than 2 can be expressed as the sum of two prime numbers. It has been verified computationally for all integers less than 4 × 10^18. *) 

let rec isPrime num a =
if a < 2 then true 
else if (num mod a) = 0 then false
else isPrime num (a-1);;

let goldbach_conjecture num =
let rec helper x = 
if num <= 2 then (-1,-1)
else if isPrime (x) (x-1) == true && isPrime(num-x) (num-x-1) then (x, num-x)
else helper (x+1)
in helper 2;;
