fun binominal n k =
  if k = 0 then 1
  else if k = n then 1
  else if k > n then 0
  else binominal (n - 1) (k - 1) + binominal (n - 1) k

(**)

fun nextRow [] = [1]
  | nextRow [x] = [1]
  | nextRow (x::y::rest) = (x + y) :: nextRow (y::rest)

fun pascal 0 = [1]
  | pascal n =
      let
        val prevRow = pascal (n - 1)
      in
        1 :: nextRow prevRow @ [1]
      end

fun binomial2 n k =
  if k < 0 orelse k > n then
    0
  else
    List.nth (pascal n, k)

(**)

fun merge (xs, []) = xs
  | merge ([], ys) = ys
  | merge (x::xs, y::ys) = if x <= y then x :: merge (xs, y::ys) else y :: merge (x::xs, ys)

fun mergesort [] = []
  | mergesort [x] = [x]
  | mergesort xs = 
    let
        val n = List.length xs div 2
        val ys = List.take(xs, n)
        val zs = List.drop(xs, n)
    in
        merge (mergesort ys, mergesort zs)
    end

(**)
exception NoSolution

fun extended_gcd 0 b = (b, 0, 1)
  | extended_gcd a b =
    let
        val (g, x, y) = extended_gcd (b mod a) a
    in
        (g, y - (b div a) * x, x)
    end

fun de a b z = 
    let
        val (g, x, y) = extended_gcd a b
    in
        if z = g then (x, y, z) else raise NoSolution
    end

(**)

fun prime_factors n = 
    let
        fun factor (n, p) =
            if p * p > n then if n > 1 then [n] else []
            else if n mod p = 0 then p :: factor (n div p, p)
            else factor (n, p + 1)
    in
        factor (n, 2)
    end

(**)

fun gcd(a, 0) = a
  | gcd(a, b) = gcd(b, a mod b);

fun totient n = 
    List.length (List.filter (fn x => gcd (x, n) = 1) (List.tabulate (n, fn i => i + 1)))

(**)

fun prime_factors n =
    let
        fun factor (n, p) =
            if p * p > n then
                if n > 1 then [n] else []
            else if n mod p = 0 then
                p :: factor (n div p, p)
            else
                factor (n, p + 1)
    in
        factor (n, 2)
    end

(**)

fun unique [] = []
  | unique (x::xs) = x :: unique (List.filter (fn y => y <> x) xs)

fun float_product [] = 1.0
  | float_product (x::xs) = x * float_product xs

fun totient2 n =
    let
        val pf = unique (prime_factors n)
        val product_term = float_product (List.map (fn p => 1.0 - 1.0 / Real.fromInt p) pf)
    in
        Real.round (Real.fromInt n * product_term)
    end

