(*----------------------------------------------------------------------------*
 # Abstrakcija
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Naravna števila
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte signaturo `NAT`, ki določa strukturo naravnih števil. Ima osnovni
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov `int` tip. Opomba: Funkcije za
 pretvarjanje ponavadi poimenujemo `to_int` and `of_int`, tako da skupaj z
 imenom modula dobimo ime `NAT.of_int`, ki nam pove, da pridobivamo naravno
 število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t
  val eq  : t -> t -> bool
  val zero : t
  val unit : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val to_int : t -> int
  val of_int : int -> t
end

(*----------------------------------------------------------------------------*
 Napišite implementacijo modula `Nat_int`, ki zgradi modul s signaturo `NAT`,
 kjer kot osnovni tip uporablja OCamlov tip `int`. Namig: dokler ne
 implementirate vse funkcij v `Nat_int`, se bo OCaml pritoževal. Temu se lahko
 izognete tako, da funkcije, ki še niso napisane nadomestite z `failwith
 "later"`, vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct

  type t = int
  let eq x y = (x = y)
  let zero = 0
  let unit = 1
  let ( + ) n m = n + m
  let ( * ) n m = n * m
  let ( - ) n m = 
    match (n - m) with
    | k when k > 0 -> k
    | _ -> 0
  let to_int (n: t): int = n
  let of_int  (n: int) =
  match n with
    | k when k > 0 -> k
    | _ -> 0
end

(*----------------------------------------------------------------------------*
 Napišite implementacijo `NAT`, ki temelji na [Peanovih
 aksiomih](https://en.wikipedia.org/wiki/Peano_axioms). Osnovni tip modula
 definirajte kot naštevni tip, ki vsebuje konstruktor za ničlo in konstruktor za
 naslednika nekega naravnega števila. Večino funkcij lahko implementirate s
 pomočjo rekurzije. Naprimer, enakost števil `k` in `l` določimo s hkratno
 rekurzijo na `k` in `l`, kjer je osnoven primer `Zero = Zero`.
[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = 
  | Nil
  | Komp of unit * t
  let eq x y = failwith "later"
  let zero = Nil (* To morate spremeniti! *)
  (* Dodajte manjkajoče! *)

  let eq x y = (x = y)
  let unit = Komp ((), Nil)

  let to_int (k: t): int = 
    let rec to_int' (n: int) = function
      | Nil -> n
      | Komp ((), t) -> to_int' (n + 1) t
    in
    to_int' 0 k

  let of_int n =
    let rec of_int' acc = function
      | n when n <= 0 -> acc
      | n -> of_int' (Komp ((), acc)) (n - 1)
    in
    of_int' Nil n

  let rec ( + ) n = function
    | Nil -> n
    | Komp((), m) -> ( + ) (Komp((), n)) m

  let ( * ) n m = 
    let rec aux acc n = function
      | Nil -> acc
      | Komp((), m) -> aux (acc + n) n m
    in
    aux Nil n m

  let rec ( - ) n = function
    | Nil -> n
    | Komp((), m) ->
      match n with
      | Nil -> Nil
      | Komp((), n) -> ( - ) n m

end

(*----------------------------------------------------------------------------*
 Z ukazom `let module ImeModula = ... in ...` lahko modul definiramo samo
 lokalno. To bomo uporabili za to, da bomo lahko enostavno preklapljali med
 moduloma `Nat_int` in `Nat_peano`, saj bomo enega ali drugega shranili pod ime
 `Nat`. OCaml sicer pozna tudi ustrezne abstrakcije, ki omogočijo preklapljanje
 med moduli, na primer [funktorje](https://ocaml.org/docs/functors) ali
 [prvorazredne module](https://ocaml.org/manual/5.2/firstclassmodules.html), a
 bomo uporabili preprostejšo rešitev.

 Spodnji izračun dopolnite tako, da sešteje prvih 100 naravnih števil. Ker bo
 taka vsota tipa `NAT.t`, ki je abstrakten, končni rezultat pretvorite v tip
 `int` z uporabo funkcije `Nat.to_int`. Če ste oba modula implementirali
 pravilno, bi morali dobiti enak rezultat ne glede na to, katerega poimenujete
 `Nat`.
[*----------------------------------------------------------------------------*)

let sum_nat_100 = 
  (* let module Nat = Nat_int in *)
  let module Nat = Nat_peano in
  let rec sum acc a b =
    match a with
    | x when Nat.eq a b -> acc
    | x -> sum (Nat.(acc + x)) Nat.(x + Nat.unit) b
  in sum Nat.zero Nat.unit (Nat.of_int 101)
  |> Nat.to_int
(* val sum_nat_100 : int = 5050 *)

(*----------------------------------------------------------------------------*
 ## Kompleksna števila
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 > Once upon a time, there was a university with a peculiar tenure
 > policy. All faculty were tenured, and could only be dismissed for
 > moral turpitude. What was peculiar was the definition of moral
 > turpitude: making a false statement in class. Needless to say, the
 > university did not teach computer science. However, it had a renowned
 > department of mathematics.
 >
 > One Semester, there was such a large enrollment in complex variables
 > that two sections were scheduled. In one section, Professor Descartes
 > announced that a complex number was an ordered pair of reals, and that
 > two complex numbers were equal when their corresponding components
 > were equal. He went on to explain how to convert reals into complex
 > numbers, what "i" was, how to add, multiply, and conjugate complex
 > numbers, and how to find their magnitude.
 >
 > In the other section, Professor Bessel announced that a complex number
 > was an ordered pair of reals the first of which was nonnegative, and
 > that two complex numbers were equal if their first components were
 > equal and either the first components were zero or the second
 > components differed by a multiple of 2π. He then told an entirely
 > different story about converting reals, "i", addition, multiplication,
 > conjugation, and magnitude.
 >
 > Then, after their first classes, an unfortunate mistake in the
 > registrar's office caused the two sections to be interchanged. Despite
 > this, neither Descartes nor Bessel ever committed moral turpitude,
 > even though each was judged by the other's definitions. The reason was
 > that they both had an intuitive understanding of type. Having defined
 > complex numbers and the primitive operations upon them, thereafter
 > they spoke at a level of abstraction that encompassed both of their
 > definitions.
 >
 > The moral of this fable is that: Type structure is a syntactic
 > discipline for enforcing levels of abstraction.
 >
 > John C. Reynolds, _Types, Abstraction, and Parametric Polymorphism_, IFIP83
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte signaturo modula kompleksnih števil. Potrebujemo osnovni tip, test
 enakosti, ničlo, enko, imaginarno konstanto i, negacijo, konjugacijo,
 seštevanje in množenje.
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t
  val eq : t -> t -> bool
  val zero : t
  val unit : t
  val i : t
  val to_pair : t -> float * float
  val of_pair : float * float -> t
  val neg : t -> t
  val kon : t -> t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
end

(*----------------------------------------------------------------------------*
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}
  let eq {re = re1; im = im1} {re = re2; im = im2} = (re1 = re2) && (im1 = im2)

  let zero = {re= 0.; im= 0.}
  let unit = {re= 1.; im= 0.}
  let i = {re= 0.; im= 1.}
  let to_pair {re; im} = (re, im)
  let of_pair (re, im) = {re; im}
  let neg {re; im} = {re = -1. *. re; im = -1. *. im}
  let kon {re; im} = {re; im = -1. *. im}
  let ( + ) {re = re1; im = im1} {re = re2; im = im2} = {re = re1 +. re2; im = im1 +. im2}
  let ( * ){re = re1; im = im1} {re = re2; im = im2} = {re = re1 *. re2 -. im1 *. im2; im = re1 *. im2 +. re2 *. im1}

end

(*----------------------------------------------------------------------------*
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument). Priporočilo:
 Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga pustite za konec
 (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  let zero = {magn = 0.; arg = 0.}
  let correct {magn; arg} =
    match magn, arg with
    | r, _ when r <= 0. -> zero
    | r, fi -> 
      let rec under_2pi = function
        | fi when fi < 0. -> under_2pi (fi +. 2. *. pi)
        | fi when fi >= 2. *. pi -> under_2pi (fi +. 2. -. pi)
        | fi -> fi
      in
      {magn = magn; arg = under_2pi fi}

  let eq c1 c2 = 
    let eq' {magn = magn1; arg = arg1} {magn = magn2; arg = arg2} = (magn1 = magn2) && (arg1 = arg2)
    in eq' (correct c1) (correct c2)
   
  let unit = {magn = 1.; arg = 0.}
  let i = {magn = 1.; arg = pi/.2.}
  let to_pair {magn; arg}  = (magn, arg)
  let of_pair (magn, arg) = correct {magn; arg}
  let neg {magn; arg} = correct {magn; arg = pi +. arg}
  let kon {magn; arg} = correct {magn; arg = -1. *. arg}

  let ( * ) {magn = magn1; arg = arg1} {magn = magn2; arg = arg2} = correct {magn = magn1 *. magn2; arg = arg1 +. arg2}
  
  let cart_to_pol (a, b) =
    let fi = 
      match a, b with
      | 0., y when y < 0. -> 1.5 *. pi
      | 0., y -> 0.5 *. pi
      | x, y when x > 0. -> atan (b/.a)
      | x, y -> pi +. atan (b/.a)
    in
    correct {magn = sqrt(a *. a +. b *. b); arg = fi}
  let pol_to_cart {magn; arg} = (magn *. cos arg, magn *. sin arg)

  let ( + ) c1 c2 = 
    let a1, b1 = pol_to_cart c1 in
    let a2, b2 = pol_to_cart c2 in
    cart_to_pol (a1 +. a2, b1 +. b2)

end
