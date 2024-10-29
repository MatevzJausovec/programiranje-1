(*----------------------------------------------------------------------------*
 # Podatkovni tipi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Valute

 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute. Oglejmo si dva pristopa k izboljšavi
 varnosti pri uporabi valut.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tipa `euro` in `dollar`, kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število. Nato napišite funkciji
 `euro_to_dollar` in `dollar_to_euro`, ki primerno pretvarjata valuti (točne
 vrednosti pridobite na internetu ali pa si jih izmislite).

 Namig: Občudujte informativnost tipov funkcij.
[*----------------------------------------------------------------------------*)

type euro = Euro of float

type dollar = Dollar of float

let dollar_to_euro (Dollar x) = Euro (x *. 0.93)

let euro_to_dollar (Euro x) = Dollar (x *. 1.08)

(* let primer_valute_1 = dollar_to_euro (Dollar 0.5) *)
(* val primer_valute_1 : euro = Euro 0.4305 *)

(*----------------------------------------------------------------------------*
 Definirajte tip `currency` kot en vsotni tip z konstruktorji za jen, funt in
 švedsko krono. Nato napišite funkcijo `to_pound`, ki primerno pretvori valuto
 tipa `currency` v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
 Ocaml sam opozori, da je potrebno popraviti funkcijo `to_pound`.
[*----------------------------------------------------------------------------*)

type currency =
| Yen of float
| Funt of float
| SVK of float

let to_pound = function
| Funt x -> Funt x
| Yen x -> Funt (0.0050 *. x)
| SVK x -> Funt (0.073 *. x)

(* let primer_valute_2 = to_pound (Yen 100.) *)
(* val primer_valute_2 : currency = Pound 0.700000000000000067 *)

(*----------------------------------------------------------------------------*
 ## Mešani seznami

 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip `list` predstavimo s konstruktorjem za prazen seznam
 `Nil`(oz. `[]` v Ocamlu) in pa konstruktorjem za člen `Cons(x, xs)` (oz. `x ::
 xs` v Ocamlu).
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tip `intbool_list` z konstruktorji za:

 - prazen seznam,
 - člen s celoštevilsko vrednostjo,
 - člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal `[5; true; false; 7]`.
[*----------------------------------------------------------------------------*)

type intbool_list =
| Nil
| Int of int * intbool_list
| Bool of bool * intbool_list

let test = Int (5, Bool (true, Bool (false, Int (7, Nil))))

(*----------------------------------------------------------------------------*
 Funkcija `intbool_map f_int f_bool ib_list` preslika vrednosti `ib_list` v nov
 `intbool_list` seznam, kjer na elementih uporabi primerno od funkcij `f_int`
 oz. `f_bool`.
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool = function
  | Nil -> Nil
  | Int (n, ib_list) -> Int ((f_int n), intbool_map f_int f_bool ib_list)
  | Bool (b, ib_list) -> Bool ((f_bool b), intbool_map f_int f_bool ib_list)

(*----------------------------------------------------------------------------*
 Funkcija `intbool_reverse` obrne vrstni red elementov `intbool_list` seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let intbool_reverse ib_list = 
  let rec intbool_reverse' acc = function
  | Nil -> acc
  | Int (n, ib_list) -> intbool_reverse' (Int (n, acc)) ib_list
  | Bool (b, ib_list) -> intbool_reverse' (Bool (b, acc)) ib_list
in
intbool_reverse' Nil ib_list

(*----------------------------------------------------------------------------*
 Funkcija `intbool_separate ib_list` loči vrednosti `ib_list` v par `list`
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let intbool_separate ib_list =
  let rec intbool_separate' i_list b_list = function
  | Nil -> (i_list, b_list)
  | Int (n, ib_list) -> intbool_separate' (n::i_list) b_list ib_list
  | Bool (b, ib_list) -> intbool_separate' i_list (b::b_list) ib_list
in
intbool_separate' [] [] ib_list

(*----------------------------------------------------------------------------*
 ## Čarodeji

 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 `magic`, ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire, frost
 in arcane. Ko se čarodej zaposli na akademiji, se usmeri v zgodovino,
 poučevanje ali raziskovanje oz. historian, teacher in researcher. Definirajte
 tip `specialisation`, ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic =
| Fire
| Frost
| Arcane

type specialisation =
| History
| Teacher
| Researcher

(*----------------------------------------------------------------------------*
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent, na
 koncu pa SE lahko tudi zaposli. Definirajte tip `status`, ki določa ali je
 čarodej:

 - začetnik `Newbie`,
 - študent `Student` (in kateri vrsti magije pripada in koliko časa študira),
 - zaposlen `Employed` (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip `wizard` z poljem za ime in poljem za trenuten
 status ter dodajte primer `professor`, ki je zaposlen učitelj magije ognja, in
 `jaina`, ki je četrto leto študentka magije ledu.
[*----------------------------------------------------------------------------*)

type status =
| Newbie
| Student of magic * int
| Employed of magic * specialisation

type wizard = {ime: string; status: status}

let professor  = {ime = "Matu"; status = Employed (Fire, Teacher)}

let jaina  = {ime = "Jaina"; status = Student (Frost, 4)}

(*----------------------------------------------------------------------------*
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip `magic_counter`, ki v posameznem polju hrani število
 uporabnikov magije. Nato definirajte funkcijo `update counter magic`, ki vrne
 nov števec s posodobljenim poljem glede na vrednost `magic`.
[*----------------------------------------------------------------------------*)

type magic_counter = {fire: int; frost: int; arcane: int}

let update {fire = a; frost = b; arcane = c} = function
| Fire -> {fire = a + 1; frost = b; arcane = c}
| Frost -> {fire = a; frost = b + 1; arcane = c}
| Arcane -> {fire = a; frost = b; arcane = c + 1}

(* let primer_carovniki_1 = update {fire = 1; frost = 1; arcane = 1} Arcane *)
(* val primer_carovniki_1 : magic_counter = {fire = 1; frost = 1; arcane = 2} *)

(*----------------------------------------------------------------------------*
 Funkcija `count_magic` sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
[*----------------------------------------------------------------------------*)

let count_magic list =
  let rec count_magic' counter = function
  | [] -> counter
  | {ime ; status}::t -> 
    match status with 
    | Newbie -> count_magic' counter t
    | Student (mag, _) -> count_magic' (update counter mag) t
    | Employed (mag, _) -> count_magic' (update counter mag) t
  in
  count_magic' {fire = 0; frost = 0; arcane = 0} list


(* let primer_carovniki_2 = count_magic [professor; professor; professor] *)
(* val primer_carovniki_2 : magic_counter = {fire = 3; frost = 0; arcane = 0} *)

(*----------------------------------------------------------------------------*
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija. Funkcija `find_candidate magic
 specialisation wizard_list` poišče prvega primernega kandidata na seznamu
 čarodejev in vrne njegovo ime, čim ustreza zahtevam za `specialisation` in
 študira vrsto `magic`. V primeru, da ni primernega kandidata, funkcija vrne
 `None`.
[*----------------------------------------------------------------------------*)

let rec find_candidate magic specialisation wizard_list =
  match wizard_list with
  | [] -> None
  | {ime ; status}::t ->
    match status with
    | Student (mag, leta) when mag = magic ->
      begin match specialisation with
      | History when leta > 2 -> Some ime
      | Researcher when leta > 3 -> Some ime
      | Teacher when leta > 4 -> Some ime
      | _ -> find_candidate magic specialisation t
    end
    | _ -> find_candidate magic specialisation t

(*let rec find_candidate magic specialisation wizard_list =
  let n =
    match specialisation with
    | History -> 3
    | Researcher -> 4
    | Teacher -> 5
  in
  match wizard_list with
  | [] -> None
  | {ime ; status}::t ->
    match status with
    | Student (mag, leta) when mag = magic and leta >= n -> Some ime
    | _ -> find_candidate magic specialisation t*)


(* let primer_carovniki_3 =
  find_candidate Frost Researcher [professor; jaina] *)
(* val primer_carovniki_3 : string option = Some "Jaina" *)
