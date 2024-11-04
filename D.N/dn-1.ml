(* # 1. domača naloga *)

(* Nekaj pomožnih funkcij za sezname 
   Uporabljene v raznih delih naloge. *)

let reverse list = 
   let rec reverse' acc = function
      | [] -> acc
      | h::t -> reverse' (h::acc) t
   in
   reverse' [] list

let len list =
   let rec len' acc = function
      | [] -> acc
      | _::t -> len' (acc + 1) t
   in
   len' 0 list

let list_mapi f list =
  let rec mapi' acc i f = function
   | [] -> acc
   | h::t -> mapi' ((f h i)::acc) (i + 1) f t
   in
   reverse (mapi' [] 0 f list)

let list_map f list =
   let rec map' acc  f = function
      | [] -> acc
      | h::t -> map' ((f h)::acc)  f t
   in
   reverse (map' [] f list)

let zdruzi list1 list2 =
   let rec zdruzi' list2 = function
      | [] -> list2
      | h::t -> zdruzi' (h::list2) t
   in 
   zdruzi' list2 (reverse list1)

(* Konec pomožnih funkcij za sezname *)

(* ## Ogrevanje *)

(** Števke *)

let stevke baza numb = 
   let rec stevke' acc baza numb =
      let a = numb / baza in
      begin match a with
      | 0 -> numb::acc
      | _ -> stevke' ((numb - (baza * a))::acc) baza a
      end
   in
   stevke' [] baza numb
  
(* let primer_1_1 = stevke 10 12345 *)
(* let primer_1_2 = stevke 2 42 *)
(* let primer_1_3 = stevke 16 ((3 * 16 * 16 * 16) + (14 * 16 * 16) + (15 * 16) + 9) *)

(** Začetek seznama *)

let take n list =
   let rec take' acc n list =
      match n, list with
      | _, [] -> reverse acc
      | 0, _ -> reverse acc
      | n, h::t -> take' (h::acc) (n - 1) t
   in
   take' [] n list

(* let primer_1_4 = take 3 [ 1; 2; 3; 4; 5 ] *)
(* let primer_1_5 = take 10 [ 1; 2; 3; 4; 5 ] *)

(** Odstranjevanje ujemajočih *)

let rec drop_while f = function
   | h::t when (f h) = true -> drop_while f t
   | list -> list

(* let primer_1_6 = drop_while (fun x -> x < 5) [ 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 ] *)
(* let primer_1_7 = drop_while (fun x -> x < 5) [ 9; 8; 7; 6; 5; 4; 3; 2; 1; 0 ] *)

(** Funkcija `filter_mapi` *)

let filter_mapi f list = 
   let rec filter_mapi' acc x f list = 
      match list with
      | [] -> acc
      | h::t ->
         match (f x h) with
         | None -> filter_mapi' acc (x + 1) f t
         | Some b -> filter_mapi' ((b)::acc) (x + 1) f t
   in
   filter_mapi' [] 0 f (reverse list)

(* let primer_1_8 =
   filter_mapi
     (fun i x -> if i mod 2 = 0 then Some (x * x) else None)
     [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] *)

(* ## Izomorfizmi množic *)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

(** $A \times B \cong B \times A$ *)

let phi1 (a, b) = (b, a)

let psi1 = phi1

(** $A + B \cong B + A$ *)

let phi2 = function
   | In1 a -> In2 a
   | In2 b -> In1 b

let psi2 = phi2

(** $A \times (B \times C) \cong (A \times B) \times C$ *)

let phi3 (a, (b, c)) = ((a, b), c)

let psi3 ((a, b), c) = (a, (b, c))

(** $A + (B + C) \cong (A + B) + C$ *)

let phi4 (x: ('a, ('b, 'c) sum) sum) =
   match x with
   | In1 a -> In1 (In1 a)
   | In2 zmesni -> 
      match zmesni with
      | In1 b -> In1 (In2 b)
      | In2 c -> In2 c

let psi4 = function
   | In2 c -> In2 (In2 c)
   | In1 zmesni ->
      match zmesni with
      | In1 a -> In1 a
      | In2 b -> In2 (In1 b)

(** $A \times (B + C) \cong (A \times B) + (A \times C)$ *)

let phi5 = function
   | a, In1 b -> In1 (a, b)
   | a, In2 c -> In2 (a, c)

let psi5 = function
   | In1 (a, b) -> (a, In1 b)
   | In2 (a, c) -> (a, In2 c)

(** $A^{B + C} \cong A^B \times A^C$ *)

let phi6 (f: (('a, 'b) sum) -> 'c) = 
   let f1 a = f (In1 a) in
   let f2 b = f (In2 b) in
   (f1, f2)

let psi6 (f1, f2) =
   let f = function
      | In1 b -> f1 b
      | In2 c -> f2 c
   in f

(** $(A \times B)^C \cong A^C \times B^C$ *)

let phi7 f =
   let f1 c=
      let (a, b) = f c in
      a in
   let f2 c=
      let (a, b) = f c in
      b in
   (f1, f2)

let psi7 (f1, f2) =
   fun c -> (f1 c, f2 c)

(* ## Polinomi *)

type polinom = int list

(** Odstranjevanje odvečnih ničel *)

let pocisti (list: polinom): polinom =
   let rec pocisti' = function
      | [] -> []
      | 0::t -> pocisti' t
      | list -> list
   in
   reverse (pocisti' (reverse list))

(* let primer_3_1 = pocisti [ 1; -2; 3; 0; 0 ] *)

(** Seštevanje *)

let ( +++ ) (list1: polinom) (list2: polinom) =
   let rec aux acc list1 list2 =
      match list1, list2 with
      | [], [] -> acc
      | [], h2::t2 -> aux (h2::acc) [] t2
      | h1::t1, [] -> aux (h1::acc) t1 []
      | h1::t1, h2::t2 -> aux ((h1 + h2)::acc) t1 t2
   in
   pocisti (reverse (aux [] list1 list2))

(* let primer_3_2 = [ 1; -2; 3 ] +++ [ 1; 2 ] *)
(* let primer_3_3 = [ 1; -2; 3 ] +++ [ 1; 2; -3 ] *)

(** Množenje *)

(* Produk je zapisan po definiciji v obliki dvojne sume. *)

let ( *** ) (list1: polinom) (list2: polinom) =
   let n1 = (len list1) - 1 in
   let n2 = (len list2) -1 in
   let max_p = n1 + n2 in
   let rec nth_el n list =
      match n, list with
      | _, [] -> 0
      | 0, h::t -> h
      | i, h::t -> nth_el (i - 1) t
   in
   let rec sum2 acc i n list1 list2 =
      match i with
      | x when x = n + 1 -> acc
      | x -> sum2 (((nth_el x list1) * (nth_el (n - x) list2)) + acc) (i + 1) n list1 list2
   in
   let rec sum1 i acc list1 list2 =
      match i with
      | n when n = max_p -> ((nth_el n1 list1) * (nth_el n2 list2))::acc
      | 0 -> sum1 (i + 1) (((nth_el 0 list1) * (nth_el 0 list2))::acc) list1 list2
      | n -> sum1 (i + 1) ((sum2 0 0 n list1 list2)::acc) list1 list2
   in 
   pocisti (reverse (sum1 0 [] list1 list2))

(* let primer_3_4 = [ 1; 1 ] *** [ 1; 1 ] *** [ 1; 1 ] *)
(* let primer_3_5 = [ 1; 1 ] *** [ 1; -1 ] *)

(** Izračun vrednosti v točki *)

(* Funkciji 'vrednost' in 'odvod' sem preimenoval v 'vrednost_pilinoma' in 'odvod_polinoma',
   ker sta imeni bili konfliktni z funkcijama v poznejšem delu domače naloge.*)

let vrednost_polinoma (poli: polinom) x =
   let rec vrednost' acc p x = function
      | [] -> acc
      | h::t -> vrednost' (h * (int_of_float ((float_of_int x) ** (float_of_int p))) + acc) (p + 1) x t
   in vrednost' 0 0 x poli

(* let primer_3_6 = vrednost [ 1; -2; 3 ] 2 *)

(** Odvajanje *)

let odvod_polinoma (poli: polinom) =
   let rec odvod' i acc = function
      | [] -> acc
      | h::t -> odvod' (i + 1) ((i * h)::acc) t
   in match poli with
   | [] -> []
   | h::t -> pocisti (reverse (odvod' 1 [] t))

(* let primer_3_7 = odvod [ 1; -2; 3 ] *)

(** Lep izpis *)

(* String.map ne deluje, ker superscupt številke niso tipa char *)

let string_map (f: char -> string) str =
   let l = String.length str in
   let rec string_map' acc f str = function
      | i when i = l -> acc
      | i -> string_map' (acc ^ (f str.[i])) f str (i + 1)
   in
   string_map' "" f str 0

let izpis (poli: polinom) =
   let exp_list = ["⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹"]
   in 
   let f a i =
      match a, i with
      | 0, _ -> "" 
      | b, 0 -> 
         begin match b with
         | a when a > 0 -> "+ " ^ (string_of_int a)
         | a -> "- " ^ (string_of_int (-a))
         end
      | b, 1 ->
         begin match b with
         | 1 -> "+ x "
         | -1 -> "- x "
         | a when a > 0 -> "+ " ^ (string_of_int a) ^ " x "
         | a -> "- " ^ (string_of_int (-a)) ^ " x "
         end
      | b, i ->
         begin match b with
         | 1 -> "+ x" ^ (string_map (fun (c: char) -> List.nth exp_list (int_of_string (Char.escaped c))) (string_of_int i)) ^ " "
         | -1 -> "- x" ^ (string_map (fun (c: char) -> List.nth exp_list (int_of_string (Char.escaped c))) (string_of_int i)) ^ " "
         | a when a > 0 -> "+ " ^ (string_of_int  a) ^ " x" ^ (string_map (fun (c: char) -> List.nth exp_list (int_of_string (Char.escaped c))) (string_of_int i)) ^ " "
         | a -> "- " ^ (string_of_int  (- a) ^ " x" ^ (string_map (fun (c: char) -> List.nth exp_list (int_of_string (Char.escaped c))) (string_of_int i))) ^ " "
         end
   in
   let rec together acc = function
      | [] -> acc
      | h::t -> together (acc ^ h) t
   in
   let end_str = together "" (reverse ( list_mapi f poli))
   in
   if end_str = "" then end_str else
      match end_str.[0] with
      | '+' -> String.trim (String.sub end_str 2 ((String.length end_str) - 2))
      | _ -> "-" ^ String.trim (String.sub end_str 2 ((String.length end_str) - 2))

(* let primer_3_8 = izpis [ 1; 2; 1 ] *)
(* let primer_3_9 = izpis [ 1; 0; -1; 0; 1; 0; -1; 0; 1; 0; -1; 0; 1 ] *)
(* let primer_3_10 = izpis [ 0; -3; 3; -1 ] *)

(* ## Samodejno odvajanje *)

let priblizek_odvoda f x0 h = (f (x0 +. h) -. f x0) /. h

(* let primer_3_11 =
   let f x = sin x +. cos x +. exp x in
   List.map (priblizek_odvoda f 1.) [ 0.1; 0.01; 0.001; 0.0001; 0.00001 ] *)

type odvedljiva = (float -> float) * (float -> float)

let sinus : odvedljiva = (sin, cos)
let kosinus : odvedljiva = (cos, fun x -> -.sin x)
let eksp : odvedljiva = (exp, exp)

let ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva =
 (* pozorni bodite, da anonimni funkciji v paru date med oklepaje *)
 fun (f, f') (g, g') -> ((fun x -> f x +. g x), fun x -> f' x +. g' x)

(* let primer_3_12 =
   let _, f' = sinus ++. kosinus ++. eksp in
   f' 1. *)

(** Vrednost odvoda *)

let vrednost (f, f') x = f x
let odvod (f, f') x = f' x

(** Osnovne funkcije *)

let konstanta c: odvedljiva = ((fun x -> c), (fun x -> 0. ))
let identiteta: odvedljiva = ((fun x -> x), (fun x -> 1.))

(** Produkt in kvocient *)

let ( **. ) ((f1, f1'): odvedljiva) ((f2, f2'): odvedljiva): odvedljiva =
   ((fun x -> (f1 x) *. (f2 x)),(fun x -> ((f1' x) *. (f2 x)) +. ((f1 x) *. (f2' x))))

let ( //. )  ((f1, f1'): odvedljiva) ((f2, f2'): odvedljiva): odvedljiva =
   ((fun x -> (f1 x) /. (f2 x)) , (fun x -> (((f1' x) *. (f2 x)) -. ((f1 x) *. (f2' x))) /. ((f2 x) ** 2.)))

(* let kvadrat = identiteta **. identiteta *)

(** Kompozitum *)

let ( @@. )((f1, f1'): odvedljiva) ((f2, f2'): odvedljiva): odvedljiva =
   ((fun x -> f1 (f2 x)), (fun x -> (f1' (f2 x)) *. (f2' x)))

(* let vedno_ena = (kvadrat @@. sinus) ++. (kvadrat @@. kosinus) *)
(* let primer_4_1 = vrednost vedno_ena 12345. *)
(* let primer_4_2 = odvod vedno_ena 12345. *)

(* ## Substitucijska šifra *)

let quick_brown_fox = "THEQUICKBRWNFXJMPSOVLAZYDG"
let rot13 = "NOPQRSTUVWXYZABCDEFGHIJKLM"
let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A')

(** Šifriranje *)

let sifriraj sifra tekst =
   let f sifra znak = 
      match (indeks znak) with
      | i when 0 <= i && i <= 25 -> sifra.[indeks znak]
      | _ -> znak
   in
   String.map (f sifra) tekst

(* let primer_5_1 = sifriraj quick_brown_fox "HELLO, WORLD!" *)
(* let primer_5_2 = "VENI, VIDI, VICI" |> sifriraj rot13 *)
(* let primer_5_3 = "VENI, VIDI, VICI" |> sifriraj rot13 |> sifriraj rot13 *)

(** Inverzni ključ *)

let inverz sifra = 
   let rec inverz' acc sifra = function
      | i when i = String.length sifra -> acc
      | i -> 
         if String.contains sifra (crka i)
         then inverz' (acc ^ (Char.escaped (crka (String.index sifra (crka i))))) sifra (i + 1)
         else inverz' (acc ^ "_") sifra (i + 1)
   in
   inverz' "" sifra 0

(* let primer_5_4 = inverz quick_brown_fox *)
(* let primer_5_5 = inverz rot13 = rot13 *)
(* let primer_5_6 = inverz "BCDEA" *)

(** Ugibanje ključa *)

let besede =
  "the of to and a in is it you that he was for on are with as i his they be \
   at one have this from or had by word but what some we can out other were \
   all there when up use your how said an each she which do their time if will \
   way about many then them write would like so these her long make thing see \
   him two has look more day could go come did number sound no most people my \
   over know water than call first who may down side been now find any new \
   work part take get place made live where after back little only round man \
   year came show every good me give our under name very through just form \
   sentence great think say help low line differ turn cause much mean before \
   move right boy old too same tell does set three want air well also play \
   small end put home read hand port large spell add even land here must big \
   high such follow act why ask men change went light kind off need house \
   picture try us again animal point mother world near build self earth father \
   head stand own page should country found answer school grow study still \
   learn plant cover food sun four between state keep eye never last let \
   thought city tree cross farm hard start might story saw far sea draw left \
   late run don't while press close night real life few north open seem \
   together next white children begin got walk example ease paper group always \
   music those both mark often letter until mile river car feet care second \
   book carry took science eat room friend began idea fish mountain stop once \
   base hear horse cut sure watch color face wood main enough plain girl usual \
   young ready above ever red list though feel talk bird soon body dog family \
   direct pose leave song measure door product black short numeral class wind \
   question happen complete ship area half rock order fire south problem piece \
   told knew pass since top whole king space heard best hour better true . \
   during hundred five remember step early hold west ground interest reach \
   fast verb sing listen six table travel less morning ten simple several \
   vowel toward war lay against pattern slow center love person money serve \
   appear road map rain rule govern pull cold notice voice unit power town \
   fine certain fly fall lead cry dark machine note wait plan figure star box \
   noun field rest correct able pound done beauty drive stoDo contain front \
   teach week final gave green oh quick develop ocean warm free minute strong \
   special mind behind clear tail produce fact street inch multiply nothing \
   course stay wheel full force blue object decide surface deep moon island \
   foot system busy test record boat common gold possible plane stead dry \
   wonder laugh thousand ago ran check game shape equate hot miss brought heat \
   snow tire bring yes distant fill east paint language among grand ball yet \
   wave drop heart am present heavy dance engine position arm wide sail \
   material size vary settle speak weight general ice matter circle pair \
   include divide syllable felt perhaps pick sudden count square reason length \
   represent art subject region energy hunt probable bed brother egg ride cell \
   believe fraction forest sit race window store summer train sleep prove lone \
   leg exercise wall catch mount wish sky board joy winter sat written wild \
   instrument kept glass grass cow job edge sign visit past soft fun bright \
   gas weather month million bear finish happy hope flower clothe strange gone \
   jump baby eight village meet root buy raise solve metal whether push seven \
   paragraph third shall held hair describe cook floor either result burn hill \
   safe cat century consider type law bit coast copy phrase silent tall sand \
   soil roll temperature finger industry value fight lie beat excite natural \
   view sense ear else quite broke case middle kill son lake moment scale loud \
   spring observe child straight consonant nation dictionary milk speed method \
   organ pay age section dress cloud surprise quiet stone tiny climb cool \
   design poor lot experiment bottom key iron single stick flat twenty skin \
   smile crease hole trade melody trip office receive row mouth exact symbol \
   die least trouble shout except wrote seed tone join suggest clean break \
   lady yard rise bad blow oil blood touch grew cent mix team wire cost lost \
   brown wear garden equal sent choose fell fit flow fair bank collect save \
   control decimal gentle woman captain practice separate difficult doctor \
   please protect noon whose locate ring character insect caught period \
   indicate radio spoke atom human history effect electric expect crop modern \
   element hit student corner party supply bone rail imagine provide agree \
   thus capital won't chair danger fruit rich thick soldier process operate \
   guess necessary sharp wing create neighbor wash bat rather crowd corn \
   compare poem string bell depend meat rub tube famous dollar stream fear \
   sight thin triangle planet hurry chief colony clock mine tie enter major \
   fresh search send yellow gun allow print dead spot desert suit current lift \
   rose continue block chart hat sell success company subtract event \
   particular deal swim term opposite wife shoe shoulder spread arrange camp \
   invent cotton born determine quart nine truck noise level chance gather \
   shop stretch throw shine property column molecule select wrong gray repeat \
   require broad prepare salt nose plural anger claim continent oxygen sugar \
   death pretty skill women season solution magnet silver thank branch match \
   suffix especially fig afraid huge sister steel discuss forward similar \
   guide experience score apple bought led pitch coat mass card band rope slip \
   win dream evening condition feed tool total basic smell valley nor double \
   seat arrive master track parent shore division sheet substance favor \
   connect post spend chord fat glad original share station dad bread charge \
   proper bar offer segment slave duck instant market degree populate chick \
   dear enemy reply drink occur support speech nature range steam motion path \
   liquid log meant quotient teeth shell neck"

(* Vse kar je med dvema presledkoma je geslo v slovarju. "." je beseda, saj je v slovarju. "%^^&" ni beseda, ker "%^^&" ni v slovarju. 
   Glede na trenuten slovar prepoznamo "STOP .", ne pa "STOP !" 
   Da bomo prepoznali poljubne znake izven A-Z, dodamo prazno besedo. *)

let slovar = ""::(String.split_on_char ' ' (String.uppercase_ascii besede))

(* let primer_5_7 = take 42 slovar *)
(* let primer_5_8 = List.nth slovar 321 *)

(* Med ugibanjem seveda ne bomo poznali celotnega ključa. V tem primeru bomo za neznane črke uporabili znak `_`. Na primer, če bi vedeli, da je črka `A` v besedilu šifrirana kot `X`, črka `C` pa kot `Y`, bi ključ zapisali kot `"X_Y_______________________"`. *)
(*  *)
(* Napišite funkcijo `dodaj_zamenjavo : string -> char * char -> string option`, ki sprejme ključ ter ga poskusi razširiti z zamenjavo dane črke. Funkcija naj vrne `None`, če razširitev vodi v ključ, ki ni bijektiven (torej če ima črka že dodeljeno drugo zamenjavo ali če smo isto zamenjavo dodelili dvema različnima črkama). *)

(** Razširjanje ključa s črko *)

let dodaj_zamenjavo sifra (x, y) =
   if indeks x < 0 || indeks x > 25 ||
      indeks y < 0 || indeks y > 25 ||
      String.contains sifra y ||
      sifra.[indeks x] <> '_' then None
   else Some ((String.sub sifra 0 (indeks x)) ^ (Char.escaped y) ^ (String.sub sifra ((indeks x) + 1) ((String.length sifra) - 1 - indeks x)))

(* let primer_5_9 = dodaj_zamenjavo "AB__E" ('C', 'X') *)
(* let primer_5_10 = dodaj_zamenjavo "ABX_E" ('C', 'X') *)
(* let primer_5_11 = dodaj_zamenjavo "ABY_E" ('C', 'E') *)

(** Razširjanje ključa z besedo *)

(* S pomočjo funkcije `dodaj_zamenjavo` sestavite še funkcijo `dodaj_zamenjave : string -> string * string -> string option`, ki ključ razširi z zamenjavami, ki prvo besedo preslikajo v drugo. *)

(* Pri odšifriranju se bo zašifrirano besedlio razdelilo po ' '.
   Prvo preveri, če se besedi morda ne ujemata zaradi končnega ločila na beseda2, saj ta prihaja iz besedila.
   Znak izven A-Z na koncu besede torej ignorira, če je ta beseda prišla iz besedila.
   Beseda "YU!!" v besedilu se na priemr ujema z gesli "YU", "yu!" ali "yu!!", če bi se ta nahajala v slovarju.*)

let rec dodaj_zamenjave sifra (beseda1, beseda2) =
   if String.length beseda1 <> String.length beseda2 
   then
      if (String.length beseda2) - (String.length beseda1) = 1
         then 
            if (indeks beseda2.[(String.length beseda2) - 1] < 0 || indeks beseda2.[(String.length beseda2) - 1] > 25)
               then dodaj_zamenjave sifra (beseda1, String.sub beseda2 0 ((String.length beseda2) - 1))
               else None
         else None 
   else
   let rec dodaj_zamenjave' sifra (beseda1, beseda2) = function
      | i when i = String.length beseda1 -> Some sifra
      | i -> 
         if not (indeks beseda1.[i] < 0 || indeks beseda1.[i] > 25) then
            match (dodaj_zamenjavo sifra (beseda1.[i], beseda2.[i])) with
            | None -> if sifra.[indeks beseda1.[i]] = beseda2.[i] then dodaj_zamenjave' sifra (beseda1, beseda2) (i + 1) else None
            | Some str -> dodaj_zamenjave' str (beseda1, beseda2) (i + 1)
         else
            if beseda1.[i] = beseda2.[i] then dodaj_zamenjave' sifra (beseda1, beseda2) (i + 1)
            else None
   in
   dodaj_zamenjave' sifra (beseda1, beseda2) 0

(* let primer_5_12 = dodaj_zamenjave "__________________________" ("HELLO", "KUNNJ") *)
(* let primer_5_13 = dodaj_zamenjave "ABCDU_____________________" ("HELLO", "KUNNJ") *)
(* let primer_5_14 = dodaj_zamenjave "ABCDE_____________________" ("HELLO", "KUNNJ") *)

(** Vse možne razširitve *)

(* Sestavite funkcijo `mozne_razsiritve : string -> string -> string list -> string list`, ki vzame ključ, šifrirano besedo ter slovar vseh možnih besed, vrne pa seznam vseh možnih razširitev ključa, ki šifrirano besedo slikajo v eno od besed v slovarju. *)

let mozne_razsiritve sifra beseda_sif slovar =
   let rec mozne_razsiritve' acc sifra beseda_sif = function
      | [] -> acc
      | h::t -> 
         match dodaj_zamenjave sifra (h, beseda_sif) with
         | None -> mozne_razsiritve' acc sifra beseda_sif t
         | Some str -> mozne_razsiritve' (str::acc) sifra beseda_sif t
   in
   mozne_razsiritve' [] sifra beseda_sif slovar

(* let primer_5_15 =
   slovar
   |> mozne_razsiritve (String.make 26 '_') "KUNNJ"
   |> List.map (fun kljuc -> (kljuc, sifriraj kljuc "KUNNJ")) *)

(** Odšifriranje *)

(* Napišite funkcijo `odsifriraj : string -> string option`, ki sprejme šifrirano besedilo in s pomočjo slovarja besed ugane odšifrirano besedilo. Funkcija naj vrne `None`, če ni mogoče najti nobenega ustreznega ključa. *)

(* Funkcija je napisana tako, da zahteva, da slovar mora vsebovati vse besede, ki so bile uporabljene. Če na primer poskusimo odšifrirati "TROLOBORIGOR",
   funkcija vrne None.
   Funkcija ne prepozna znakov izven A-Z, ki se držijo začetka besede.
   Geslo v slovarju "ab" se na primer ujema z "vm,", ne pa z " vm".
   Funkcije 'odsifriraj_vse' vrne seznam vseh možnih verzij razšifriranega besedila, nato 'odsifriraj' vrne prvi element tega seznama oz. None, če je seznam prazen.
   Funkcija 'mozne_razsiritve_sifer' je map funkcije 'mozne_razsiritve' na seznam šifer. *)

let mozne_razsiritve_sifer mozne_sifre beseda =
   let rec mozne_razsiritve_sifer' acc beseda = function
      | [] -> acc
      | h::t -> mozne_razsiritve_sifer' (zdruzi (mozne_razsiritve h beseda slovar) acc) beseda t
   in
   mozne_razsiritve_sifer' [] beseda mozne_sifre

let odsifriraj_vse besedilo_sif = 
   let vse_besede = String.split_on_char ' ' besedilo_sif 
   in
   let rec iterator mozne_sifre vse_besede = 
      match vse_besede with
      | [] -> mozne_sifre
      | h::t -> iterator (mozne_razsiritve_sifer mozne_sifre h) t
   in
   let mozne_sifre = iterator [String.make 26 '_'] vse_besede
   in let mozne_razsiritve = list_map ((fun besedilo sifra -> sifriraj (inverz sifra) besedilo) besedilo_sif) mozne_sifre
   in mozne_razsiritve

let odsifriraj besedilo_sif =
   match odsifriraj_vse besedilo_sif with
   | [] -> None
   | h::t -> Some h

(* let primer_5_16 = sifriraj quick_brown_fox "THIS IS A VERY HARD PROBLEM" *)
(* let primer_5_17 = odsifriraj "VKBO BO T AUSD KTSQ MSJHNUF" *)
