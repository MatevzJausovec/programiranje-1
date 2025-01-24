(*----------------------------------------------------------------------------*
 # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi
 preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo
 izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike
 zafiksiramo tip `direction`:
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

(*----------------------------------------------------------------------------*
 ## Implementacija trakov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
 - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
 - `read`, ki vrne znak pod glavo;
 - `write`, ki pod glavo zapiše dani znak;
 - `move`, ki glavo premakne v dano smer;
 - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter
 pod njim z `^` označi mesto glave.

 Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita
 nespremenjenega.

 Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato
 poskrbite tako za učinkovitost kot za preglednost kode.
[*----------------------------------------------------------------------------*)

module type TAPE = sig
  type t

  val make : string -> t
  val move : direction -> t -> t
  val read : t -> char
  val write : char -> t -> t
  val print : t -> unit
end


module Tape : TAPE = struct
  type t = char list * char * char list

  let make (str : string) : t = 
    match String.fold_right (fun chr acc -> chr::acc) str [] with
    | [] -> ([], ' ', [])
    | h::t -> ([], h, t)
  let move direct ((l_list, head, r_list) : t) : t = 
    match direct with
    | Right ->
      begin match r_list with
      | [] -> (head::l_list, ' ', [])
      | h::t -> (head::l_list, h, t)
      end
    | Left ->
      match l_list with
      | [] -> ([], ' ', head::r_list)
      | h::t -> (t, h, head::r_list)

  let read ((_, head, _) : t) = head

  let write chr ((l_list, _, r_list) : t) : t = (l_list, chr, r_list)

  let print ((l_list, head, r_list) : t) = 
    let rec air_devourer = function
      | [] -> []
      | ' '::t -> air_devourer t
      | sez -> sez
    in
    let l_list', r_list' = l_list |> List.rev |> air_devourer |> List.rev, r_list |> List.rev |> air_devourer in
    let str1 = List.fold_left (fun acc chr -> (Char.escaped chr) ^ acc) "" l_list' ^ 
      Char.escaped head ^ 
      List.fold_left (fun acc chr -> (Char.escaped chr) ^ acc) "" r_list' in
    let str2 = String.make (List.length l_list') ' ' ^ "^"
    in
    print_endline str1;
    print_endline str2
end

let primer_trak = Tape.(
  make "ABCDE"
  |> move Left
  |> move Left
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> write '!'
  |> print
)
(*
AB!DE
  ^
*)
(* val primer_trak : unit = () *)

(*----------------------------------------------------------------------------*
 ## Implementacija Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip Turingovih strojev;
 - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih
 stanj ter prazno prehodno funkcijo;
 - `initial`, ki vrne začetno stanje stroja;
 - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto
 (q', a', d)$;
 - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.

 Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa
 naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko
 predpostavite, da ju bomo klicali le na poprej podanih stanjih.

 Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost
 kode.
[*----------------------------------------------------------------------------*)

module type MACHINE = sig
  type t
  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
end

module Machine : MACHINE = struct
  module CharKey = struct
    type t = char
    let compare (ch1 : t) (ch2 : t) = compare ch1 ch2
  end
  module StringKey = struct
    type t = state
    let compare (s1 : t) (s2 :t) =  compare s1 s2
  end
  module CharDict = Map.Make(CharKey)
  module StringDict = Map.Make(StringKey)
  type preslikave = ((char * state * direction) CharDict.t) StringDict.t
  type t = (state list) * state * preslikave

  let make state_0 other_states : t =
    let rec dict_maker dict = function
      | [] -> dict
      | h::t -> dict_maker (StringDict.add h CharDict.empty dict) t
    in
    ((state_0::other_states), state_0, dict_maker StringDict.empty (state_0::other_states))

    let add_transition (q1 : state) (ch1 : char) (q2 : state) (ch2 : char) (d : direction) ((states_dict, state_0, f) : t): t =
      let update key value = function
        | None -> failwith ""
        | Some preslikava -> Some (CharDict.add key value preslikava)
    in (states_dict, state_0, f |> StringDict.update q1 (update ch1 (ch2, q2, d)))

    let initial ((_, state_0, _) : t) = state_0

    let step ((states_dict, state_0, f) : t) (q : state) (tape : Tape.t) =
      match CharDict.find_opt (Tape.read tape) (StringDict.find q f) with
      | None -> None
      | Some (a', q', d) -> Some (q', tape |> Tape.write a' |> Tape.move d) 
end

module Machine3 : MACHINE = struct
  module Dict = struct
    type t = state * char
    let compare (a : t) (b : t) = compare a b
  end
  module Preslikave = Map.Make(Dict)
  type preslikave = (char * state * direction) Preslikave.t
  type t = (state list) * state * preslikave

  let make state_0 other_states : t = ((state_0::other_states), state_0, Preslikave.empty)
  let initial ((_, state_0, _) : t) = state_0
  let add_transition (q1 : state) (ch1 : char) (q2 : state) (ch2 : char) (d : direction) ((states_dict, state_0, f) : t): t =
    (states_dict, state_0, f |> Preslikave.add (q1, ch1) (ch2, q2, d))
  let step ((states_dict, state_0, f) : t) (q : state) (tape : Tape.t) =
    match  Preslikave.find_opt (q, (Tape.read tape)) f with
    | None -> None
    | Some (a', q', d) -> Some (q', tape |> Tape.write a' |> Tape.move d)
end

module Machine2 : MACHINE = struct
  module CharKey = struct
    type t = char
    let compare (ch1 : t) (ch2 : t) = compare ch1 ch2
  end
  module StringKey = struct
    type t = state
    let compare (s1 : t) (s2 :t) =  compare s1 s2
  end
  module States = Map.Make(StringKey)
  module Preslikave = Map.Make(CharKey)
  type preslikave = ((char * state * direction)) Preslikave.t
  type dict = preslikave States.t
  type t = dict * state * (dict -> char -> state -> (char * state * direction) option)

  let make state_0 other_states : t = 
    let rec dict_maker (sez : state list) (slov :dict) =
      match sez with
      | [] -> slov
      | h::t -> dict_maker t (States.add h Preslikave.empty slov)
    in let all_states = dict_maker (state_0::other_states) States.empty
    in 
    let f (state_dict : dict) ch1 q1 = 
      state_dict |> States.find q1 |> Preslikave.find_opt ch1
    in (all_states, state_0, f)
  let initial ((_, state_0, _) : t) = state_0
  let add_transition (q1 : state) (ch1 : char) (q2 : state) (ch2 : char) (d : direction) ((states_dict, state_0, f) : t): t = 
    let rec aux ch1 = function
      | None -> failwith" Transitiion was added at non-existant state"
      | Some preslikava -> Some (Preslikave.add ch1 (ch2, q2, d) preslikava)
    in
    let new_states = states_dict |> States.update q1 (aux ch1)
    in (new_states, state_0, f)
  let step ((states_tree, state_0, f) : t) (q : state) (tape : Tape.t) = 
    match f states_tree (Tape.read tape) q with
      | None -> None
      | Some (a', q', d) -> Some (q', tape |> Tape.write a' |> Tape.move d)
end

module Machine1 : MACHINE = struct
  type 'a bbs_tree = (* Balanced binary search tree *)
  | Empty
  | Node of 'a bbs_tree * 'a * 'a bbs_tree
  (* Množica stanj je implementirana kot bbs drevo parov. Pri element je ime stanja, drugi elemet je iskalno drevo definiranih preslikav v tem stanju.
  Z drugimi besedami lahko opišemo kot slovar slovarjev. *)
  type states_dict = (state * ((char * (char * state * direction)) bbs_tree)) bbs_tree
  type t = states_dict * state * (states_dict -> char -> state -> (char * state * direction) option)
  
  (* Pomožne funkcije *)

  let rec bbst_creator states =
    (* Zahteva že urejen seznam *)
    let n = (List.length states) / 2 in
    let rec split_list_n acc_n n (acc, a, sez) =
      match acc_n with
      | x when x = n -> (acc, a, sez)
      | x when x > n -> failwith "Index skipped over n"
      | x -> match sez with
        | [] -> failwith "Index out of range"
        | h::t -> split_list_n (acc_n + 1) n (a::acc, h, t)
    in
    match states with
      | [] -> Empty
      | h::t -> 
        let (list1, a, list2) = split_list_n 0 n ([], h, t) in
        Node (bbst_creator (List.rev list1), a, bbst_creator list2)

  let bbst_to_list bbs_tree = 
    (* Vrne urejen seznam urejenega drevesa*)
    let rec aux acc = function
    | Empty -> acc
    | Node (l, a, r) -> aux (a::aux acc r) l
  in aux [] bbs_tree

  let rec change_value key value = function
    | Empty -> failwith "Key not found; change_value"
    | Node (l, (k, v), r) ->
      match key with
      | key when key = k -> Node (l, (k, value), r)
      | key when key < k -> Node (change_value key value l, (k, v), r)
      | key when key > k -> Node (l, (k, v), change_value key value r)
      | _ -> failwith "Comparison in change_value failed"
  
  (* Glavne funkcije *)

  let make state_0 all_states = 
    if List.mem state_0 all_states then failwith "Initial state name is duplicated" else
    if let rec duplicate_finder = function
        | [] -> true
        | h::t -> if List.mem h t then false else duplicate_finder t
      in not (duplicate_finder all_states) then failwith "Duplicate state name found" else
    let sorted = List.sort compare (state_0::all_states) |> List.map (fun x -> (x, Empty)) in
    let states_tree = bbst_creator sorted  in
    (* Iskanje funkcije po drevesih je definirano v funkcije. Dodajanje prehoda bo spreminjalo drevo stanj.*)
    let f (states_tree : states_dict) (x : char) (current_state : state) : ((char * state * direction) option) =
      let rec f_sub (x : char) (tree : (char * (char * state * direction)) bbs_tree) =
        match tree with
        | Empty -> None
        | Node (l, (a, out), r) ->
          match x with
          | x when x = a -> Some out
          | x when x < a -> f_sub x l
          | x when x > a -> f_sub x r
          | _ -> failwith "Comparison in f_sub failed"
      in
      let rec f_main x q states =
        match states with
        | Empty -> failwith "Transition function preformed on non-existant state"
        | Node (l, (a, sub_tree), r) ->
          match q with
          | q when q = a -> f_sub x sub_tree
          | q when q < a -> f_main x q l
          | q when q > a -> f_main x q r
          | _ -> failwith "Comparison in f_main failed"
      in f_main x current_state states_tree
    in (states_tree, state_0, f)

  let initial ((_, state_0, _) : t) = state_0

  let add_transition (q1 : state) (ch1 : char) (q2 : state) (ch2 : char) (d : direction) ((states_tree, state_0, f) : t): t = 
    (* Ta funkcija je implementirana skoraj po navodilih. Namesto da so preslikave del prehodne funkcije so le te vrednosti v slovarju stanj, 
    ki so tudi sami slovarju. Prehodna funkcija le vrača te vrednosti oz. None, če stanje za določeno vrednost nima definirano preslikavo,
    torej se prehodna funkcija z dodajanjem preslikav ne spreminja. Spreminja se slovar slovarjev stanj. *)
    let states_tree' =
      let aux_sub (tree : (char * (char * state * direction)) bbs_tree) ch1 q2 ch2 d =
        let sez = bbst_to_list tree in
        if List.mem ch1 (List.map (fun (x, _) -> x ) sez) then change_value ch1 (ch2, q2, d) tree
        else (ch1, (ch2, q2, d))::sez |> List.sort compare |> bbst_creator
      in 
      let rec aux_main tree q1 ch1 q2 ch2 d =
        match tree with
        | Empty -> failwith ("Key " ^ q1 ^ " not found; aux_main")
        | Node (l, (k, v), r) ->
          match q1 with
          | key when key = k -> Node (l, (k, aux_sub v ch1 q2 ch2 d), r)
          | key when key < k -> Node (aux_main l q1 ch1 q2 ch2 d, (k, v), r)
          | key when key > k -> Node (l, (k, v), aux_main r q1 ch1 q2 ch2 d)
          | _ -> failwith "Comparison in aux_main failed"
        in aux_main states_tree q1 ch1 q2 ch2 d
    (* Muzejski artefakt *)
    in let f' states_tree ch q=
      match ch, q with
      | ch, q when ch = ch1 && q = q1 -> Some (ch2, q2, d)
      | ch, q -> f states_tree ch q 
    in (states_tree', state_0, f)
    
  let step ((states_tree, state_0, f) : t) (q : state) (tape : Tape.t) = 
    match f states_tree (Tape.read tape) q with
      | None -> None
      | Some (a', q', d) -> Some (q', tape |> Tape.write a' |> Tape.move d)
end

(*----------------------------------------------------------------------------*
 Primer stroja "Binary Increment" na <http://turingmachine.io> lahko
 implementiramo kot:
[*----------------------------------------------------------------------------*)

let binary_increment =
  Machine.(
    make "right" [ "carry"; "done" ]
    |> add_transition "right" '1' "right" '1' Right
    |> add_transition "right" '0' "right" '0' Right
    |> add_transition "right" ' ' "carry" ' ' Left
    |> add_transition "carry" '1' "carry" '0' Left
    |> add_transition "carry" ' ' "done" '1' Left
    |> add_transition "carry" '0' "done" '1' Left

  )

(* val binary_increment : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki
 simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz.
 Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše
 le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti
 izvajanja.
[*----------------------------------------------------------------------------*)

let slow_run makina (input : string) = 
  let tape = Tape.make input in
  let rec runner makina current tape =
    Tape.print tape ;
    match Machine.step makina current tape with
    | None -> ()
    | Some (state', tape') -> runner makina state' tape'
  in runner makina (Machine.initial makina) tape

(* let primer_slow_run =
  slow_run binary_increment "1011" *)
(*
1011
^
right
1011
  ^
right
1011
  ^
right
1011
    ^
right
1011
    ^
right
1011
    ^
carry
1010
  ^
carry
1000
  ^
carry
1100
^
done
*)
(* val primer_slow_run : unit = () *)

let speed_run makina (input : string) = 
  let tape = Tape.make input in
  let rec runner makina current tape =
    match Machine.step makina current tape with
    | None -> Tape.print tape
    | Some (state', tape') -> runner makina state' tape'
  in runner makina (Machine.initial makina) tape

let primer_speed_run =
  speed_run binary_increment "1011"
(*
1100
^
*)
(* val primer_speed_run : unit = () *)

(*----------------------------------------------------------------------------*
 ## Krajši zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih,
 nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak
 in stanje pa pustimo pri miru. Zapišite funkcije:

 - `for_state`
 - `for_character`
 - `for_characters`
 - `move`
 - `switch_and_move`
 - `write_and_move`
 - `write_switch_and_move`

 s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj.
 Implementacijo in tipe ugotovite sami.
[*----------------------------------------------------------------------------*)

(* PROBLEM *)

let for_state state transition_lists_list makina =
  let transition_list = List.concat transition_lists_list in
  let rec transition_adder state transition_list makina =
    match transition_list with
    | [] -> makina
    | (ch1, q2, ch2, d)::l -> 
      match q2, ch2 with
      | Some q, Some ch -> transition_adder state l (Machine.add_transition state ch1 q ch d makina)
      | Some q, None -> transition_adder state l (Machine.add_transition state ch1 q ch1 d makina)
      | None, Some ch -> transition_adder state l (Machine.add_transition state ch1 state ch d makina)
      | None, None -> transition_adder state l (Machine.add_transition state ch1 state ch1 d makina)
  in transition_adder state transition_list makina

let for_character ch (q2, ch2, d) = [(ch, q2, ch2, d)]
let for_characters string (q2, ch2, d) = 
  let f (q2, ch2, d) acc ch = (ch, q2, ch2, d)::acc in
  String.fold_left (f (q2, ch2, d)) [] string

let move d = (None, None, d)
let switch_and_move state d = (Some state, None, d)
let write_and_move ch d = (None, Some ch, d)
let write_switch_and_move ch state d = (Some state, Some ch, d)
    

let binary_increment' =
  Machine.make "right" ["carry"; "done"]
  |> for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "carry" Left
  ]
  |> for_state "carry" [
    for_character '1' @@ write_switch_and_move '0' "carry" Left;
    for_characters "0 " @@ write_switch_and_move '1' "done" Left
  ]   
(* val binary_increment' : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 ## Primeri Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na
 različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni
 niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu
 izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo
 trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku,
 kot se je začel prvotni niz.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Obračanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki začetni niz obrne na glavo.
[*----------------------------------------------------------------------------*)

let reverse = 
  Machine.make "check_length" ["length"; "first"; "start"; "check0"; "gap0"; "paste0"; "check1"; "gap1"; "paste1";
                               "return"; "final0"; "final_paste0"; "final1"; "final_paste1"; "end"; "done"]
  |> for_state "check_length" [
    for_characters "01" @@ switch_and_move "length" Right;
    for_character ' ' @@ move Right
  ]
  |> for_state "length" [
    for_characters "01" @@ switch_and_move "first" Left;
    for_character ' ' @@ switch_and_move "donr" Left
  ]
  |> for_state "first" [
    for_character ' ' @@ move Right;
    for_character '1' @@ write_switch_and_move ' ' "paste1" Left;
    for_character '0' @@ write_switch_and_move ' ' "paste0" Left
  ]

  |> for_state "start" [
    for_character ' ' @@ move Right;
    for_character '1' @@ write_switch_and_move ' ' "check1" Right;
    for_character '0' @@ write_switch_and_move ' ' "check0" Right
  ]
  |> for_state "check0" [
    for_characters "01" @@ switch_and_move "gap0" Left;
    for_character ' ' @@ switch_and_move "final0" Left
  ]
  |> for_state "gap0" [
    for_characters "01" @@ switch_and_move "paste0" Left;
    for_character ' ' @@ move Left
  ]
  |> for_state "paste0" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ write_switch_and_move '0' "return" Right
  ]
  |> for_state "check1" [
    for_characters "01" @@ switch_and_move "gap1" Left;
    for_character ' ' @@ switch_and_move "final1" Left
  ]
  |> for_state "gap1" [
    for_characters "01" @@ switch_and_move "paste1" Left;
    for_character ' ' @@ move Left
  ]
  |> for_state "paste1" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ write_switch_and_move '1' "return" Right
  ]
  |> for_state "return" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "start" Right
  ]

  |> for_state "final0" [
    for_characters "01" @@ switch_and_move "final_paste0" Left;
    for_character ' ' @@ move Left
  ]
  |> for_state "final_paste0" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ write_switch_and_move '0' "end" Right
  ]
  |> for_state "final1" [
    for_characters "01" @@ switch_and_move "final_paste1" Left;
    for_character ' ' @@ move Left
  ]
  |> for_state "final_paste1" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ write_switch_and_move '1' "end" Right
  ]
  |> for_state "end" [
    for_characters "01" @@ switch_and_move "done" Left;
  ]
let primer_reverse = speed_run reverse "0000111001"
(* 
1001110000          
^
*)
(* val primer_reverse : unit = () *)

(*----------------------------------------------------------------------------*
 ### Podvajanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki podvoji začetni niz.
[*----------------------------------------------------------------------------*)

let duplicate =
  Machine.make "start" ["pusher"; "push0"; "push1"; "gap0"; "paste0a"; "paste0b"; "gap1"; "paste1a";
                        "paste1b"; "to_push"; "push_gap"; "back"; "end"; "finish"; "done"]
  |> for_state "start" [
    for_character ' ' @@ move Right;
    for_character '1' @@ switch_and_move "gap1" Left;
    for_character '0' @@ switch_and_move "gap0" Left
  ]
  |> for_state "pusher" [
    for_character ' ' @@ switch_and_move "end" Left;
    for_character '1' @@ write_switch_and_move ' ' "push1" Right;
    for_character '0' @@ write_switch_and_move ' ' "push0" Right
  ]
  |> for_state "push1" [
    for_character ' ' @@ write_switch_and_move '1' "back" Left;
    for_character '1' @@ move Right;
    for_character '0' @@ write_switch_and_move '1' "push0" Right
  ]
  |> for_state "push0" [
    for_character ' ' @@ write_switch_and_move '0' "back" Left;
    for_character '0' @@ move Right;
    for_character '1' @@ write_switch_and_move '0' "push1" Right
  ]

  |> for_state "gap0" [
    for_character ' ' @@ switch_and_move "paste0a" Left
  ]
  |> for_state "paste0a" [
    for_character ' ' @@ write_switch_and_move '0' "paste0b" Left
  ]
  |> for_state "paste0b" [
    for_character ' ' @@ write_switch_and_move '0' "to_push" Right
  ]
  |> for_state "gap1" [
    for_character ' ' @@ switch_and_move "paste1a" Left
  ]
  |> for_state "paste1a" [
    for_character ' ' @@ write_switch_and_move '1' "paste1b" Left
  ]
  |> for_state "paste1b" [
    for_character ' ' @@ write_switch_and_move '1' "to_push" Right
  ]

  |> for_state "to_push" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "push_gap" Right
  ]
  |> for_state "push_gap" [
    for_characters "01" @@ write_switch_and_move ' ' "pusher" Right
  ]
  |> for_state "back" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ switch_and_move "start" Right
  ]
  |> for_state "end" [
    for_characters "01" @@ switch_and_move "finish" Left;
    for_character ' ' @@ move Left
  ]
  |> for_state "finish" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ switch_and_move "done" Right
  ]

let primer_duplicate = speed_run duplicate "010011"
(* 
001100001111       
^
*)
(* val primer_duplicate : unit = () *)

(*----------------------------------------------------------------------------*
 ### Eniški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano
 v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.
[*----------------------------------------------------------------------------*)

let to_unary = 
  Machine.make "start" ["minus"; "backfill"; "add"; "return"; "finish"; "done"]
  |> for_state "start" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "minus" Left
  ]
  |> for_state "minus" [
    for_character ' ' @@ switch_and_move "finish" Right;
    for_character '0' @@ move Left;
    for_character '1' @@ write_switch_and_move '0' "backfill" Right
  ]
  |> for_state "backfill" [
    for_character '0' @@ write_switch_and_move '1' "backfill" Right;
    for_character ' ' @@ switch_and_move "add" Right
  ]
  |> for_state "add" [
    for_character ' ' @@ write_switch_and_move '1' "return" Left;
    for_character '1' @@ move Right
  ]
  |> for_state "return" [
    for_character ' ' @@ switch_and_move "minus" Left;
    for_character '1' @@ move Left
  ]
  |> for_state "finish" [
    for_character ' ' @@ switch_and_move "done" Right;
    for_character '0' @@ write_and_move ' ' Right
  ]

let primer_to_unary = speed_run to_unary "1010"
(* 
1111111111
^
*)
(* val primer_to_unary : unit = () *)

(*----------------------------------------------------------------------------*
 ### Dvojiški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
 sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v
 dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let to_binary = 
  Machine.make "start" ["seed"; "gap"; "right"; "minus"; "add"; "carry"; "return"; "finish"; "done"]
  |> for_state "start" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ switch_and_move "seed" Left
  ]
  |> for_state "seed" [
    for_character ' ' @@ write_switch_and_move '0' "gap" Right
  ]
  |> for_state "gap" [
    for_character ' ' @@ switch_and_move "right" Right
  ]
  |> for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "minus" Left
  ]
  |> for_state "minus" [
    for_character ' ' @@ switch_and_move "finish" Left;
    for_character '1' @@ write_switch_and_move ' ' "add" Left
  ]
  |> for_state "add" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ switch_and_move "carry" Left
  ]
  |> for_state "carry" [
    for_characters "0 " @@ write_switch_and_move '1' "return" Right;
    for_character '1' @@ write_and_move '0' Left
  ]
  |> for_state "return" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "right" Right
  ]
  |> for_state "finish" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ switch_and_move "done" Right
  ]


let primer_to_binary = speed_run to_binary (String.make 42 '1')
(* 
101010                                           
^
*)
(* val primer_to_binary : unit = () *) 

let busy_beaver5 =
  Machine.(make "A" ["B"; "C"; "D"; "E"]
  |> add_transition "A" ' ' "B" '1' Right
  |> add_transition "A" '1' "C" '1' Left
  |> add_transition "B" ' ' "C" '1' Right
  |> add_transition "B" '1' "B" '1' Right
  |> add_transition "C" ' ' "D" '1' Right
  |> add_transition "C" '1' "E" ' ' Left
  |> add_transition "D" ' ' "A" '1' Left
  |> add_transition "D" '1' "D" '1' Left
  |> add_transition "E" '1' "A" ' ' Left
)

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

let x = time (speed_run busy_beaver5) ""