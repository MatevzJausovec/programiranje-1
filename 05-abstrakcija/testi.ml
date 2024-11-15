module type DATUM =
  sig
    type t
    val naredi : int -> int -> int -> t option
    val to_string : t -> string
  end

  module Datum : DATUM = struct
    type t = { dan : int; mesec : int; leto : int }
  
    let je_prestopno leto =
      (leto mod 4 = 0 && leto mod 100 <> 0) || leto mod 400 = 0
      
    let dolzina_meseca leto =
      function
      | 4 | 6 | 9 | 11 -> 30
      | 2 -> if je_prestopno leto then 29 else 28
      | _ -> 31
  
    let je_veljaven datum =
      let veljaven_dan = 1 <= datum.dan && datum.dan <= dolzina_meseca datum.leto datum.mesec
      and veljaven_mesec = 1 <= datum.mesec && datum.mesec <= 12
      in
      veljaven_dan && veljaven_mesec
  
    let naredi dan mesec leto =
      let datum = { dan; mesec; leto } in
      if je_veljaven datum then Some datum else None
  
    let to_string { dan; mesec; leto } =
      Format.sprintf "%04d-%02d-%02d" leto mesec dan
  end