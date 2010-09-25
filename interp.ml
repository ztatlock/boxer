
(* expose nice names for big ints *)

module I = Big_int
type bigi = I.big_int
let bigi_0 = I.zero_big_int
let bigi_str = I.string_of_big_int

(* structures to represent and execute card decks *)

type bit =
  | O (* off *)
  | X (* on *)

type instr = (* 10 bits *)
  (bit * bit * bit * bit * bit * bit * bit * bit * bit * bit)

type deck = instr list

type state =
  { r0   : bigi (* 8 registers *)
  ; r1   : bigi
  ; r2   : bigi
  ; r3   : bigi
  ; r4   : bigi
  ; r5   : bigi
  ; r6   : bigi
  ; r7   : bigi
  ; acc  : bigi (* accumulator *)
  ; pc   : int  (* program counter *)
  ; ictr : int  (* instruction counter *)
  ; out  : bigi list (* output *)
  ; fin  : bool (* has machine halted *)
  }

(* auxillary definitions *)

let (|>) x f = f x

let mkstr = Printf.sprintf

let readlines file =
  let f = open_in file in
  let next () =
    try Some (input_line f)
    with End_of_file -> None
  in
  let rec loop ls =
    match next () with
    | Some l -> loop (l::ls)
    | None   -> List.rev ls
  in
  let ls = loop [] in
  close_in f;
  ls

let write_str file s =
  let f = open_out file in
  output_string f s;
  close_out f

let write_strs file ss =
  ss |> String.concat "\n"
     |> write_str file

let chars s =
  let cs = ref [] in
  String.iter (fun c -> cs := c::!cs) s;
  List.rev !cs

let args () =
  Sys.argv |> Array.to_list
           |> List.tl

(* string representations *)

let bit_str = function
  | O -> "O"
  | X -> "X"

let instr_str = function
  | (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9) ->
      [b0; b1; b2; b3; b4; b5; b6; b7; b8; b9]
        |> List.map bit_str
        |> String.concat ""

let deck_str d =
  d |> List.map instr_str
    |> String.concat "\n"

let state_str s =
  String.concat "\n"
    [ mkstr "r0   : %s" (bigi_str s.r0)
    ; mkstr "r1   : %s" (bigi_str s.r1)
    ; mkstr "r2   : %s" (bigi_str s.r2)
    ; mkstr "r3   : %s" (bigi_str s.r3)
    ; mkstr "r4   : %s" (bigi_str s.r4)
    ; mkstr "r5   : %s" (bigi_str s.r5)
    ; mkstr "r6   : %s" (bigi_str s.r6)
    ; mkstr "r7   : %s" (bigi_str s.r7)
    ; mkstr "acc  : %s" (bigi_str s.acc)
    ; mkstr "pc   : %d" s.pc
    ; mkstr "ictr : %d" s.ictr
    ]

(* the real show *)

let char_bit = function
  | '-' -> O
  | '1' -> X
  | _ as c ->
      failwith (mkstr "'%c' not a bit" c)

let line_instr (l: string) : instr =
  l |> chars
    |> List.map char_bit
    |> (function
        | [b0; b1; b2; b3; b4; b5; b6; b7; b8; b9] -> 
            (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9)
        | _ ->
            failwith (mkstr "'%s' wrong # of bits" l))

let read_deck () : deck =
  () |> args
     |> List.map readlines
     |> List.flatten
     |> List.map line_instr

let init_state =
  { r0   = bigi_0
  ; r1   = bigi_0
  ; r2   = bigi_0
  ; r3   = bigi_0
  ; r4   = bigi_0
  ; r5   = bigi_0
  ; r6   = bigi_0
  ; r7   = bigi_0
  ; acc  = bigi_0
  ; pc   = 0
  ; ictr = 0
  ; out  = []
  ; fin  = false
  }

let step st = function
  | (O, O, O, O, O, O, O, O, O, O) -> (* nop *)
      { st with pc = st.pc + 1 }
  | (O, O, O, O, O, O, O, O, O, X) -> (* halt *)
      { st with fin = true }
  | (O, O, O, O, O, O, O, O, X, O) -> (* A <= input() *)
      (* TODO *)
      { st with acc = bigi_0 }
  | (O, O, O, O, O, O, O, O, X, X) -> (* output(A) *)
      { st with out = st.acc :: st.out }
  (* HERE *)
  | _ -> st

let lkup_instr (d: deck) (pc: int) : instr =
  try List.nth d pc
  with _ -> failwith (mkstr "no instr at pc '%d'" pc)

let step_star (d: deck) : string =
  let rec loop st =
    if st.fin then
      st.out |> List.map bigi_str
             |> String.concat "\n"
    else
      st.pc |> lkup_instr d
            |> step st
            |> loop
  in
  loop init_state

let main () =
  () |> read_deck
     |> step_star
     |> print_string

let _ = main ()

