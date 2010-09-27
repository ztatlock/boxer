(* TODO
 *
 * How to set up input?
 *
 *)

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
    | None   -> List.rev ls
    | Some l -> loop (l::ls)
  in
  let ls = loop [] in
  close_in f;
  ls

let write_str file s =
  let f = open_out file in
  output_string f s;
  close_out f

let writelines file ss =
  ss |> String.concat "\n"
     |> write_str file

(* split list into first i elements and the rest *)
let split i l =
  if i < 0 then
    invalid_arg "split negative"
  else
    let rec loop hs i l =
      match i, l with
      | 0, _    -> (List.rev hs, l)
      | _, h::t -> loop (h::hs) (i-1) t
      | _, _    -> failwith "split > list length"
    in
    loop [] i l

(* remove first i elements of l *)
let leave i l =
  split i l |> fst

(* keep only first i elements of l *)
let take i l =
  split i l |> snd

let chars s =
  let cs = ref [] in
  String.iter (fun c -> cs := c::!cs) s;
  List.rev !cs

let args () =
  Sys.argv |> Array.to_list
           |> List.tl

(* expose nice names for big ints *)

type big     = Big_int.big_int
let  big_0   = Big_int.zero_big_int
let  big_1   = Big_int.unit_big_int
let  big_eq  = Big_int.eq_big_int
let  big_lt  = Big_int.lt_big_int
let  big_neg = Big_int.minus_big_int
let  big_add = Big_int.add_big_int
let  big_sub = Big_int.sub_big_int
let  big_mul = Big_int.mult_big_int
let  big_div = Big_int.div_big_int
let  big_str = Big_int.string_of_big_int
let  int_big = Big_int.big_int_of_int

let bool_big = function
  | false -> big_0
  | true  -> big_1

let big_true b =
  not (big_eq b big_0)

let bigs_str bigs =
  bigs |> List.map big_str |> String.concat ", "

(* structures to represent and execute card decks *)

type bit =
  | O (* off *)
  | X (* on *)

let char_bit = function
  | '-' -> O
  | '1' -> X
  | _ as c ->
      failwith (mkstr "'%c' not a bit" c)

let bit_str = function
  | O -> "-"
  | X -> "1"

let bit_int = function
  | O -> 0
  | X -> 1

let bits_int bs =
  List.fold_left
    (fun acc b -> 2 * acc + (bit_int b))
    0
    bs

let bits_big bs =
  bs |> bits_int |> int_big
 
type instr = (* 10 bits *)
  (bit * bit * bit * bit * bit * bit * bit * bit * bit * bit)

let ibits = function
  (b9, b8, b7, b6, b5, b4, b3, b2, b1, b0) ->
  [b9; b8; b7; b6; b5; b4; b3; b2; b1; b0]

let bitsi = function
  | [b9; b8; b7; b6; b5; b4; b3; b2; b1; b0] ->
    (b9, b8, b7, b6, b5, b4, b3, b2, b1, b0)
  | _ -> invalid_arg "wrong # of bits"

(* return bits between index l and r inclusive *)
let iproj l r instr =
  instr |> ibits
        |> leave (9 - l)
        |> take  (l - r)

let str_instr s : instr =
  s |> chars
    |> List.map char_bit
    |> bitsi

let instr_str i =
  i |> ibits
    |> List.map bit_str
    |> String.concat ""

type deck = instr list

let deck_str d =
  d |> List.map instr_str
    |> String.concat "\n"

type state =
  { mutable r0  : big (* 8 registers *)
  ; mutable r1  : big
  ; mutable r2  : big
  ; mutable r3  : big
  ; mutable r4  : big
  ; mutable r5  : big
  ; mutable r6  : big
  ; mutable r7  : big
  ; mutable acc : big  (* accumulator *)
  ; mutable pc  : int  (* program counter *)
  ; mutable ctr : int  (* instruction counter *)
  ; mutable fin : bool (* has machine halted *)
  ; mutable inp : big list (* input *)
  ; mutable out : big list (* output *)
  }

let init_state =
  { r0  = big_0
  ; r1  = big_0
  ; r2  = big_0
  ; r3  = big_0
  ; r4  = big_0
  ; r5  = big_0
  ; r6  = big_0
  ; r7  = big_0
  ; acc = big_0
  ; pc  = 0
  ; ctr = 0
  ; fin = false
  ; inp = []
  ; out = []
  }

let get_reg st r =
  match r with
  | 0 -> st.r0
  | 1 -> st.r1
  | 2 -> st.r2
  | 3 -> st.r3
  | 4 -> st.r4
  | 5 -> st.r5
  | 6 -> st.r6
  | 7 -> st.r7
  | _ -> invalid_arg (mkstr "get_reg: invalid index '%d'" r)

let set_reg st r v =
  match r with
  | 0 -> st.r0 <- v
  | 1 -> st.r1 <- v
  | 2 -> st.r2 <- v
  | 3 -> st.r3 <- v
  | 4 -> st.r4 <- v
  | 5 -> st.r5 <- v
  | 6 -> st.r6 <- v
  | 7 -> st.r7 <- v
  | _ -> invalid_arg (mkstr "set_reg: invalid index '%d'" r)

let state_str s =
  String.concat "\n"
    [ mkstr "r0  : %s" (big_str  s.r0)
    ; mkstr "r1  : %s" (big_str  s.r1)
    ; mkstr "r2  : %s" (big_str  s.r2)
    ; mkstr "r3  : %s" (big_str  s.r3)
    ; mkstr "r4  : %s" (big_str  s.r4)
    ; mkstr "r5  : %s" (big_str  s.r5)
    ; mkstr "r6  : %s" (big_str  s.r6)
    ; mkstr "r7  : %s" (big_str  s.r7)
    ; mkstr "acc : %s" (big_str  s.acc)
    ; mkstr "pc  : %d" s.pc
    ; mkstr "ctr : %d" s.ctr
    ; mkstr "fin : %b" s.fin
    ; mkstr "inp : %s" (bigs_str s.inp)
    ; mkstr "out : %s" (bigs_str s.out)
    ]

(* the real show *)

let input_deck () : deck =
  () |> args
     |> List.map readlines
     |> List.flatten
     |> List.map str_instr

let do_op op l r =
  match op with
  | 0 -> big_add l r
  | 1 -> big_sub l r
  | 2 -> big_mul l r
  | 3 -> big_div l r
  | 4 -> big_eq  l r |> bool_big
  | 5 -> big_lt  l r |> bool_big
  | 6 -> ((big_true l) || (big_true r)) |> bool_big
  | 7 -> big_true l |> not |> bool_big
  | _ -> invalid_arg (mkstr "bad opcode '%d'" op)

let step st instr : unit =
  st.pc  <- st.pc  + 1;
  st.ctr <- st.ctr + 1;
  match instr with
  (* nop *)
  | (O, O, O, O, O, O, O, O, O, O) ->
      ()
  (* halt *)
  | (O, O, O, O, O, O, O, O, O, X) ->
      st.fin <- true
  (* A <= input() *)
  | (O, O, O, O, O, O, O, O, X, O) ->
      begin match st.inp with
      | []   -> st.acc <- big_0
      | h::t -> st.acc <- h; st.inp <- t
      end
  (* output(A) *)
  | (O, O, O, O, O, O, O, O, X, X) ->
      st.out <- st.acc :: st.out
  (* TODO take an arg to handle this case *)
  | (O, O, O, O, O, O, O, X, _, _) ->
      ()
  (* A <= -const *)
  | (O, O, O, O, O, O, X, _, _, _) ->
      st.acc <- instr |> iproj 2 0
                      |> bits_big
                      |> big_neg
  (* A <= r *)
  | (O, O, O, O, O, X, O, _, _, _) ->
      st.acc <- instr |> iproj 2 0
                      |> bits_int
                      |> get_reg st
  (* r <= A *)
  | (O, O, O, O, O, X, X, _, _, _) ->
      let r = instr |> iproj 2 0
                    |> bits_int in
      set_reg st r st.acc
  (* A <= const *)
  | (O, O, O, O, X, _, _, _, _, _) ->
      st.acc <- instr |> iproj 4 0
                      |> bits_big
  (* A <= A op r *)
  | (O, O, O, X, _, _, _, _, _, _) ->
      let op = instr |> iproj 5 3 |> bits_int in
      let r  = instr |> iproj 2 0 |> bits_int |> get_reg st in
      st.acc <- do_op op st.acc r
  (* goto const *)
  | (O, O, X, _, _, _, _, _, _, _) ->
      st.pc <- instr |> iproj 6 0
                     |> bits_int
  (* goto + *)
  | (O, X, O, O, _, _, _, _, _, _) ->
      st.pc <- instr |> iproj 5 0
                     |> bits_int
                     |> (+) st.pc
  (* goto - *)
  | (O, X, O, X, _, _, _, _, _, _) ->
      st.pc <- instr |> iproj 5 0
                     |> bits_int
                     |> (-) st.pc
  (* branch + *)
  | (O, X, X, O, _, _, _, _, _, _) ->
      if big_true st.acc then
        st.pc <- instr |> iproj 5 0
                       |> bits_int
                       |> (+) st.pc
  (* branch - *)
  | (O, X, X, X, _, _, _, _, _, _) ->
      if big_true st.acc then
        st.pc <- instr |> iproj 5 0
                       |> bits_int
                       |> (-) st.pc
  (* A <= l op r *)
  | (X, _, _, _, _, _, _, _, _, _) ->
      let op = instr |> iproj 8 6 |> bits_int in
      let l  = instr |> iproj 5 3 |> bits_int |> get_reg st in
      let r  = instr |> iproj 2 0 |> bits_int |> get_reg st in
      st.acc <- do_op op l r

let lkup_instr deck pc : instr =
  try List.nth deck pc
  with _ -> failwith (mkstr "no instr at pc '%d'" pc)

let step_star deck : string =
  let st = init_state in
  while not st.fin do
    st.pc |> lkup_instr deck
          |> step st
  done;
  bigs_str st.out

let main () =
  () |> input_deck
     |> step_star
     |> print_string

let _ = main ()

