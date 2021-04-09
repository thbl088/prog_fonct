type token =
  | Add    (* Binary operator for addition (+) *)
  | Subtract    (* Binary operator for subtraction (-) *)
  | Multiply    (* Binary operator for multiplication [*] *)
  | Divide    (* Binary operator for division (/) *)
  | Minus    (* Unary operator for opposite (~) *)
  | Variable of char    (* Variable names are lowercase unaccented letters *)
  | Number of int    (* Only nonnegative integer numbers *)
  | End    (* End of expression (;) *)

val string_of_token : token -> string

val print_token : token -> unit

val print_token_list : token list -> unit
  
val input_to_token_list : unit -> token list

val string_to_token_list : string -> token list

type input_flow
   
val initialize_scanner : unit -> input_flow
val input_to_token_list2 : input_flow -> token list

exception Lexical_Error of string
