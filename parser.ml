open SmallCTypes
open Utils
open List

type stmt_result = token list * stmt
type expr_result = token list * expr

(* Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

let lookahead toks = 
  match toks with
    [] -> failwith "No Tokens"
  | (h::t) -> h

(* Grammar:
 A -> B || A | B
 B -> C && B | C
 C -> D == C | D != C | D
 D -> E > D | E < D | E >= D| E <= D | E
 E -> F + E | F - E | F
 F -> G * F| G / F | G
 G -> H ^ G | H
 H -> !I | I
 I -> Tok_Int | Tok_Bool | Tok_Id | (A) *)
let rec parse_expr toks = parse_A toks

and parse_A toks = 
  let (o,e1) = parse_B toks in
  let t = lookahead o in 
  begin
  match t with Tok_Or -> let tail = match_token o Tok_Or in let (r,e2) = parse_A tail in (r,Or(e1,e2))
               |_ -> (o,e1)
  end
  
and parse_B l = 
  let (o,e1) = parse_C l in
  let t = lookahead o in
  begin
  match t with Tok_And -> let tail = match_token o Tok_And in let (r,e2) = parse_B tail in (r,And(e1,e2))
               | _ -> (o,e1)
  end
  
and parse_C l =
  let (o,e1) = parse_D l in
  let t = lookahead o in
  begin
  match t with Tok_Equal -> let tail = match_token o Tok_Equal in let (r,e2) = parse_C tail in (r,Equal(e1,e2))
              |Tok_NotEqual -> let tail = match_token o Tok_NotEqual in let (r,e2) = parse_C tail in (r,NotEqual(e1,e2))
              |_ -> (o,e1)
  end
  
and parse_D l =
  let (o,e1) = parse_E l in
  let t = lookahead o in
  begin
  match t with Tok_Greater -> let tail = match_token o Tok_Greater in let (r,e2) = parse_D tail in (r,Greater(e1,e2))
              |Tok_GreaterEqual-> let tail = match_token o Tok_GreaterEqual in let (r,e2) = parse_D tail in (r,GreaterEqual(e1,e2))
              |Tok_Less -> let tail = match_token o Tok_Less in let (r,e2) = parse_D tail in (r,Less(e1,e2))
              |Tok_LessEqual -> let tail = match_token o Tok_LessEqual in let (r,e2) = parse_D tail in (r,LessEqual(e1,e2))
              |_ -> (o,e1)
  end
  
and parse_E l = 
  let (o,e1) = parse_F l in
  let t = lookahead o in
  begin
  match t with Tok_Sub -> let tail = match_token o Tok_Sub in let (r,e2) = parse_E tail in (r,Sub(e1,e2))
              |Tok_Plus -> let tail = match_token o Tok_Plus in let (r,e2) = parse_E tail in (r,Plus(e1,e2))
              |_ -> (o,e1)
  end
  
and parse_F l = 
  let (o,e1) = parse_G l in
  let t = lookahead o in
  begin
  match t with Tok_Mult -> let tail = match_token o Tok_Mult in let (r,e2) = parse_F tail in (r,Mult(e1,e2))
              |Tok_Div -> let tail = match_token o Tok_Div in let (r,e2) = parse_F tail in (r,Div(e1,e2))
              |_ -> (o,e1)
  end
  
and parse_G l = 
  let (o,e1) = parse_H l in
  let t = lookahead o in
  begin
  match t with Tok_Pow -> let tail = match_token o Tok_Pow in let (r,e2) = parse_G tail in (r,Pow(e1,e2))
              |_ -> (o,e1)
  end  

and parse_H l = 
  let (o,e1) = parse_I l in
  let t = lookahead o in
  begin
  match t with Tok_Not -> let tail = match_token o Tok_Not in (tail,Not(e1))
              |_ -> (o,e1)
  end  

and parse_I l = 
  let t = lookahead l in
  begin
  match t with Tok_Int(i) -> let tail = match_token l (Tok_Int(i)) in (tail,Int(i))
              |Tok_Bool(b) -> let tail = match_token l (Tok_Bool(b)) in (tail,Bool(b))
              |Tok_ID(s) -> let tail = match_token l (Tok_ID(s)) in (tail,Id(s))
              |Tok_LParen -> let tail = match_token l Tok_LParen in let (r,e1) = parse_A tail in let t2 = lookahead r in
              begin
              match t2 with Tok_RParen -> let t3 = match_token r Tok_RParen in (t3,e1)
                            |_ -> raise (InvalidInputException "Unmatched parenthesis")
              end
              | _ -> raise (InvalidInputException "stop")
  end
           

(*
 stmt -> dstmt
 dstmt -> astmt
 astmt -> pstmt*)

let rec parse_stmt toks = let t = lookahead toks in 
                          begin
                          match t with
                            | Tok_Type_Int -> let (r1,s1) = parse_dstmt toks in let (r2,s2) = parse_stmt r1 in (r2,Seq(s1,s2))
                            | Tok_ID(s) -> let (r1,s1) = parse_astmt toks in let (r2,s2) = parse_stmt r1 in (r2,Seq(s1,s2))
                            | Tok_Print -> let (r1,s1) = parse_pstmt toks in let (r2,s2) = parse_stmt r1 in (r2,Seq(s1,s2))
                            | Tok_If -> let tail = match_token toks Tok_If in let (r1,s1) = parse_ifstmt tail in let (r2,s2) = parse_stmt r1 in (r2,Seq(s1,s2))
                            | Tok_While -> let tail = match_token toks Tok_While in let (r1,s1) = parse_whilestmt tail in let (r2,s2) = parse_stmt r1 in (r2,Seq(s1,s2))
                            | Tok_RBrace -> let tail = match_token toks Tok_RBrace in (tail,NoOp)
                            | _ -> raise (InvalidInputException "no")
                          end 
                          
 and parse_body toks = let t = lookahead toks in 
                          begin
                          match t with
                            | Tok_Type_Int -> let (r1,s1) = parse_dstmt toks in let (r2,s2) = parse_body r1 in (r2,Seq(s1,s2))
                            | Tok_ID(s) -> let (r1,s1) = parse_astmt toks in let (r2,s2) = parse_body r1 in (r2,Seq(s1,s2))
                            | Tok_Print -> let (r1,s1) = parse_pstmt toks in let (r2,s2) = parse_body r1 in (r2,Seq(s1,s2))
                            | Tok_If -> let tail = match_token toks Tok_If in let (r1,s1) = parse_ifstmt tail in let (r2,s2) = parse_body r1 in (r2,Seq(s1,s2))
                            | Tok_While -> let tail = match_token toks Tok_While in let (r1,s1) = parse_whilestmt tail in let (r2,s2) = parse_body r1 in (r2,Seq(s1,s2))
                            | _ -> (toks,NoOp)
                          end 

and parse_dstmt l = let t = lookahead l in 
                          begin
                          match t with 
                            | Tok_Type_Int -> let tail = match_token l Tok_Type_Int in let t2 = lookahead tail in
                            begin
                            match t2 with
                              | Tok_ID(s) -> let t3 = match_token tail (Tok_ID(s)) in let t4 = lookahead t3 in 
                              begin
                              match t4 with 
                                | Tok_Semi -> let t5 = match_token t3 Tok_Semi in (t5,Declare(Type_Int, s))
                                | _ -> raise (InvalidInputException ";")
                              end
                              | _ -> raise (InvalidInputException "Declare")
                            end
                            | Tok_Type_Bool-> let tail = match_token l Tok_Type_Bool in let t2 = lookahead tail in
                            begin
                            match t2 with
                              | Tok_ID(s) -> let t3 = match_token tail (Tok_ID(s)) in let t4 = lookahead t3 in 
                              begin
                              match t4 with 
                                | Tok_Semi -> let t5 = match_token t3 Tok_Semi in (t5,Declare(Type_Bool, s))
                                | _ -> raise (InvalidInputException ";")
                              end
                              | _ -> raise (InvalidInputException "Declare")
                            end
                            | _ -> raise (InvalidInputException "Declare")
                          end
                          
and parse_astmt l = let t = lookahead l in 
                          begin
                          match t with 
                            | Tok_ID(id) -> let tail = match_token l (Tok_ID(id)) in let t2 = lookahead tail in
                            begin
                            match t2 with
                              | Tok_Assign -> let t3 = match_token tail Tok_Assign in let (t4,e) = parse_A t3 in 
                              let t5 = lookahead t4 in 
                              begin
                              match t5 with
                                |Tok_Semi -> let t6 = match_token t4 Tok_Semi in (t6,Assign((id),e))
                                | _ -> raise (InvalidInputException ";")
                              end
                              | _ -> raise (InvalidInputException "Assign")
                            end
                            | _ -> raise (InvalidInputException "Assign")
                          end
                          
and parse_pstmt l = let t = lookahead l in 
                          begin
                          match t with 
                            | Tok_Print -> let tail = match_token l Tok_Print in
                               let (t2,e) = parse_expr tail in let t3 = lookahead t2 in
                               begin
                               match t3 with
                                 | Tok_Semi -> let t4 = match_token t2 Tok_Semi in (t4,Print(e))
                                 | _ -> raise (InvalidInputException ";")
                                end
                            | _ -> raise (InvalidInputException "Print")
                          end  
                          
and parse_ifstmt l = let (r1,e1) = parse_expr l in 
                     begin
                     match r1 with
                       | Tok_LBrace::_ -> let tail2 = match_token r1 Tok_LBrace in 
                       let (r2,s1) = parse_body tail2 in 
                       begin
                       match r2 with
                         | Tok_RBrace::Tok_Else::Tok_LBrace::_ -> let tail3 = match_token r2 Tok_RBrace in let tail4 = match_token tail3 Tok_Else in
                         let tail5 = match_token tail4 Tok_LBrace in let (r3,s2) = parse_body tail5 in let t2 = lookahead r3 in
                         begin
                         match t2 with
                           | Tok_RBrace -> let tail6 = match_token r3 Tok_RBrace in (tail6,If(e1,s1,s2))
                           | _ -> raise (InvalidInputException "Missing brace")
                          end
                        | Tok_RBrace::_ -> let tail7 = match_token r2 Tok_RBrace in (tail7,If(e1,s1,NoOp))
                        | _ -> raise (InvalidInputException "brace?")
                      end
                      | _ -> raise (InvalidInputException "if")
                    end
                    
and parse_whilestmt l = let (r1,e1) = parse_expr l in let t1 = lookahead r1 in
                        begin 
                        match t1 with
                          | Tok_LBrace -> let tail2 = match_token r1 Tok_LBrace in 
                          let (r2,s1) = parse_body tail2 in let t3 = lookahead r2 in
                          begin
                          match t3 with
                            | Tok_RBrace -> let tail3 = match_token r2 Tok_RBrace in (tail3,While(e1,s1))
                            | _ -> raise (InvalidInputException "Missing }")
                          end 
                          | _ -> raise (InvalidInputException "While")
                        end


let parse_main toks = begin
                      match toks with
                        | Tok_Type_Int::Tok_Main::Tok_LParen::Tok_RParen::Tok_LBrace::t -> let (tail,s) = parse_stmt t in
                        begin
                        match tail with 
                          (*| Tok_RBrace::EOF::_ -> s*)
                          | EOF::_ -> s
                          | _ -> raise (InvalidInputException "Main")
                        end
                        | _ -> raise (InvalidInputException "Main")
                      end