open SmallCTypes
open Str
open String
open Pervasives

let rec tokenize input = 
    if (string_match (regexp "^while") (trim input) 0) = true then Tok_While :: (tokenize (sub (trim input) (5) ((length (trim input))-5)))
    else if (string_match (regexp "^int") (trim input) 0) = true then Tok_Type_Int :: (tokenize (sub (trim input) (3) ((length (trim input))-3)))
    else if (string_match (regexp "^bool") (trim input) 0) = true then Tok_Type_Bool :: (tokenize (sub (trim input) (4) ((length (trim input))-4)))
    else if (string_match (regexp "^;\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Semi :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^)\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_RParen :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^}\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_RBrace :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^printf") (trim input) 0) = true then Tok_Print :: (tokenize (sub (trim input) (6) ((length (trim input))-6)))
    else if (string_match (regexp "^\\^\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Pow :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^\\+\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Plus :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^||\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Or :: (tokenize (sub (trim input) (2) ((length (trim input))-2)))
    else if (string_match (regexp "^!=\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_NotEqual :: (tokenize (sub (trim input) (2) ((length (trim input))-2)))
    else if (string_match (regexp "^!\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Not :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^\\*\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Mult :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^main") (trim input) 0) = true then Tok_Main :: (tokenize (sub (trim input) (4) ((length (trim input))-4)))
    else if (string_match (regexp "^<=\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_LessEqual :: (tokenize (sub (trim input) (2) ((length (trim input))-2)))
    else if (string_match (regexp "^<\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Less :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^(\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_LParen :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^{\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_LBrace :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^if") (trim input) 0) = true then Tok_If :: (tokenize (sub (trim input) (2) ((length (trim input))-2)))
    else if (string_match (regexp "^>=\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_GreaterEqual :: (tokenize (sub (trim input) (2) ((length (trim input))-2)))
    else if (string_match (regexp "^>\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Greater :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^==\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Equal :: (tokenize (sub (trim input) (2) ((length (trim input))-2)))
    else if (string_match (regexp "^else") (trim input) 0) = true then Tok_Else :: (tokenize (sub (trim input) (4) ((length (trim input))-4)))
    else if (string_match (regexp "^/\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Div :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^=\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Assign :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^&&\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_And :: (tokenize (sub (trim input) (2) ((length (trim input))-2)))
    else if (string_match (regexp "^-\\(\n\\|\t\\| \\)*") (trim input) 0) = true then Tok_Sub :: (tokenize (sub (trim input) (1) ((length (trim input))-1)))
    else if (string_match (regexp "^\\(true\\|false\\)") (trim input) 0) = true then let token = matched_string (trim input) in Tok_Bool (bool_of_string token) :: (tokenize (sub (trim input) (match_end()) ((length (trim input))-match_end())))
    else if (string_match (regexp "^\\(-?[0-9]+\\)") (trim input) 0) = true then let token = matched_string (trim input) in let num = match_end() in Tok_Int (int_of_string token) :: (tokenize (sub (trim input) (num) ((length (trim input))-num)))
    
    else if (string_match (regexp "^\\([a-zA-Z][a-zA-Z0-9]*\\)") (trim input) 0) = true then let token = matched_string (trim input) in Tok_ID token :: (tokenize (sub (trim input) (match_end()) ((length (trim input))-match_end())))
    else [EOF]
;;