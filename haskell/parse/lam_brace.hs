parse_lam_brace :: ReadP Term
parse_lam_brace = between (lexeme (char '{')) (lexeme (char '}')) $ choice
  [ do lexeme (string "0:"); z <- parse_term; optional (lexeme (char ';')); lexeme (string "1+:"); s <- parse_term; return (Swi z s)
  , do lexeme (string "[]:"); n <- parse_term; optional (lexeme (char ';')); lexeme (string "<>:"); c <- parse_term; return (Mat n c)
  , do lexeme (string "#F:"); f <- parse_term; optional (lexeme (char ';')); lexeme (string "#T:"); t <- parse_term; return (If f t)
  , do lexeme (string "():"); u <- parse_term; return (Use u)
  , do lexeme (string ",:"); c <- parse_term; return (Get c)
  , return Efq
  ]

