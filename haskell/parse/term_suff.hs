parse_term_suff :: Term -> ReadP Term
parse_term_suff t = skipSpaces >> choice
  [ do string "&&"; t2 <- parse_term; return (And t t2)
  , do string "=="; t2 <- parse_term; return (Eql t t2)
  , do string "~>"; t2 <- parse_term; return (Gua t t2)
  , do string "[]"; t' <- return (Lst t); parse_term_suff t'
  , do string "<>"; t2 <- parse_term; return (Con t t2)
  , return t
  ]

