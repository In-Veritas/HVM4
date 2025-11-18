parse_sup :: ReadP Term
parse_sup = do
  lexeme (char '&')
  l <- parse_name
  between (lexeme (char '{')) (lexeme (char '}')) $ do
    a <- parse_term
    optional (lexeme (char ','))
    b <- parse_term
    return (Sup (name_to_int l) a b)

