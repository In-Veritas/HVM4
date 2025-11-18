parse_dup :: ReadP Term
parse_dup = do
  lexeme (char '!')
  k <- parse_name
  lexeme (char '&')
  l <- parse_name
  lexeme (char '=')
  v <- parse_term
  optional (lexeme (char ';'))
  t <- parse_term
  return (Dup (name_to_int k) (name_to_int l) v t)

