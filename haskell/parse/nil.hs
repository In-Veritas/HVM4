parse_nil :: ReadP Term
parse_nil = do
  lexeme (string "[]")
  return Nil

