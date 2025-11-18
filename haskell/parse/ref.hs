parse_ref :: ReadP Term
parse_ref = do
  lexeme (char '@')
  k <- parse_name
  return (Ref (name_to_int k))

