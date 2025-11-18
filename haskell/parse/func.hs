parse_func :: ReadP (Name, Term)
parse_func = do
  lexeme (char '@')
  k <- parse_name
  lexeme (char '=')
  f <- parse_term
  return (name_to_int k, f)

