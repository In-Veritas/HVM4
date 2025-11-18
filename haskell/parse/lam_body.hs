parse_lam_body :: ReadP Term
parse_lam_body = do
  k <- parse_name
  lexeme (char '.')
  body <- parse_term
  return (Lam (name_to_int k) body)

