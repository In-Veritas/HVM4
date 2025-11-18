parse_sig :: ReadP Term
parse_sig = do
  lexeme (char 'Σ')
  a <- parse_term_base
  lexeme (char '.')
  b <- parse_term
  return (Sig a b)

