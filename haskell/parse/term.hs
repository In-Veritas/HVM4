parse_term :: ReadP Term
parse_term = do
  t <- parse_term_base
  parse_term_suff t

