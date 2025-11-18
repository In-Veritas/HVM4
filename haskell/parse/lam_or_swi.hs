parse_lam_or_swi :: ReadP Term
parse_lam_or_swi = do
  lexeme (choice [char 'λ', char '\\'])
  parse_lam_brace <++ parse_lam_body

