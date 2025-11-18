parse_uni :: ReadP Term
parse_uni = lexeme (char '⊤') >> return Uni

