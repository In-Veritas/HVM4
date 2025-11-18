parse_emp :: ReadP Term
parse_emp = lexeme (char '⊥') >> return Emp

