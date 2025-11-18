parse_nat :: ReadP Term
parse_nat = lexeme (char 'ℕ') >> return Nat

