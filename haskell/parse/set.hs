parse_set :: ReadP Term
parse_set = lexeme (char '*') >> return Set

