parse_era :: ReadP Term
parse_era = lexeme (string "&{}") >> return Era

