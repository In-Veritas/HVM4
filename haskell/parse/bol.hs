parse_bol :: ReadP Term
parse_bol = lexeme (char '𝔹') >> return Bol

