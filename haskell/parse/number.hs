parse_number :: ReadP Int
parse_number = read <$> munch1 isDigit

