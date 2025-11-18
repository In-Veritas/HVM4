parse_nam_or_dry :: ReadP Term
parse_nam_or_dry = do
  lexeme (char '^')
  choice
    [ do lexeme (char '('); f <- parse_term; x <- parse_term; lexeme (char ')'); return (Dry f x)
    , do k <- parse_name; return (Nam k)
    ]

