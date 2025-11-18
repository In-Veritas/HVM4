parse_par :: ReadP Term
parse_par = do
  lexeme (char '(')
  choice
    [ do lexeme (char ')'); return One
    , do
      t <- parse_term
      choice
        [ do lexeme (char ','); u <- parse_term; lexeme (char ')'); return (Tup t u)
        , do ts <- many parse_term; lexeme (char ')'); return (foldl' App t ts)
        ]
    ]

