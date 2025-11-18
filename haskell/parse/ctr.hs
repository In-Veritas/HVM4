parse_ctr :: ReadP Term
parse_ctr = do
  lexeme (char '#')
  choice
    [ char 'F' >> return Fal
    , char 'T' >> return Tru
    ]

