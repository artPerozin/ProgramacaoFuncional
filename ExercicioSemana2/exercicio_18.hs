-- maneira 1:
minusculas :: String -> String
minusculas = map toLowerManual
  where
    toLowerManual c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise            = c

-- maneira 2:
-- minusculas = map toLower