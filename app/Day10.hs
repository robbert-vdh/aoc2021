{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where



main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-10.txt"

  putStrLn "Part 1:"
  print (errorScore input)


-- | A line is simply either a stack of delimiters, or a 'Left' value containing
-- the rest of the line if the line contained invalid delimiters.
type Line = Either String [Delim]

-- | The delimiters in our input.
data Delim = Paren | Square | Curly | Angular
  deriving (Eq, Show)

-- | A token in our input file, we only have opening and closing delimiters.
data Token = Open Delim | Close Delim

-- | Parse the input file to a list of lines that either contain a stack of
-- unclosed delimiters, or a 'Nothing' if the line was not valid.
parse :: String -> [Line]
parse = map pLine . lines

-- | Compute error score from the invalid lines in the list.
errorScore :: [Line] -> Int
errorScore = sum . map lineScore
  where
    lineScore :: Line -> Int
    lineScore (Left ((pSym -> Just (Close Paren))   : _)) = 3
    lineScore (Left ((pSym -> Just (Close Square))  : _)) = 57
    lineScore (Left ((pSym -> Just (Close Curly))   : _)) = 1197
    lineScore (Left ((pSym -> Just (Close Angular)) : _)) = 25137
    lineScore _                                           = 0

pLine :: String -> Line
pLine = go []
  where
    go :: [Delim] -> String -> Line
    -- At the start of the line the stack will be empty
    go []                     ((pSym -> Just (Open d)) : ds) = go [d] ds
    -- At the end of the line we may have a stack of unclosed symbols
    go stack                  []                             = Right stack
    go stack@(current : rest) ((pSym -> Just token)    : ds)
      -- Insert new tokens on the stack
      | Open d <- token                                      = go (d : stack) ds
      -- And close adjacent tokens
      | Close d <- token, d == current                       = go rest ds
    go _                      remainder                      = Left remainder

pSym :: Char -> Maybe Token
pSym '(' = Just (Open Paren)
pSym ')' = Just (Close Paren)
pSym '[' = Just (Open Square)
pSym ']' = Just (Close Square)
pSym '{' = Just (Open Curly)
pSym '}' = Just (Close Curly)
pSym '<' = Just (Open Angular)
pSym '>' = Just (Close Angular)
pSym _   = Nothing
