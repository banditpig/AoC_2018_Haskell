{-# LANGUAGE TupleSections #-}
module Input
where
import           Data.Char
import           Text.Parsec
import           Text.Parsec.String

-- Make this into a library when done AoC
--
withData :: FilePath -> Parser a -> IO a
withData path p = do
    result <- parseFromFile (p <* eof) path
    either (error . show) return result

parserListString :: Parser [[String]]
parserListString = many1 (sepBy1 (many1 lower) (char ' ') <* newline)

parserListListInt :: Parser [[Int]]
parserListListInt = many1 (sepBy1 number (char '\t') <* newline)
    where
        number :: Parser Int
        number = read <$> many1 digit

-- this wont do -ve
parserListPositiveInt :: Parser [Int]
parserListPositiveInt = many1 (digitToInt <$> digit) <* newline
-- this will do -ve
parserListInt :: Parser [Int]
parserListInt = many ( int <* newline)

int :: Parser Int
int = char '-' *> pure ((-1)*) <*> natural <|> char '+' *> pure ((1)*) <*> natural <|> natural

natural :: Parser Int
natural = pure read <*> many1 digit

type Program = (String , Int)
data Tower = Tower (String , Int) [String]

instance Show Tower where
    show ( Tower nw towers) = show nw ++ show " -> " ++ show (map show towers)
instance Eq  Tower where
    (==) (Tower (n, _) _) (Tower (n', _) _)  = n == n'



commaSp :: Parser String
commaSp = string ", "

names :: Parser [String]
names = do
    nl <- sepEndBy1 (many1 letter) commaSp
    return  nl

program :: Parser Program
program = do
    n <- many1 letter
    space
    char '('
    w <- natural
    char ')'
    return (n, w)

-- see option https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-ParserCombinators-Parsec-Combinator.html
progLine :: Parser Tower
progLine = do
    p <- program
    arrow <- optionMaybe (string " -> ")
    case arrow of
        Just arrow -> do
            ns <- names
            return ( Tower p ns)
        _ -> return ( Tower p [])

allTowers = many (progLine <* newline)
parserDay7 = allTowers
-- just tests.
day1 :: IO ()
day1 = withData "Day1.txt" parserListInt >>= \ d -> print d

day2 :: IO ()
day2 = withData "data/day02.txt" parserListListInt >>= \ d -> print d

day4 :: IO ()
day4 = withData "data/day04.txt" parserListString >>= \ d -> print d

day7 :: IO ()
day7 = withData "data/day07.txt" allTowers >>= \ d -> print d