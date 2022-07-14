import Data.Map
import Data.Char
import Text.ParserCombinators.ReadP

data HtmlTag = Link | Text

data HtmlNode = HtmlNode {
    content :: String,
    tag :: String
} deriving Show

contentParser :: ReadP String
contentParser = many1 $ satisfy (\_ -> True)

openTag :: ReadP String
openTag = do
    satisfy (== '<')
    tag <- many1 $ satisfy isAlpha
    satisfy (== '>')
    return tag

closingTag :: String -> (ReadP String)
closingTag tag = do
    satisfy (== '<')
    satisfy (== '/')
    string tag
    satisfy (== '>')
    return tag


parseNode :: ReadP HtmlNode
parseNode = do
    tag <- openTag
    content <- contentParser
    closingTag tag
    return HtmlNode { content=content, tag=tag }


main :: IO ()
main = do {
    str <- readFile "./bookmarks.html";
    print $ readP_to_S parseNode str 
}
