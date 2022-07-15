import Data.List
import Data.Char

import Text.ParserCombinators.ReadP

-- data HtmlNode = HtmlNode {
--     content :: String,
--     tag :: String
-- } deriving Show

data HtmlNode =
    HtmlContent String |
    HtmlTag String [HtmlNode]
    deriving Show

parseContent :: ReadP String
parseContent = many get

openTag :: ReadP String
openTag = do
    char '<'
    tag <- munch1 isAlpha
    char '>'
    return tag

closeTag :: String -> (ReadP String)
closeTag tag = do
    char '<'
    char '/'
    string tag
    char '>'
    return tag

parseNode :: ReadP HtmlNode
parseNode = do
    tag <- openTag
    content <- parseContent 
    closeTag tag
    return $ HtmlTag tag [HtmlContent content]


main :: IO ()
main = do {
    str <- readFile "./bookmarks.html";
    print $ readP_to_S parseNode str 
}
