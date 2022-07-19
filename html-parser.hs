import Data.List
import Data.Char

import Text.ParserCombinators.ReadP

data HtmlNode =
    HtmlContent String |
    HtmlTag String [HtmlNode]
    deriving Show

openTag :: ReadP String
openTag = do
    char '<'
    tag <- munch1 isAlpha
    char '>'
    return tag

closeTag :: String -> ReadP String
closeTag tag = do
    char '<'
    char '/'
    string tag
    char '>'
    return tag

parseContent :: ReadP HtmlNode
parseContent = do
    content <- many1 $ satisfy (\ch -> all (ch/=) ['<', '>'])
    return $ HtmlContent content

parseTag :: ReadP HtmlNode
parseTag = do
    tag <- openTag
    children <- parseNode
    closeTag tag
    return $ HtmlTag tag children

parseNode :: ReadP [HtmlNode]
parseNode = option [] $ fmap (:[]) $ choice $ parseContent:parseTag:[] 

main :: IO ()
main = do
    str <- readFile "./bookmarks.html";
    print $ readP_to_S parseTag str 
