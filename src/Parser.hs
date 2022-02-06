{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import           Control.Applicative            ( (<|>)
                                                , many
                                                )
import           Control.Monad.Reader           ( Reader
                                                , ask
                                                , local
                                                , runReader
                                                )
import           Data.Functor                   ( ($>) )
import           Data.Text.Lazy                 ( Text
                                                , pack
                                                , toStrict
                                                )
import           Lucid                          ( Attribute
                                                , Html
                                                , br_
                                                , class_
                                                , div_
                                                , rp_
                                                , rt_
                                                , ruby_
                                                , style_
                                                , toHtml
                                                )
import           Text.Parsec                    ( ParsecT
                                                , Stream
                                                , anyChar
                                                , char
                                                , choice
                                                , digit
                                                , lookAhead
                                                , many1
                                                , manyTill
                                                , newline
                                                , noneOf
                                                , oneOf
                                                , optionMaybe
                                                , parse
                                                , spaces
                                                , string
                                                , try
                                                )

data Mark = Plain Text
          | NewLine
          | Seq [Mark]
          | Bold Text
          | Underline Text
          | Italic Text
          | BulletList [Mark]
          | NumberedList [Mark]
          | ListItem Mark
          | Style Text Mark
          | Ruby Mark
          | Rp Mark
          | Rt Mark
          deriving (Show, Eq)

markToHtml' :: Mark -> Reader [Attribute] (Html ())
markToHtml' (Plain txt)        = release div_ (toHtml txt)
markToHtml' NewLine            = return $ br_ []
markToHtml' (Seq          ms ) = sequence_ <$> mapM markToHtml' ms
markToHtml' (Bold         txt) = buildup (class_ "bold") $ release div_ (toHtml txt)
markToHtml' (Underline    txt) = buildup (class_ "underline") $ release div_ (toHtml txt)
markToHtml' (Italic       txt) = buildup (class_ "italic") $ release div_ (toHtml txt)
markToHtml' (BulletList   mas) = div_ [class_ "bullet-list"] <$> buildup (class_ "bullet-item ") (sequence_ <$> mapM markToHtml' mas)
markToHtml' (NumberedList mas) = div_ [class_ "numbered-list"] <$> buildup (class_ "numbered-item ") (sequence_ <$> mapM markToHtml' mas)
markToHtml' (ListItem     mas) = buildup (class_ "item ") ((>> br_ []) <$> markToHtml' mas)
markToHtml' (Style txt ma    ) = buildup (style_ (toStrict txt)) $ markToHtml' ma
markToHtml' (Ruby ma         ) = ruby_ <$> markToHtml' ma
markToHtml' (Rp   ma         ) = rp_ <$> markToHtml' ma
markToHtml' (Rt   ma         ) = rt_ <$> markToHtml' ma

markToHtml :: Mark -> Html ()
markToHtml = flip runReader [] . markToHtml'

release :: ([Attribute] -> Html () -> Html ()) -> Html () -> Reader [Attribute] (Html ())
release f ma = do
    attrs <- ask
    return $ f attrs ma

buildup :: Attribute -> Reader [Attribute] (Html ()) -> Reader [Attribute] (Html ())
buildup attr = local (attr :)

-- >>> markToHtml (BulletList [ListItem (Plain "a"), ListItem (Plain "b")])
-- <div class="bullet-list"><div class="bullet-item item ">a</div><br><div class="bullet-item item ">b</div><br></div>

-- | parse text until two newlines
parsePlain :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parsePlain = Plain . pack <$> many1 (noneOf "\n\r")

parseBr :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parseBr = do
    _ <- string "<br>"
    return NewLine

parseBold :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parseBold = do
    _   <- string "**"
    txt <- manyTill anyChar (try $ string "**")
    return $ Bold $ pack txt

parseUnderline :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parseUnderline = do
    _   <- string "__"
    txt <- manyTill anyChar (try $ string "__")
    return $ Underline $ pack txt

parseItalic :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parseItalic = do
    _   <- string "*"
    txt <- manyTill anyChar (try $ string "*")
    return $ Italic $ pack txt

parseBulletList :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parseBulletList = do
    -- preceded by pragma "@[bullet-list]"
    _     <- string "@[bullet-list]"
    -- followed by a newline
    _     <- newline
    -- followed by a list of items
    items <- manyTill parseListItem (try $ string "@[list-end]")
    return $ BulletList items

parseNumberedList :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parseNumberedList = do
    -- preceded by plagma "@[numbered-list]"
    _     <- string "@[numbered-list]"
    -- followed by a newline
    _     <- newline
    -- followed by a list of items
    items <- manyTill parseListItem (try $ string "@[list-end]")
    return $ NumberedList items

parseListItem :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parseListItem = do
    -- start by a "*", "-", or number "1." "2.", etc
    _ <- oneOf "*-" <|> (many1 digit >> char '.')
    spaces
    -- collect the rest of the text until a double newlines
    txt <- step ""
    return $ ListItem $ parseMark' $ pack txt
  where
    step acc = do
        r <- lookAhead (optionMaybe (try (oneOf "*-") <|> try (many1 digit >> char '.') <|> try (string "@[list-end]" $> ' ')))
        case r of
            Nothing -> step . (acc ++) =<< do
                r    <- optionMaybe (oneOf "@*-" <|> digit)
                rest <- many1 (noneOf "@*-123456789")
                case r of
                    Nothing -> return rest
                    Just c  -> return $ c : rest
            Just _ -> return acc

parseStyle :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parseStyle = do
    -- style block starts with "@[style property1=value1 property2=value2 ...]"
    _ <- string "@[style"
    spaces
    props <- manyTill anyChar (try $ string "]")
    -- followed by a newline
    _     <- newline
    -- followed by the text
    txt   <- manyTill anyChar (try $ string "@[style-end]")
    return $ Style (pack props) $ parseMark' $ pack txt

parseRuby :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parseRuby = do
    -- ruby block: @[ruby block]
    -- where block is either "{plain-text}", "[rt-text]" or "(rp-text)"
    _ <- string "@[ruby"
    spaces
    mas <- many (choice [plainBlock, rtBlock, rpBlock] <* spaces)
    char ']'
    return $ Ruby (Seq mas)
  where
    plainBlock = do
        _   <- char '{'
        txt <- many (noneOf "}")
        char '}'
        return $ parseMark' $ pack txt
    rtBlock = do
        _   <- char '['
        txt <- many (noneOf "]")
        char ']'
        return $ Rt $ parseMark' $ pack txt
    rpBlock = do
        _   <- char '('
        txt <- many (noneOf ")")
        char ')'
        return $ Rp $ parseMark' $ pack txt

parseMark :: (Monad m, Stream s m Char) => ParsecT s () m Mark
parseMark =
    spaces
        *> choice
               [ try parseBold
               , try parseBr
               , try parseUnderline
               , try parseItalic
               , try parseBulletList
               , try parseNumberedList
               , try parseListItem
               , try parseStyle
               , try parseRuby
               , parsePlain
               ]
        <* spaces

parseMark' :: Text -> Mark
parseMark' ""  = Plain ""
parseMark' txt = case parse parseMark "embedded" txt of
    Left  err -> error $ show err
    Right x   -> x

parseMarks :: (Monad m, Stream s m Char) => ParsecT s () m [Mark]
parseMarks = many1 parseMark

parseMarks' :: Text -> Mark
parseMarks' txt = case parse parseMarks "" txt of
    Left  err -> error $ show err
    Right x   -> Seq x

