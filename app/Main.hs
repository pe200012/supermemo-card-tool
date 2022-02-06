{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson                     ( (.=)
                                                , encode
                                                , object
                                                )
import           Data.Text                      ( Text )
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status      ( statusCode )

main :: IO ()
main = pure ()
