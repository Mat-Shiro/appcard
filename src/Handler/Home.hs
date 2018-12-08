{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Text.Lucius
import Text.Julius
import Import
import Prelude (read)
import Database.Persist.Sql

widgetBootstrapLinks :: Widget
widgetBootstrapLinks = $(whamletFile "templates/bootstrapLinks.hamlet")

getHomeR :: Handler Html
getHomeR = do 
    mensagem <- getMessage
    admin <- lookupSession "_ADM"
    case admin of
        Just _ -> do 
            defaultLayout $ do 
                addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
                toWidgetHead $(juliusFile "templates/home.julius")
                toWidget $(luciusFile "templates/home.lucius")
                $(whamletFile "templates/home.hamlet")
        Nothing -> do
            logado <- lookupSession "_PLA"
            case logado of 
                Just sessionPlayer -> do 
                    dados <- return $ read $ unpack sessionPlayer
                    player <- runDB $ selectFirst [PlayerEmail ==. (playerEmail dados), PlayerSenha ==. (playerSenha dados)] []
                    case player of
                        Just (Entity plaid _) -> do
                            pid <- return $ plaid
                            defaultLayout $ do 
                                addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
                                toWidgetHead $(juliusFile "templates/home.julius")
                                toWidget $(luciusFile "templates/home.lucius")
                                $(whamletFile "templates/home.hamlet")
                        Nothing -> do
                            defaultLayout $ do 
                                addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
                                toWidgetHead $(juliusFile "templates/home.julius")
                                toWidget $(luciusFile "templates/home.lucius")
                                $(whamletFile "templates/home.hamlet")
                Nothing -> do
                    defaultLayout $ do 
                        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
                        toWidgetHead $(juliusFile "templates/home.julius")
                        toWidget $(luciusFile "templates/home.lucius")
                        $(whamletFile "templates/home.hamlet")
