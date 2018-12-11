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

widgetBootstrapTheme :: Widget
widgetBootstrapTheme = $(whamletFile "templates/bootstrapTheme.hamlet")

getHomeR :: Handler Html
getHomeR = do 
    mensagem <- getMessage
    admin <- lookupSession "_ADM"
    logado <- lookupSession "_PLA"
    defaultLayout $ do 
        setTitle "Gamble: The Game | Página Inicial"
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/home.hamlet")
