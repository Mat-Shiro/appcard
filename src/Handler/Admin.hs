{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Admin where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

widgetBootstrapTheme :: Widget
widgetBootstrapTheme = $(whamletFile "templates/bootstrapTheme.hamlet")

getAdminR :: Handler Html   
getAdminR = do 
    players <- runDB $ selectList [] [Asc PlayerNome]
    defaultLayout $ do 
        setTitle "Gamble: The Game | Painel Admin"
        $(whamletFile "templates/admin.hamlet")

postApagarR :: PlayerId -> Handler Html
postApagarR plaid = do 
    runDB $ deleteCascade plaid
    redirect AdminR
