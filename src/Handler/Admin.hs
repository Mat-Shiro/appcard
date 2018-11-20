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

widgetBootstrapLinks :: Widget
widgetBootstrapLinks = $(whamletFile "templates/bootstrapLinks.hamlet")

getAdminR :: Handler Html   
getAdminR = do 
    players <- runDB $ selectList [] [Asc PlayerNome]
    defaultLayout $ do 
        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
        $(whamletFile "templates/admin.hamlet")

postApagarR :: PlayerId -> Handler Html
postApagarR plaid = do 
    runDB $ delete plaid
    redirect AdminR
