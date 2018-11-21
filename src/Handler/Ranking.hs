{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Ranking where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

widgetBootstrapLinks :: Widget
widgetBootstrapLinks = $(whamletFile "templates/bootstrapLinks.hamlet")
    
getRankingR :: Handler Html
getRankingR = do
    defaultLayout $ do 
        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
        $(whamletFile "templates/ranking.hamlet")
    
postRankR :: RankingId -> Handler Html
postRankR ranid = do 
    runDB $ delete ranid
    redirect RankingR