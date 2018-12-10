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

formRank :: PlayerId -> Form Ranking
formRank plaid = renderBootstrap $ Ranking plaid
    <$> areq intField "Rank: " Nothing
    <*> areq intField "Pontos de Liga: " Nothing
    
getRankR :: PlayerId -> Handler Html
getRankR plaid = do
    -- GERA O FORMULARIO NA widgetForm
    player <- runDB $ get404 plaid
    (widgetForm, enctype) <- generateFormPost (formRank plaid)
    defaultLayout $ do
        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
        toWidget $(luciusFile "templates/rank.lucius")
        $(whamletFile "templates/rank.hamlet")

postRankR :: PlayerId -> Handler Html
postRankR plaid = do
    -- LE O DIGITADO
    ((res,_),_) <- runFormPost (formRank plaid)
    case res of
        FormSuccess rank -> do
            -- INSERE O PRODUTO
            rid <- runDB $ insert rank
            setMessage [shamlet|
                Player Rankeado com sucesso!
            |]
            redirect HomeR
        _ -> redirect HomeR

getRankingR :: Handler Html
getRankingR = do
    rankings <- runDB $ rawSql
        "SELECT ??, ?? \
        \FROM ranking INNER JOIN player \
        \ON ranking.plaid=player.id"
        []
    defaultLayout $ do
        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
        toWidget $(luciusFile "templates/ranking.lucius")
        $(whamletFile "templates/ranking.hamlet")