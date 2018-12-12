{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Ranking where

import Text.Lucius
import Text.Julius
import Import
import Database.Persist.Sql

widgetBootstrapTheme :: Widget
widgetBootstrapTheme = $(whamletFile "templates/bootstrapTheme.hamlet")

formRank :: PlayerId -> Form Ranking
formRank plaid = renderBootstrap $ Ranking plaid
    <$> areq intField "Rank: " Nothing
    <*> areq intField "Pontos de Liga: " Nothing
    
getRankR :: PlayerId -> Handler Html
getRankR plaid = do
    -- GERA O FORMULARIO NA widgetForm
    player <- runDB $ get404 plaid
    titulo <- return $ toHtml $ "Gamble: The Game | Rankeando jogador " ++ (playerNome player)
    (widgetForm, enctype) <- generateFormPost (formRank plaid)
    defaultLayout $ do
        setTitle titulo
        toWidget $(luciusFile "templates/rank.lucius")
        $(whamletFile "templates/rank.hamlet")

postRankR :: PlayerId -> Handler Html
postRankR plaid = do
    ranking <- runDB $ selectFirst [RankingPlaid ==. plaid] []
    case ranking of
        Just (Entity ranid _) -> do
            -- LE O DIGITADO
            ((res,_),_) <- runFormPost (formRank plaid)
            case res of
                FormSuccess rank -> do
                    -- INSERE O PRODUTO
                    runDB $ replace ranid rank
                    setMessage [shamlet|
                        Player Rankeado com sucesso!
                    |]
                    redirect HomeR
                _ -> redirect HomeR
        Nothing -> do
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
        \ON ranking.plaid=player.id \
        \ORDER BY ranking.rank ASC"
        []
    admin <- lookupSession "_ADM"
    defaultLayout $ do
        setTitle "Gamble: The Game | Ranking de Jogadores"
        toWidget $(luciusFile "templates/ranking.lucius")
        $(whamletFile "templates/ranking.hamlet")