{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Sugestao where

import Text.Lucius
import Text.Julius
import Import
import Prelude (read)
import Database.Persist.Sql

widgetBootstrapTheme :: Widget
widgetBootstrapTheme = $(whamletFile "templates/bootstrapTheme.hamlet")

widgetBootstrapLinks :: Widget
widgetBootstrapLinks = $(whamletFile "templates/bootstrapLinks.hamlet")

formSugestao :: PlayerId -> Form Sugestao
formSugestao plaid = renderBootstrap $ Sugestao plaid
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Ação Ativa: " Nothing
    <*> areq textField "Ação Passiva: " Nothing
    
getSugestaoR :: PlayerId -> Handler Html
getSugestaoR plaid = do
    -- GERA O FORMULARIO NA widgetForm
    usuario <- runDB $ selectFirst [PlayerId ==. plaid] []
    case usuario of
        Just (Entity pid player) -> do
            logado <- lookupSession "_PLA"
            case logado of 
                Just sessionPlayer -> do 
                    dados <- return $ read $ unpack sessionPlayer
                    if (playerEmail player) == (playerEmail dados) then do
                        (widgetForm, enctype) <- generateFormPost (formSugestao plaid)
                        defaultLayout $ do
                            setTitle "Gamble: The Game | Envio de Sugestão"
                            toWidget $(luciusFile "templates/sugestao.lucius")
                            $(whamletFile "templates/sugestao.hamlet")
                    else
                        redirect HomeR
        Nothing -> return notFound ""
        
postSugestaoR :: PlayerId -> Handler Html
postSugestaoR plaid = do
    -- LE O DIGITADO
    ((res,_),_) <- runFormPost (formSugestao plaid)
    case res of
        FormSuccess sugestao -> do
            -- INSERE O PRODUTO
            iid <- runDB $ insert sugestao
            setMessage [shamlet|
                Sugestão enviada com sucesso!
            |]
            redirect HomeR
        _ -> redirect HomeR
        
getPainelR :: Handler Html
getPainelR = do
    admin <- lookupSession "_ADM"
    case admin of
        Just _ -> do 
            defaultLayout $ do 
                addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
                toWidget $(luciusFile "templates/painelSugestaoAdm.lucius")
                $(whamletFile "templates/painelSugestaoAdm.hamlet")
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
                                toWidget $(luciusFile "templates/painelSugestaoLogado.lucius")
                                $(whamletFile "templates/painelSugestaoLogado.hamlet")
                        Nothing -> do
                            defaultLayout $ do 
                                addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
                                toWidget $(luciusFile "templates/painelSugestao.lucius")
                                $(whamletFile "templates/painelSugestao.hamlet")
                Nothing -> do
                    defaultLayout $ do 
                        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
                        toWidget $(luciusFile "templates/painelSugestao.lucius")
                        $(whamletFile "templates/painelSugestao.hamlet")
                        
getSugestoesAllR :: Handler Html
getSugestoesAllR = do
    sugestoes <- runDB $ rawSql
        "SELECT ??, ?? \
        \FROM sugestao INNER JOIN player \
        \ON sugestao.plaid=player.id"
        []
    defaultLayout $ do
        setTitle "Gamble: The Game | Sugestões dos Players"
        toWidget $(luciusFile "templates/sugestoesAllAdm.lucius")
        $(whamletFile "templates/sugestoesAllAdm.hamlet")
        
getSugestoesPlayerR :: PlayerId -> Handler Html
getSugestoesPlayerR plaid = do
    usuario <- runDB $ selectFirst [PlayerId ==. plaid] []
    case usuario of
        Just (Entity pid player) -> do
            logado <- lookupSession "_PLA"
            case logado of 
                Just sessionPlayer -> do 
                    dados <- return $ read $ unpack sessionPlayer
                    if (playerEmail player) == (playerEmail dados) then do
                        sugestoes <- runDB $ selectList [SugestaoPlaid ==. plaid] [Desc SugestaoId]
                        defaultLayout $ do
                            setTitle "Gamble: The Game | Visualizando suas sugestões"
                            toWidget $(luciusFile "templates/sugestoesAllPlayer.lucius")
                            $(whamletFile "templates/sugestoesAllPlayer.hamlet")
                    else
                        redirect HomeR
                Nothing -> do
                    sugestoes <- runDB $ selectList [SugestaoPlaid ==. plaid] [Desc SugestaoId]
                    defaultLayout $ do
                        setTitle $ toHtml $ "Gamble: The Game | Visualizando sugestões de " ++ (playerNome player)
                        toWidget $(luciusFile "templates/sugestoesAllPlayer.lucius")
                        $(whamletFile "templates/sugestoesAllPlayer.hamlet")
        Nothing -> return notFound ""