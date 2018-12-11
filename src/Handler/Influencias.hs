{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Influencias where

import Text.Lucius
import Text.Julius
import Import

widgetBootstrapTheme :: Widget
widgetBootstrapTheme = $(whamletFile "templates/bootstrapTheme.hamlet")

formInfluencias :: Form Influencias
formInfluencias = renderBootstrap $ Influencias
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Ação Ativa: " Nothing
    <*> areq textField "Ação Passiva: " Nothing
    
getInfluenciasR :: Handler Html
getInfluenciasR = do
    -- GERA O FORMULARIO NA widgetForm
    (widgetForm, enctype) <- generateFormPost formInfluencias
    defaultLayout $ do
        setTitle "Gamble: The Game | Cadastro de Influência"
        toWidget $(luciusFile "templates/influencias.lucius")
        $(whamletFile "templates/influencias.hamlet")
    
postInfluenciasR :: Handler Html
postInfluenciasR = do
    -- LE O DIGITADO
    ((res,_),_) <- runFormPost formInfluencias
    case res of
        FormSuccess influencias -> do
            -- INSERE O PRODUTO
            iid <- runDB $ insert influencias
            redirect HomeR
        _ -> redirect HomeR
        
getInfluenciasAllR :: Handler Html
getInfluenciasAllR = do
    influencias <- runDB $ selectList [] [Asc InfluenciasNome]
    admin <- lookupSession "_ADM"
    defaultLayout $ do
        setTitle "Gamble: The Game | Influências"
        toWidget $(luciusFile "templates/influenciasAll.lucius")
        $(whamletFile "templates/influenciasAll.hamlet")