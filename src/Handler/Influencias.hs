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

widgetBootstrapLinks :: Widget
widgetBootstrapLinks = $(whamletFile "templates/bootstrapLinks.hamlet")

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
        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
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
    defaultLayout $ do
        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
        toWidget $(luciusFile "templates/influenciasAll.lucius")
        $(whamletFile "templates/influenciasAll.hamlet")