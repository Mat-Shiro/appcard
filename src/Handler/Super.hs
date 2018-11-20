{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Super where

import Text.Lucius
import Text.Julius
import Import

widgetBootstrapLinks :: Widget
widgetBootstrapLinks = $(whamletFile "templates/bootstrapLinks.hamlet")

formSuper :: Form Super
formSuper = renderBootstrap $ Super
    <$> areq textField "Nome: " Nothing
    <*> aopt textField "Ação Ativa: " Nothing
    <*> aopt textField "Ação Passiva: " Nothing
    
getSuperR :: Handler Html
getSuperR = do
    -- GERA O FORMULARIO NA widgetForm
    (widgetForm, enctype) <- generateFormPost formSuper
    defaultLayout $ do
        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
        toWidget $(luciusFile "templates/super.lucius")
        $(whamletFile "templates/super.hamlet")
    
postSuperR :: Handler Html
postSuperR = do
    -- LE O DIGITADO
    ((res,_),_) <- runFormPost formSuper
    case res of
        FormSuccess super -> do
            -- INSERE O PRODUTO
            iid <- runDB $ insert super
            redirect HomeR
        _ -> redirect HomeR