{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Player where

import Text.Lucius
import Text.Julius
import Import

widgetBootstrapLinks :: Widget
widgetBootstrapLinks = $(whamletFile "templates/bootstrapLinks.hamlet")

formPlayer :: Form (Player,Text)
formPlayer = renderBootstrap $ (,) 
    <$> (Player 
            <$> areq textField "Nome: " Nothing
            <*> areq emailField "E-mail: " Nothing
            <*> areq passwordField "Senha: " Nothing
        )
    <*> areq passwordField "Confirmacao de senha: " Nothing

getPlayerR :: Handler Html
getPlayerR = do 
    (widgetForm, enctype) <- generateFormPost formPlayer
    mensagem <- getMessage
    defaultLayout $ do 
        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
        toWidget $(luciusFile "templates/player.lucius")
        $(whamletFile "templates/player.hamlet")
    
postPlayerR :: Handler Html 
postPlayerR = do 
    ((res,_),_) <- runFormPost formPlayer
    case res of 
        FormSuccess (player,confirmacao) -> do
            if (playerSenha player == confirmacao) then do 
                runDB $ insert player
                setMessage [shamlet|
                    <h1>
                        Player cadastrado
                |]
                redirect HomeR
            else do 
                setMessage [shamlet|
                    <h1>
                        Senha e confirmacao não conferem
                |]
                redirect PlayerR
        _ -> redirect PlayerR
        