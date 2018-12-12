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

widgetBootstrapTheme :: Widget
widgetBootstrapTheme = $(whamletFile "templates/bootstrapTheme.hamlet")

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
        setTitle "Gamble: The Game | Cadastro"
        toWidget $(luciusFile "templates/player.lucius")
        $(whamletFile "templates/player.hamlet")
    
postPlayerR :: Handler Html 
postPlayerR = do 
    ((res,_),_) <- runFormPost formPlayer
    case res of 
        FormSuccess (player,confirmacao) -> do
            if (playerSenha player == confirmacao) then do 
                runDB $ insert player
                redirect LoginR
            else do 
                setMessage [shamlet|
                    <h1>
                        Senha e confirmação não batem!
                |]
                redirect PlayerR
        _ -> redirect PlayerR
        