{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Text.Lucius
import Text.Julius
import Import
import Database.Persist.Sql

widgetBootstrapTheme :: Widget
widgetBootstrapTheme = $(whamletFile "templates/bootstrapTheme.hamlet")

formLogin :: Form (Text,Text)
formLogin = renderBootstrap $ (,) 
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getLoginR :: Handler Html
getLoginR = do 
    (widgetForm, enctype) <- generateFormPost formLogin
    mensagem <- getMessage
    defaultLayout $ do 
        setTitle "Gamble: The Game | Login"
        toWidget $(luciusFile "templates/login.lucius")
        $(whamletFile "templates/login.hamlet")
    
postLoginR :: Handler Html 
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess ("admin@admin.com", "admin123") -> do
            setSession "_ADM" (pack $ show $ Player "admin" "admin@admin.com" "")
            redirect AdminR
        FormSuccess (email,senha) -> do
            logado <- runDB $ selectFirst [PlayerEmail ==. email,
                                           PlayerSenha ==. senha] []
            case logado of
                Just (Entity plaid player) -> do 
                    setSession "_PLA" (pack $ show player)
                    setMessage [shamlet|
                        <h1>
                            Player logado
                    |]
                    redirect HomeR
                Nothing -> do 
                    setMessage [shamlet|
                        <h1>
                            Player e senha não encontrados!
                    |]
                    redirect LoginR
        _ -> redirect LoginR
        
postLogoutR :: Handler Html
postLogoutR = do 
    deleteSession "_ADM"
    deleteSession "_PLA"
    redirect HomeR
    