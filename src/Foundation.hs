{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)
import Prelude              (read)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

mkMessage "App" "messages" "en"

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    makeLogger = return . appLogger
    authRoute _ = Just LoginR
    isAuthorized HomeR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized PlayerR _ = return Authorized
    
    isAuthorized InfluenciasR getInfluenciasR = ehAdmin
    isAuthorized SuperR getSuperR = ehAdmin
    isAuthorized AdminR _ = ehAdmin
    isAuthorized LogoutAdmR _ = ehAdmin
    
    isAuthorized _ _ = ehPlayer
    

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

ehAdmin :: Handler AuthResult
ehAdmin = do 
    player <- lookupSession "_PLA"
    case player of
        Just _ -> return $ Unauthorized "Acesso negado!"
        Nothing -> do
            logado <- lookupSession "_ADM"
            case logado of 
                Just stringPlayer -> do 
                    player <- return $ read $ unpack stringPlayer
                    if (playerNome player) == "admin" then do 
                        return Authorized
                    else 
                        return $ Unauthorized "Acesso negado!"
                Nothing -> return AuthenticationRequired
    

ehPlayer :: Handler AuthResult
ehPlayer = do
    logado <- lookupSession "_PLA"
    case logado of
        Just _ -> return Authorized
        Nothing -> return AuthenticationRequired