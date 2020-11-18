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

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
    authRoute _ = Just EntrarR
    
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized EntrarR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUsuario 
    makeLogger = return . appLogger

isAdmin :: Handler AuthResult
isAdmin = do 
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> return AuthenticationRequired
        Just "admin@admin.com" -> return Authorized
        Just _ -> return $ Unauthorized "VC EH USUARIO COMUM"

isUsuario :: Handler AuthResult
isUsuario = do 
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> return AuthenticationRequired
        Just _ -> return Authorized
        
type Form a = Html -> MForm Handler (FormResult a, Widget)    
    
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
