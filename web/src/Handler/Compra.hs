{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Compra where

import Import
import Tool
import Database.Persist.Sql

getListCompraR :: Handler Html
getListCompraR = do
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect HomeR
        Just email -> do
            usu <- runDB $ getBy (UniqueEmail email)
            case usu of 
                 Nothing -> redirect HomeR 
                 Just (Entity uid usuario) -> do 
                     let sql = "SELECT ??,??,?? FROM usuario \
                        \ INNER JOIN compra ON compra.usuarioid = usuario.id \
                        \ INNER JOIN imovel ON compra.imovelid = imovel.id \
                        \ WHERE usuario.id = ?"
                     imoveis <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity Compra,Entity Imovel)]
                     defaultLayout $ do 
                        [whamlet|
                            <h1>
                                COMPRAS de #{usuarioNome usuario}
                            
                            <ul>
                                $forall (Entity _ _, Entity _ compra, Entity _ imovel) <- imoveis
                                    <li>
                                        #{imovelNome imovel}: #{imovelPreco imovel * (fromIntegral (compraQtunit compra))}
        |]

postComprarR :: ImovelId -> Handler Html
postComprarR pid = do
    ((resp,_),_) <- runFormPost formQt
    case resp of 
         FormSuccess qt -> do 
             sess <- lookupSession "_EMAIL"
             case sess of 
                  Nothing -> redirect HomeR
                  Just email -> do 
                      usuario <- runDB $ getBy (UniqueEmail email)
                      case usuario of 
                           Nothing -> redirect HomeR 
                           Just (Entity uid _) -> do 
                               runDB $ insert (Compra pid uid qt)
                               redirect ListCompraR
         _ -> redirect HomeR
