{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Aluguel where

import Import
import Tool
import Database.Persist.Sql
import Text.Lucius

getListAluguelR :: Handler Html
getListAluguelR = do
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect HomeR
        Just email -> do
            usu <- runDB $ getBy (UniqueEmail email)
            case usu of 
                 Nothing -> redirect HomeR 
                 Just (Entity uid usuario) -> do 
                     let sql = "SELECT ??,??,?? FROM usuario \
                        \ INNER JOIN aluguel ON aluguel.usuarioid = usuario.id \
                        \ INNER JOIN imovel ON aluguel.imovelid = imovel.id \
                        \ WHERE usuario.id = ?"
                     imoveis <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity Aluguel,Entity Imovel)]
                     defaultLayout $ do
                        toWidgetHead $(luciusFile  "templates/homepage.lucius") 
                        addStylesheet (StaticR css_bootstrap_css)
                        sess <- lookupSession "_EMAIL"
                        [whamlet|
                            <link href="https://fonts.googleapis.com/css?family=Cardo:400,700|Oswald" rel="stylesheet">
                            
                            <a href=@{HomeR}>
                                <h1>
                                    ALUGUEL DE IMÓVEIS
                            <ul>
                                <li> 
                                    <a href=@{ImovelR}>
                                        CADASTRO DE IMÓVEL

                                <li>
                                    <a href=@{ListImovR}>
                                        LISTA DE IMÓVEIS
                            
                                $maybe email <- sess

                                    <li>
                                        <a href=@{ListAluguelR}>
                                            LISTA DE ALUGUÉIS

                                    <li>
                                        <a>
                                            Logado como: #{email}

                                    <li>
                                        <div>
                                            
                                            <form method=post action=@{SairR}>
                                                <input type="submit" value="SAIR">
                                $nothing
                                    <li>
                                        <a href=@{EntrarR}>
                                            LOGIN

                                    <li> 
                                        <a href=@{UsuarioR}>
                                            CADASTRO DE USUÁRIO
                                
                        |]

                        [whamlet|
                            <div class="formDiv">
                                <h2>
                                    Aluguéis de #{usuarioNome usuario}
                                
                                <ul>
                                    $forall (Entity _ _, Entity _ aluguel, Entity _ imovel) <- imoveis
                                        <li class="itemPrincipal">
                                            #{imovelNome imovel}
                                            <ul>
                                                <li>
                                                    Preço da diária: #{imovelPreco imovel}
                                                <li>
                                                    Quant. de dias alugados: #{aluguelQtunit aluguel}                                                    
                                                <li>
                                                    Total do aluguel: #{imovelPreco imovel * (fromIntegral (aluguelQtunit aluguel))}

                        |]

postAlugarR :: ImovelId -> Handler Html
postAlugarR pid = do
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
                               runDB $ insert (Aluguel pid uid qt)
                               redirect ListAluguelR
         _ -> redirect HomeR
