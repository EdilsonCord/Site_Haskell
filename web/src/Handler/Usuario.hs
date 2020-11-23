{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Tool
import Text.Lucius

formUsu :: Form (Usuario, Text)
formUsu = renderBootstrap $ (,)
    <$> (Usuario 
        <$> areq textField "Nome: " Nothing
        <*> areq emailField "E-mail: " Nothing
        <*> areq passwordField "Senha: " Nothing)
    <*> areq passwordField "Digite Novamente: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do 
    (widget,_) <- generateFormPost formUsu
    msg <- getMessage
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
                        <a href=@{ListCompraR}>
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
        geraForm UsuarioR "CADASTRO DE USUÁRIO" "Cadastrar" msg widget

postUsuarioR :: Handler Html
postUsuarioR = do 
    ((result,_),_) <- runFormPost formUsu
    case result of 
        FormSuccess (usuario,veri) -> do 
            if (usuarioSenha usuario == veri) then do 
                runDB $ insert400 usuario 
                setMessage [shamlet|
                    <div>
                        USUARIO INCLUIDO
                |]
                redirect UsuarioR
            else do 
                setMessage [shamlet|
                    <div>
                        SENHA E VERIFICACAO N CONFEREM
                |]
                redirect UsuarioR
        _ -> redirect HomeR
    
    
