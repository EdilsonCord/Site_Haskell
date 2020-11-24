{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Tool
import Text.Lucius

formLogin :: Form (Text, Text)
formLogin = renderBootstrap $ (,)
    <$>  areq emailField "E-mail: " Nothing
    <*>  areq passwordField "Senha: " Nothing
    
getEntrarR :: Handler Html
getEntrarR = do 
    (widget,_) <- generateFormPost formLogin
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
        geraForm EntrarR "INFORME SUAS CREDENCIAIS" "Login" msg widget

postEntrarR :: Handler Html
postEntrarR = do 
    ((result,_),_) <- runFormPost formLogin
    case result of 
        FormSuccess ("admin@admin.com","root") -> do
            setSession "_EMAIL" "admin@admin.com"
            redirect HomeR
        FormSuccess (email,senha) -> do 
           -- select * from usuario where email=digitado.email
           usuario <- runDB $ getBy (UniqueEmail email)
           case usuario of 
                Nothing -> do 
                    setMessage [shamlet|
                        <div>
                            E-mail ainda não foi cadastrado!
                    |]
                    redirect EntrarR
                Just (Entity _ usu) -> do 
                    if (usuarioSenha usu == senha) then do
                        setSession "_EMAIL" (usuarioEmail usu)
                        redirect HomeR
                    else do 
                        setMessage [shamlet|
                            <div>
                                Senha incorreta. Favor tentar novamente!
                        |]
                        redirect EntrarR 
        _ -> redirect HomeR
        
postSairR :: Handler Html 
postSairR = do 
    deleteSession "_EMAIL"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = defaultLayout $ do
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
                <h3>
                    VOCÊ ESTÁ LOGADO COMO ADMIN
        |]
