{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Database.Persist.Postgresql
import Text.Lucius

--         <img src=@{StaticR img_imovel_jpg}>
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
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

        <img src="https://i.imgur.com/zOQImYb.png" title="walking" />        
    |]            
