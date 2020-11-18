{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Database.Persist.Postgresql

--         <img src=@{StaticR img_produto_jpg}>
getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    [whamlet|
        <h1>
            SISTEMA DE PRODUTO
        <ul>
            <li> 
                <a href=@{ProdutoR}>
                    CADASTRO

            <li>
                <a href=@{ListProdR}>
                    LISTAR
        
            $maybe email <- sess
                <li>
                    <div>
                        #{email}
                        <form method=post action=@{SairR}>
                            <input type="submit" value="Sair">
            $nothing
                <li>
                    <a href=@{EntrarR}>
                        ENTRE
    |]
