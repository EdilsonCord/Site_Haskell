{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    addStylesheet (StaticR css_bootstrap_css)
    [whamlet|
        <h1>
            SISTEMA DE PRODUTO
        
        <img src=@{StaticR img_produto_jpg}>
        
        <ul>
            <li> 
                <a href=@{ProdutoR}>
                    CADASTRO
    |]
