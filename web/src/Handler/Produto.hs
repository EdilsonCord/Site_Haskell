{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Produto where

import Import
import Tool
--import Database.Persist.Postgresql

-- (<$>) = fmap :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
formProduto :: Maybe Produto -> Form Produto
formProduto prod = renderDivs $ Produto  
    <$> areq textField (FieldSettings "Nome: " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                       ) (fmap produtoNome prod)
    <*> areq doubleField "Preco: " (fmap produtoPreco prod)

auxProdutoR :: Route App -> Maybe Produto -> Handler Html
auxProdutoR rt produto = do
    (widget,_) <- generateFormPost (formProduto produto)
    defaultLayout $ do
        [whamlet|
            <h1>
                 CADASTRO DE PRODUTO
            
            <form action=@{rt} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
    
getProdutoR :: Handler Html
getProdutoR = auxProdutoR ProdutoR Nothing
    
postProdutoR :: Handler Html
postProdutoR = do
    ((resp,_),_) <- runFormPost (formProduto Nothing)
    case resp of 
         FormSuccess produto -> do 
             pid <- runDB $ insert produto
             redirect (DescR pid)
         _ -> redirect HomeR
    
-- SELECT * from produto where id = pid 
getDescR :: ProdutoId -> Handler Html
getDescR pid = do 
    produto <- runDB $ get404 pid
    (widget,_) <- generateFormPost formQt
    defaultLayout [whamlet|
        <h1>
            Nome: #{produtoNome produto}
        
        <h2>
            Preco: #{produtoPreco produto}
        
        <form action=@{ComprarR pid} method=post>
            ^{widget}
            <input type="submit" value="Comprar">
    |]

getListProdR :: Handler Html
getListProdR = do 
    -- produtos :: [Entity Produto]
    produtos <- runDB $ selectList [] [Desc ProdutoPreco]
    defaultLayout [whamlet|
            <table>
                <thead>
                    <tr>
                        <th> 
                            Nome
                        
                        <th>
                            Produto
                        
                        <th>
                        
                        <th>
                <tbody>
                    $forall Entity pid prod <- produtos
                        <tr>
                            <td>
                                <a href=@{DescR pid}>
                                    #{produtoNome prod}
                            
                            <td>
                                #{produtoPreco prod}
                            
                            <th>
                                <a href=@{UpdProdR pid}>
                                    Editar
                            <th>
                                <form action=@{DelProdR pid} method=post>
                                    <input type="submit" value="X">
    |]

getUpdProdR :: ProdutoId -> Handler Html
getUpdProdR pid = do 
    antigo <- runDB $ get404 pid
    auxProdutoR (UpdProdR pid) (Just antigo)    
    
-- UPDATE produto WHERE id = pid SET ...
postUpdProdR :: ProdutoId -> Handler Html
postUpdProdR pid = do
    ((resp,_),_) <- runFormPost (formProduto Nothing)
    case resp of 
         FormSuccess novo -> do
            runDB $ replace pid novo
            redirect (DescR pid) 
         _ -> redirect HomeR

postDelProdR :: ProdutoId -> Handler Html
postDelProdR pid = do 
    _ <- runDB $ get404 pid 
    runDB $ delete pid 
    redirect ListProdR



