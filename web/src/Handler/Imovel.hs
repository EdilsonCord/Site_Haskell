{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Imovel where

import Import
import Tool
--import Database.Persist.Postgresql

-- (<$>) = fmap :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
formImovel :: Maybe Imovel -> Form Imovel
formImovel imov = renderDivs $ Imovel  
    <$> areq textField (FieldSettings "Nome: " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                       ) (fmap imovelNome imov)
    <*> areq doubleField "Preco: " (fmap imovelPreco imov)

auxImovelR :: Route App -> Maybe Imovel -> Handler Html
auxImovelR rt imovel = do
    (widget,_) <- generateFormPost (formImovel imovel)
    defaultLayout $ do
        [whamlet|
            <h1>
                 CADASTRO DE PRODUTO
            
            <form action=@{rt} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
    
getImovelR :: Handler Html
getImovelR = auxImovelR ImovelR Nothing
    
postImovelR :: Handler Html
postImovelR = do
    ((resp,_),_) <- runFormPost (formImovel Nothing)
    case resp of 
         FormSuccess imovel -> do 
             pid <- runDB $ insert imovel
             redirect (DescR pid)
         _ -> redirect HomeR
    
-- SELECT * from imovel where id = pid 
getDescR :: ImovelId -> Handler Html
getDescR pid = do 
    imovel <- runDB $ get404 pid
    (widget,_) <- generateFormPost formQt
    defaultLayout [whamlet|
        <h1>
            Nome: #{imovelNome imovel}
        
        <h2>
            Preco: #{imovelPreco imovel}
        
        <form action=@{ComprarR pid} method=post>
            ^{widget}
            <input type="submit" value="Comprar">
    |]

getListImovR :: Handler Html
getListImovR = do 
    -- imoveis :: [Entity Imovel]
    imoveis <- runDB $ selectList [] [Desc ImovelPreco]
    defaultLayout [whamlet|
            <table>
                <thead>
                    <tr>
                        <th> 
                            Nome
                        
                        <th>
                            Imovel
                        
                        <th>
                        
                        <th>
                <tbody>
                    $forall Entity pid imov <- imoveis
                        <tr>
                            <td>
                                <a href=@{DescR pid}>
                                    #{imovelNome imov}
                            
                            <td>
                                #{imovelPreco imov}
                            
                            <th>
                                <a href=@{UpdImovR pid}>
                                    Editar
                            <th>
                                <form action=@{DelImovR pid} method=post>
                                    <input type="submit" value="X">
    |]

getUpdImovR :: ImovelId -> Handler Html
getUpdImovR pid = do 
    antigo <- runDB $ get404 pid
    auxImovelR (UpdImovR pid) (Just antigo)    
    
-- UPDATE imovel WHERE id = pid SET ...
postUpdImovR :: ImovelId -> Handler Html
postUpdImovR pid = do
    ((resp,_),_) <- runFormPost (formImovel Nothing)
    case resp of 
         FormSuccess novo -> do
            runDB $ replace pid novo
            redirect (DescR pid) 
         _ -> redirect HomeR

postDelImovR :: ImovelId -> Handler Html
postDelImovR pid = do 
    _ <- runDB $ get404 pid 
    runDB $ delete pid 
    redirect ListImovR



