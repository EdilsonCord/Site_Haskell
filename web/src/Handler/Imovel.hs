{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Imovel where

import Import
import Tool ( formQt )
import Text.Lucius
--import Database.Persist.Postgresql

-- (<$>) = fmap :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
formImovel :: Maybe Imovel -> Form Imovel
formImovel imov = renderDivs $ Imovel  
    <$> areq textField (FieldSettings "Nome:      " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                       ) (fmap imovelNome imov)
    <*> areq textField   "Descrição: " (fmap imovelDescricao imov)
    <*> areq textField   "Endereço:  " (fmap imovelEndereco imov)
    <*> areq doubleField "Preço:     " (fmap imovelPreco imov)
    

auxImovelR :: Route App -> Maybe Imovel -> Handler Html
auxImovelR rt imovel = do
    (widget,_) <- generateFormPost (formImovel imovel)
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
                        CADASTRO DE IMOVEL

                <li>
                    <a href=@{ListImovR}>
                        LISTAR
            
                $maybe email <- sess
                    <li>
                        <a>
                            Logado como: #{email}

                    <li>
                        <div>
                            #{email}
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
            <div class="formDivCadastro">
                <h3>
                    CADASTRO DE IMÓVEL

                <form action=@{rt} method=post>
                    ^{widget}
                    <input class="btnCadastrar" type="submit" value="Cadastrar">
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
                        CADASTRO DE IMOVEL

                <li>
                    <a href=@{ListImovR}>
                        LISTAR
            
                $maybe email <- sess
                    <li>
                        <a>
                            Logado como: #{email}

                    <li>
                        <div>
                            #{email}
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
            <h1>
                Nome: #{imovelNome imovel}
            
            <h2>
                Descricao: #{imovelDescricao imovel}
            
            <h3>
                Endereco: #{imovelEndereco imovel}
            
            <h4>
                Preco: #{imovelPreco imovel}
            
            <form action=@{ComprarR pid} method=post>
                ^{widget}
                <input type="submit" value="Comprar">
        |]

getListImovR :: Handler Html
getListImovR = do 
    -- imoveis :: [Entity Imovel]
    imoveis <- runDB $ selectList [] [Desc ImovelPreco]
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
                        CADASTRO DE IMOVEL

                <li>
                    <a href=@{ListImovR}>
                        LISTAR
            
                $maybe email <- sess
                    <li>
                        <a>
                            Logado como: #{email}

                    <li>
                        <div>
                            #{email}
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
            <div class="divLista">
                <table>
                    <thead>
                        <tr>
                            <th> 
                                Nome
                            
                            <th>
                                Descrição
                            
                            <th>
                                Endereço
                            
                            <th>
                                Preço: 

                            <th>
                    <tbody>
                        $forall Entity pid imov <- imoveis
                            <tr>
                                <td>
                                    <a href=@{DescR pid}>
                                        #{imovelNome imov}
                                
                                <td>
                                    #{imovelDescricao imov}
                                
                                <td>
                                    #{imovelEndereco imov}
                                
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

    
