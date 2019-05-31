{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Local where

import Import
import Database.Persist.Postgresql

-- <$>, <*> CAP 7.4 do LIVRO!
formLocal :: Maybe Local -> Form Local
formLocal mLocal = renderBootstrap $ Local 
    <$> areq textField "Nome: " (fmap localNome mLocal)
    <*> areq textField  "Descricao: " (fmap localDescricao mLocal)
    <*> areq textField "Endereco: " (fmap localEndereco mLocal)
    
-- ^ coloca outro html, no caso, os inputs
getLocalR :: Handler Html
getLocalR = do 
    (widget,enctype) <- generateFormPost (formLocal Nothing)
    defaultLayout $ do
        [whamlet|
            <a href=@{HomeLogadoR}>
                <input type="submit" value="Voltar">
            <form action=@{LocalR} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postLocalR :: Handler Html
postLocalR = do
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formLocal Nothing)
    case res of
        FormSuccess local -> do
            runDB $ insert local
            setMessage [shamlet|
                <h2>
                    LOCAL CADASTRADO COM SUCESSO!
            |]
            redirect HomeLogadoR
        _ -> redirect HomeLogadoR
    
-- SELECT * FROM Local
getTodosLocaisR :: Handler Html
getTodosLocaisR = do 
    locais <- runDB $ selectList [] [Asc LocalNome]
    defaultLayout $(whamletFile "templates/local.hamlet")

getLocalPerfilR :: LocalId -> Handler Html
getLocalPerfilR locid = do 
    local <- runDB $ get404 locid
    defaultLayout $ do 
        [whamlet|
            <a href=@{HomeLogadoR}>
                <input type="submit" value="Voltar">
            <h1>
                Nome #{localNome local}
            <div>
                Descricao: #{localDescricao local}
            <div>
                Endereco: #{localEndereco local}
        |]

postLocalApagarR :: LocalId -> Handler Html
postLocalApagarR locid = do
    runDB $ get404 locid
    runDB $ delete locid
    redirect TodosLocaisR

getLocalAlterarR :: LocalId -> Handler Html
getLocalAlterarR locid = do
    local <- runDB $ get404 locid
    (widget,enctype) <- generateFormPost (formLocal $ Just local)
    defaultLayout $ do
        [whamlet|
            <a href=@{HomeLogadoR}>
                <input type="submit" value="Voltar">
            <form action=@{LocalAlterarR locid} method=post>
                ^{widget}
                <input type="submit" value="Atualizar">
        |]

postLocalAlterarR :: LocalId -> Handler Html
postLocalAlterarR locid = do
    local <- runDB $ get404 locid
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formLocal $ Just local) 
    case res of
        FormSuccess localNovo -> do
            runDB $ replace locid localNovo
            redirect TodosLocaisR
        _ -> redirect HomeLogadoR