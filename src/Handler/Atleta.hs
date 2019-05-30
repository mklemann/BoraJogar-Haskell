{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Atleta where

import Import
import Database.Persist.Postgresql

-- <$>, <*> CAP 7.4 do LIVRO!
formAtleta :: Maybe Atleta -> Form Atleta
formAtleta mAtleta = renderBootstrap $ Atleta 
    <$> areq textField "Nome: " (fmap atletaNome mAtleta)
    <*> areq intField  "Idade: " (fmap atletaIdade mAtleta)
    <*> areq intField "CPF: " (fmap atletaCpf mAtleta)
    <*> areq intField "Telefone: " (fmap atletaTelefone mAtleta)

-- ^ coloca outro html, no caso, os inputs

getAtletaR :: Handler Html
getAtletaR = do 
    (widget,enctype) <- generateFormPost (formAtleta Nothing)
    defaultLayout $ do
        [whamlet|
            <form action=@{AtletaR} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postAtletaR :: Handler Html
postAtletaR = do
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formAtleta Nothing)
    case res of
        FormSuccess atleta -> do
            runDB $ insert atleta
            setMessage [shamlet|
                <h2>
                    ATLETA CADASTRADO COM SUCESSO!
            |]
            redirect HomeLogadoR
        _ -> redirect HomeLogadoR
    
-- SELECT * FROM Aluno
getTodosAtletasR :: Handler Html
getTodosAtletasR = do 
    atletas <- runDB $ selectList [] [Asc AtletaNome]
    defaultLayout $(whamletFile "templates/atleta.hamlet")

getAtletaPerfilR :: AtletaId -> Handler Html
getAtletaPerfilR atlid = do 
    atleta <- runDB $ get404 atlid
    defaultLayout $ do 
        [whamlet|
            <h1>
                Atleta #{atletaNome atleta}
            <div>
                Idade: #{atletaIdade atleta}
            <div>
                CPF: #{atletaCpf atleta}
            <div>
                Telefone: #{atletaTelefone atleta}
        |]

postAtletaApagarR :: AtletaId -> Handler Html
postAtletaApagarR atlid = do
    runDB $ get404 atlid
    runDB $ delete atlid
    redirect TodosAtletasR

getAtletaAlteraR :: AtletaId -> Handler Html
getAtletaAlteraR atlid = do
    atleta <- runDB $ get404 atlid
    (widget,enctype) <- generateFormPost (formAtleta $ Just atleta)
    defaultLayout $ do
        [whamlet|
            <form action=@{AtletaAlteraR atlid} method=post>
                ^{widget}
                <input type="submit" value="Atualizar">
        |]

postAtletaAlteraR :: AtletaId -> Handler Html
postAtletaAlteraR atlid = do
    atleta <- runDB $ get404 atlid
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formAtleta $ Just atleta) 
    case res of
        FormSuccess atletaNovo -> do
            runDB $ replace atlid atletaNovo
            redirect TodosAtletasR
        _ -> redirect HomeLogadoR