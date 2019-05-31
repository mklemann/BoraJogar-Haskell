{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Esporte where

import Import
import Database.Persist.Postgresql

-- <$>, <*> CAP 7.4 do LIVRO!
formEsporte :: Maybe Esporte -> Form Esporte
formEsporte mEsporte = renderBootstrap $ Esporte 
    <$> areq textField "Nome: " (fmap esporteNome mEsporte)
    <*> areq textField  "Descricao: " (fmap esporteDescricao mEsporte)
    <*> areq intField "Quantidade de participantes: " (fmap esporteParticipantes mEsporte)
    
-- ^ coloca outro html, no caso, os inputs
getEsporteR :: Handler Html
getEsporteR = do 
    (widget,enctype) <- generateFormPost (formEsporte Nothing)
    defaultLayout $ do
        [whamlet|
            <a href=@{HomeLogadoR}>
                <input type="submit" value="Voltar">
            <form action=@{EsporteR} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postEsporteR :: Handler Html
postEsporteR = do
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formEsporte Nothing)
    case res of
        FormSuccess esporte -> do
            runDB $ insert esporte
            setMessage [shamlet|
                <h2>
                    ESPORTE CADASTRADO COM SUCESSO!
            |]
            redirect HomeLogadoR
        _ -> redirect HomeLogadoR
    
-- SELECT * FROM Esporte
getTodosEsportesR :: Handler Html
getTodosEsportesR = do 
    esportes <- runDB $ selectList [] [Asc EsporteNome]
    defaultLayout $(whamletFile "templates/esporte.hamlet")

getEsportePerfilR :: EsporteId -> Handler Html
getEsportePerfilR espid = do 
    esporte <- runDB $ get404 espid
    defaultLayout $ do 
        [whamlet|
            <h1>
                Nome #{esporteNome esporte}
            <div>
                Descricao: #{esporteDescricao esporte}
            <div>
                Participantes: #{esporteParticipantes esporte}
        |]

postEsporteApagarR :: EsporteId -> Handler Html
postEsporteApagarR espid = do
    runDB $ get404 espid
    runDB $ delete espid
    redirect TodosEsportesR

getEsporteAlterarR :: EsporteId -> Handler Html
getEsporteAlterarR espid = do
    esporte <- runDB $ get404 espid
    (widget,enctype) <- generateFormPost (formEsporte $ Just esporte)
    defaultLayout $ do
        [whamlet|
            <form action=@{EsporteAlterarR espid} method=post>
                ^{widget}
                <input type="submit" value="Atualizar">
        |]

postEsporteAlterarR :: EsporteId -> Handler Html
postEsporteAlterarR espid = do
    esporte <- runDB $ get404 espid
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formEsporte $ Just esporte) 
    case res of
        FormSuccess esporteNovo -> do
            runDB $ replace espid esporteNovo
            redirect TodosEsportesR
        _ -> redirect HomeLogadoR