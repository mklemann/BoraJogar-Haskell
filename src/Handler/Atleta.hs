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
        addStylesheet $ StaticR css_bootstrap_css
        
        toWidget [lucius|
            h1{
                color : blue;
                text-align: center;
                margin-top:50px;
            }
          
            #end{
                float: right;
                widght:100px;
                height:150px;
            }
             #init{
                float:left;
                widght:100px;
                height:150px;
            }
            span{
                align: center;
            }
            #formu{
                align:center;
                color:blue;
                flex-direction:center;
                
            }
            |]
        [whamlet|
                
            <div class="container">
                <h1 class>
                    <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                    
                    <span>
                        Bem Vindo ao Bora Jogar
                        
                    <img src=@{StaticR imgs_boraJogar_jpg} id="end">  
            
            
            <a href=@{HomeLogadoR}>
                <input type="submit" value="Voltar">
           
            <div class="container" id="formu">
              <form action=@{AtletaR} method=post class="form-inline">
                 <div class="form-group row">
                    ^{widget}
                    <input type="submit" value="Cadastrar" class="btn btn-primary mb-2">
                
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
            <a href=@{HomeLogadoR}>
                <input type="submit" value="Voltar">
            <h1>
                Atleta: #{atletaNome atleta}
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
            <a href=@{HomeLogadoR}>
                <input type="submit" value="Voltar">
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