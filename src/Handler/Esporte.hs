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
        addStylesheet $ StaticR css_bootstrap_css
        
        toWidget [lucius|
            body {
                background: rgb(173,216,230);
                background: linear-gradient(90deg, rgba(173,216,230,1) 0%, rgba(255,255,255,0) 20%, rgba(242,249,251,1) 80%, rgba(173,216,230,1) 100%);  
            }
            
            #divCentral {
                margin: 0 auto;
                width: 300px;
                height: 300px;
                border: 1px;
            }
            #divExterna{
                align-items: center;
                display: flex;
                flex-direction: row;
                flex-wrap: wrap;
                justify-content: center;
            }
            
             div{
                align-items: center;
                display: flex;
                flex-direction:row;
                flex-wrap: wrap;
                justify-content: center;
            }
            
            #init{
                float:left;
                widght:100px;
                height:150px;
            }
          
            ul{
                display: flex;            
                flex-direction:row; 
            }
            
            span{
                align: center;
            }
          
            #end{
                float: right;
                widght:100px;
                height:150px;
            }
            input{
                margin: 10px;
            }
            
        |]

        [whamlet|
                    <a href=@{HomeLogadoR}>
                        <div class="container">
                            <h1 class>
                                <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                    
                                <h2 class="h2">
                                    Cadastro de Esportes
                        
                            <img src=@{StaticR imgs_boraJogar_jpg} id="end">  
                    
            <br>
            <br>
            <br>
            
                <div id="divExterna">
                    <div id="divCentral">
                        <div class="card" style="width: 230px; height: 300px;">
                            <ul class="list-group list-group-flush">
                                <li class="list-group-item">Entre com os dados do esporte!
                                    <form action=@{EsporteR} method=post>
                                        ^{widget}
                                        <input type="submit" value="cadastrar" class="btn btn-primary mb-2">
                                        <a href=@{HomeLogadoR}>
                                            <input value="Voltar" class="btn btn-primary mb-2">

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
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        
        toWidget [lucius|
            body {
                background: rgb(173,216,230);
                background: linear-gradient(90deg, rgba(173,216,230,1) 0%, rgba(255,255,255,0) 20%, rgba(242,249,251,1) 80%, rgba(173,216,230,1) 100%);  
            }
            
            #divCentral {
                margin: 0 auto;
                width: 300px;
                height: 300px;
                border: 1px;
            }
            #divExterna{
                align-items: center;
                display: flex;
                flex-direction: row;
                flex-wrap: wrap;
                justify-content: center;
            }
            
             div{
                align-items: center;
                display: flex;
                flex-direction:row;
                flex-wrap: wrap;
                justify-content: center;
            }
            
            #init{
                float:left;
                widght:100px;
                height:150px;
            }
          
            ul{
                display: flex;            
                flex-direction:row; 
            }
            
            span{
                align: center;
            }
          
            #end{
                float: right;
                widght:100px;
                height:150px;
            }
            input{
                margin: 10px;
            }
            
        |]
        [whamlet|
            <a href=@{HomeLogadoR}>
                        <div class="container">
                            <h1 class>
                                <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                    
                                <h2 class="h2">
                                    Lista de Esportes Cadastrados
                        
                            <img src=@{StaticR imgs_boraJogar_jpg} id="end"> 
            <table class="table">
                <thead class="thead-dark">
                    <tr>
                        <th scope="col">
                             Nome
                        <th scope="col">
                            Descricao
                        <th scope="col">
                             Participantes
                        
                
                <tbody>
                    $forall (Entity espid esporte) <- esportes
                        <tr>
                            <td>
                                <a href=@{EsportePerfilR espid}>
                                    #{esporteNome esporte}
                            <td>
                                #{esporteDescricao esporte}
                            <td>
                                #{esporteParticipantes esporte}
                            <td>
                                <a href=@{EsporteAlterarR espid}>
                                    Editar
                            <td>
                                <form action=@{EsporteApagarR espid} method=post>
                                    <input type="submit" value="X">
                <a href=@{EsporteR}>
                    <input type="button" value="Adicionar Esporte">
                <a href=@{HomeLogadoR}>
                    <input type="submit" value="Voltar">
        |]

getEsportePerfilR :: EsporteId -> Handler Html
getEsportePerfilR espid = do 
    esporte <- runDB $ get404 espid
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        
        toWidget [lucius|
            body {
                background: rgb(173,216,230);
                background: linear-gradient(90deg, rgba(173,216,230,1) 0%, rgba(255,255,255,0) 20%, rgba(242,249,251,1) 80%, rgba(173,216,230,1) 100%);  
            }
            
            #divCentral {
                margin: 0 auto;
                width: 300px;
                height: 300px;
                border: 1px;
            }
            #divExterna{
                align-items: center;
                display: flex;
                flex-direction: row;
                flex-wrap: wrap;
                justify-content: center;
            }
            
             div{
                align-items: center;
                display: flex;
                flex-direction:row;
                flex-wrap: wrap;
                justify-content: center;
            }
            
            #init{
                float:left;
                widght:100px;
                height:150px;
            }
          
            ul{
                display: flex;            
                flex-direction:row; 
            }
            
            span{
                align: center;
            }
          
            #end{
                float: right;
                widght:100px;
                height:150px;
            }
            input{
                margin: 10px;
            }
            
        |]
        [whamlet|
            <a href=@{HomeLogadoR}>
                <div class="container">
                            <h1 class>
                                <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                    
                                <h2 class="h2">
                                    Cadastro de Esportes
                        
                            <img src=@{StaticR imgs_boraJogar_jpg} id="end"> 
            <h2>
                Nome: #{esporteNome esporte}
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
            <a href=@{HomeLogadoR}>
                <input type="submit" value="Voltar">
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