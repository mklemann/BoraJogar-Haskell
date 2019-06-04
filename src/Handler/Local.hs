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
                        <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                
                        <h2 class="h2">
                            Cadastro de Locais
                    
                        <img src=@{StaticR imgs_boraJogar_jpg} id="end">  

                    
            <br>
            <br>
            <br>
            
                
                   

                <div id="divExterna">
                    <div id="divCentral">
                        <div class="card" style="width: 230px; height: 300px;">
                            <ul class="list-group list-group-flush">
                                <li class="list-group-item">Entre com os dados do local!
                                    <form action=@{LocalR} method=post>
                                        ^{widget}
                                        <input type="submit" value="cadastrar">

                
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
                    <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                
                    <h2 class="h2">
                        Lista de Locais Cadastrados
                    
                    <img src=@{StaticR imgs_boraJogar_jpg} id="end">  
            <table class="table">
                <thead class="thead-dark">
                     <tr>
                        <th scope="col">
                             Nome
                        <th scope="col">
                            Descricao
                        <th scope="col">
                             Endereco
            
                
                <tbody>
                    $forall (Entity locid local) <- locais
                        <tr>
                            <td>
                                <a href=@{LocalPerfilR locid}>
                                    #{localNome local}
                            <td>
                                #{localDescricao local}
                            <td>
                                #{localEndereco local}
                            <td>
                                <a href=@{LocalAlterarR locid}>
                                    Editar
                            <td>
                                <form action=@{LocalApagarR locid} method=post>
                                    <input type="submit" value="X">
                            
                <a href=@{LocalR}>
                    <input type="button" value="Adicionar Local">
              
                <a href=@{HomeLogadoR}>
                    <input type="submit" value="Voltar">
        |]
        
        
        
getLocalPerfilR :: LocalId -> Handler Html
getLocalPerfilR locid = do 
    local <- runDB $ get404 locid
    defaultLayout $ do 
        [whamlet|
            <a href=@{HomeLogadoR}>
                <input type="submit" value="Voltar">
            <h1>
                Nome: #{localNome local}
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