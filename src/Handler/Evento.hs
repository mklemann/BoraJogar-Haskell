{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Evento where

import Import
import Database.Persist.Postgresql

formEvento :: Maybe Evento -> Form Evento
formEvento mEvento = renderBootstrap $ Evento 
    <$> areq textField "Nome: " (fmap eventoNome mEvento)
    <*> areq (selectField espLista) "Esporte: " (fmap eventoEspid mEvento) 
    <*> areq (selectField locLista) "Local: " (fmap eventoLocalid mEvento)
    <*> areq textField "Hora: " (fmap eventoHora mEvento)
    <*> areq dayField "Data: " (fmap eventoData mEvento)
    
espLista = do
      entidades <- runDB $ selectList [] [Asc EsporteNome] 
      optionsPairs $ fmap (\ent -> (esporteNome $ entityVal ent, entityKey ent)) entidades

locLista = do
      entidades <- runDB $ selectList [] [Asc LocalNome] 
      optionsPairs $ fmap (\ent -> (localNome $ entityVal ent, entityKey ent)) entidades


getEventoR :: Handler Html
getEventoR = do 
    (widget,enctype) <- generateFormPost (formEvento Nothing)
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
            <div class="container">
                    <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                    
                    <h2 class="h2">
                        Cadastro de Atletas
                        
                    <img src=@{StaticR imgs_boraJogar_jpg} id="end">  
            
            
                             
            <br>
            <br>
            <br>
                <a href=@{HomeLogadoR}>
                    <input type="submit" value="Voltar">
           
                <div id="divExterna">
                    <div id="divCentral">
                        <div class="card" style="width: 250px; height: 400px;">
                            <ul class="list-group list-group-flush">
                                <li class="list-group-item">Entre com os dados do evento!
                                    <form action=@{EventoR} method=post class="form-inline">
                                        <div class="form-group row">
                                            ^{widget}
                                            <input type="submit" value="Cadastrar" class="btn btn-primary mb-2">
        |]

postEventoR :: Handler Html
postEventoR = do
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formEvento Nothing)
    case res of
        FormSuccess evento -> do
            runDB $ insert evento
            setMessage [shamlet|
                <h2>
                    EVENTO CADASTRADO COM SUCESSO!
            |]
            redirect HomeLogadoR
        _ -> redirect HomeLogadoR
    
-- SELECT * FROM Esporte

getTodosEventosR :: Handler Html
getTodosEventosR = do 
    eventos <- runDB $ selectList [] [Asc EventoNome]
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
            
        toWidget [lucius|
            body {
                background: rgb(173,216,230);
                background: linear-gradient(90deg, rgba(173,216,230,1) 0%, rgba(255,255,255,0) 20%, rgba(242,249,251,1) 80%, rgba(173,216,230,1) 100%);  
            }
         
            #init{
                float:left;
                widght:100px;
                height:150px;
            }
        
            #end{
                float: right;
                widght:100px;
                height:150px;
            }
            table{
                align:center;
            }
            a{
                align:center;
                flex:center;
            }
        |]
        [whamlet|
         <a href=@{HomeLogadoR}>            
               <div class="container">
                    <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                    
                     <h2 class="h2">
                        Lista de Eventos Cadastrados
                        
                    <img src=@{StaticR imgs_boraJogar_jpg} id="end"> 
        <table class="table">
            <thead class="thead-dark">
                <tr>
                    <th scope="col">
                         Nome
                    <th scope="col">
                        Hora
                    <th scope="col">
                        Data
                    <th scope="col">
                        Local
                    <th scope="col">
                        Esporte
                
            <tbody>
                $forall (Entity evid evento) <- eventos
                    <tr>
                        <td>
                            #{eventoNome evento}
                        <td>
                            #{eventoHora evento}
                        <td>
                            Dia
                        <td>
                            Lugar
                        <td>
                            Atividade
            <a href=@{EventoR}>
                <input type="button" value="Adicionar Evento">
              
            <a href=@{HomeLogadoR}>
                <input type="submit" value="Voltar">
            
        |]
-- getEventoPerfilR :: EventoId -> Handler Html
-- getEventoPerfilR evid = do 
--     evento <- runDB $ get404 evid
--     defaultLayout $ do 
--         [whamlet|
--             <a href=@{HomeLogadoR}>
--                 <input type="submit" value="Voltar">
--             <h1>
--                 Nome #{eventoNome evento}
--             <div>
--                 Esporte: #{eventoEspid evento}
--             <div>
--                 Local: #{eventoLocalid evento}
--             <div>
--                 Hora: #{eventoHora evento}
--             <div>
--                 Data: #{eventoData evento}
--         |]
-- postEventoApagarR :: EventoId -> Handler Html
-- postEventoApagarR evid = do
--     runDB $ get404 evid
--     runDB $ delete evid
--     redirect TodosEventosR

-- getEventoAlterarR :: EventoId -> Handler Html
-- getEventoAlterarR evid = do
--     evento <- runDB $ get404 evid
--     (widget,enctype) <- generateFormPost (formEvento $ Just evento)
--     defaultLayout $ do
--         [whamlet|
--             <form action=@{EventoAlterarR evid} method=post>
--                 ^{widget}
--                 <input type="submit" value="Atualizar">
--         |]

-- postEventoAlterarR :: EventoId -> Handler Html
-- postEventoAlterarR evid = do
--     evento <- runDB $ get404 evid
--     -- LE DO FORM
--     ((res,_),_) <- runFormPost (formEvento $ Just evento) 
--     case res of
--         FormSuccess eventoNovo -> do
--             runDB $ replace evid eventoNovo
--             redirect TodosEventosR
--         _ -> redirect HomeLogadoR

-- <tbody>
--         $forall (Entity evid evento) <- eventos
--             <tr>
--                 <td>
--                     <a href=@{EventoPerfilR evid}>
--                         #{eventoNome evento}
--                 <td>
--                     #{eventoEspid evento}
--                 <td>
--                     #{eventoLocalid evento}
--                 <td>
--                     #{eventoData evento}
--                 <td>
--                     #{eventoHora evento}
--                 <td>
--                     <a href=@{EventoAlterarR evid}>
--                         Editar
--                 <td>
--                     <form action=@{EventoApagarR evid} method=post>
--                         <input type="submit" value="X">