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
            h2{
                color:red;
                align:center;
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
            <form action=@{EventoR} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
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
    defaultLayout $(whamletFile "templates/evento.hamlet")
    
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