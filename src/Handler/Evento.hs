{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Evento where

import Import

formEvento :: EsporteId -> Form Evento
formEvento espid = renderBootstrap $ Evento
    <$> areq textField "Nome: " Nothing
    <*> areq (selectField espLista) "Esporte: " Nothing
    <*> pure espid
    <*> areq dayField "Data: " Nothing

espLista = do
       entidades <- runDB $ selectList [] [Asc EsporteNome] 
       optionsPairs $ fmap (\ent -> (esporteNome $ entityVal ent, entityKey ent)) entidades

locLista = do
       entidades <- runDB $ selectList [] [Asc LocalNome] 
       optionsPairs $ fmap (\ent -> (localNome $ entityVal ent, entityKey ent)) entidades


getEventoR :: EsporteId -> Handler Html
getEventoR espid = do
    runDB $ get404 espid
    msg <- getMessage
    (widget,enctype) <- generateFormPost (formEvento espid)
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
            <form action=@{EventoR profid} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postEventoR :: EsporteId -> Handler Html
postEventoR espid = do 
    ((res,_),_) <- runFormPost (formEvento espid)
    case res of 
        FormSuccess evento -> do 
            _ <- runDB $ insert evento
            setMessage [shamlet|
                <h2>
                    EVENTO CADASTRADO COM SUCESSO!
            |]
            redirect HomeLogadoR
        _ -> redirect HomeR