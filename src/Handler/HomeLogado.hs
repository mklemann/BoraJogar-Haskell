{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.HomeLogado where

import Import
import Database.Persist.Postgresql

-- SHAKESPEAREAN TEMPLATES
-- whamlet => html
-- julius => javascript
-- lucius|cassius => css
getHomeLogadoR :: Handler Html
getHomeLogadoR = do
    sess <- lookupSession "_ID"
    defaultLayout $ do
        -- pasta: static/css/bootstrap.css
        -- / e . sao trocados por _
        addStylesheet $ StaticR css_bootstrap_css
        toWidgetHead [julius|
            function teste(){
                alert("BORA JOGAR!");
            }
        |]
        toWidget [lucius|
            h1 {
                color : green;
            }
        |]
        [whamlet|
            $maybe sessao <- sess    
                Ola #{sessao}
            $nothing    
                <h1 class>
                    BEM VINDO AO BORA JOGAR!
            <ul>
               
                <li>
                    <a href=@{AtletaR}>
                        Cadastro de atleta
                <li>
                    <a href=@{TodosAtletasR}>
                        Listar atletas
                <li>
                    <a href=@{LocalR}>
                        Cadastro de Locais
                <li>
                    <a href=@{TodosLocaisR}>
                        Listar Locais
                <li>
                    <a href=@{EsporteR}>
                        Cadastro de Esportes
                <li>
                    <a href=@{TodosEsportesR}>
                        Listar Esportes
                <li>
                    <a href=@{EventoR}>
                        Cadastro de Eventos
                
                $maybe _ <- sess 
                    <li>
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Sair">
                $nothing
               
               <img src=@{StaticR imgs_boraJogar_jpg}>
                        
            <button onclick="teste()" class="btn btn-primary">
                OK
            
        |]

getPage1R :: Handler Html
getPage1R = do 
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 1
            
            <a href=@{HomeR}>
                Voltar
        |]

getPage2R :: Handler Html
getPage2R = do 
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 2
        |]

getPage3R :: Handler Html
getPage3R = do 
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 3
        |]