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
            h1{
                color : orange;
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
          
        |]
        [whamlet|
            $maybe sessao <- sess    
                Ola #{sessao}
            $nothing    
            <div class="container">
                <h1 class>
                    <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                    
                    <span>
                        Bem Vindo ao Bora Jogar
                        
                    <img src=@{StaticR imgs_boraJogar_jpg} id="end">  
            <ul>
               
                <li>
                    <a href=@{AtletaR}>
                        Cadastro de Atletas
                <li>
                    <a href=@{TodosAtletasR}>
                        Listar Atletas
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
                <li>
                    <a href=@{TodosEventosR}>
                        Listar Eventos
                
                $maybe _ <- sess 
                    <li>
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Sair">
                $nothing
               
               
                        
            <button onclick="teste()" class="btn btn-primary">
                OK
            
        |]

