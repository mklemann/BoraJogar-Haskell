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
            
            a{
                 margin:20px;
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
                    
        
                    
            <nav class="navbar navbar-expand-lg navbar-light bg-light">
                <a class="navbar-brand" href=@{HomeR}>
                    Inicial
                <div class="collapse navbar-collapse id="navbarNavAltMarkup>
                
                
                    <div class="navbar-nav">
               
                       
                            <a href=@{AtletaR} class="nav-link" >
                                Cadastro de Atletas
                       
                            <a href=@{TodosAtletasR} class="nav-link" >
                                Listar Atletas
                        
                            <a href=@{LocalR} class="nav-link" >
                                Cadastro de Locais
                        
                            <a href=@{TodosLocaisR} class="nav-link" >
                                Listar Locais
                       
                            <a href=@{EsporteR} class="nav-link" >
                                Cadastro de Esportes
                      
                            <a href=@{TodosEsportesR} class="nav-link" >
                                Listar Esportes
                       
                            <a href=@{EventoR} class="nav-link" >
                                Cadastro de Eventos
                       
                            <a href=@{TodosEventosR} class="nav-link" >
                                Listar Eventos
                
            $maybe _ <- sess 
                       
                <form action=@{LogoutR} method=post>
                    <input type="submit" value="Sair">
            $nothing  
               
               
                        
            
            
        |]