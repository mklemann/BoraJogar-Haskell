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
    
    eventos <- runDB $ rawSql
        "SELECT ??,??,?? FROM Evento, Esporte, Local WHERE Evento.espid=Esporte.id AND Evento.localid=Local.id" []
        
            
    
    defaultLayout $ do
        -- pasta: static/css/bootstrap.css
        -- / e . sao trocados por _
        addStylesheet $ StaticR css_bootstrap_css
        
        toWidgetHead [julius|
        |]
        
        toWidget [lucius|
            body {
                background: rgb(173,216,230);
                background: linear-gradient(90deg, rgba(173,216,230,1) 0%, rgba(255,255,255,0) 20%, rgba(242,249,251,1) 80%, rgba(173,216,230,1) 100%);            }
    
            h1{
                color : green;
                text-align: center;
                margin-top:50px;
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
            table{
                align:center;
            }
          
        |]
        
        [whamlet|
            
            $maybe sessao <- sess    
                Ola #{sessao}
            $nothing 
            
            <div class="container">
                <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                
                <h2 class="h2">
                    Bem Vindo ao Bora Jogar
                    
                <img src=@{StaticR imgs_boraJogar_jpg} id="end">  
                    
        
            <br>
            <br>
            <br>

               <ul class="nav nav-tabs">
                  <li class="nav-item">
                    <a class="nav-link" href="@{AtletaR}">Cadastro de Atletas</a>
                  <li class="nav-item">
                    <a class="nav-link" href="@{TodosAtletasR}">Atletas Cadastrados</a>
                  <li class="nav-item">
                    <a class="nav-link" href="@{LocalR}">Cadastro de Locais</a>
                  <li class="nav-item">
                    <a class="nav-link" href="@{TodosLocaisR}">Locais  Cadastrados</a>
                  <li class="nav-item">
                    <a class="nav-link" href="@{EsporteR}">Cadastro de Esportes</a>
                  <li class="nav-item">
                    <a class="nav-link" href="@{TodosEsportesR}">Esportes Cadastrados</a>
                  <li class="nav-item">
                    <a class="nav-link" href="@{EventoR}">Cadastro de Eventos</a>
                  <li class="nav-item">
                    <a class="nav-link" href="@{TodosEventosR}">Eventos Cadastrados</a>
                  <li class="nav-item">

            $maybe _ <- sess 
                       
                <form action=@{LogoutR} method=post>
                    <input type="submit" value="Sair">
            $nothing  
            <br>
            <br>
            
            <table class="table">
                 <thead class="thead-dark">
                    <h3 style="text-align: center; ">
                        Lista de Eventos	                        
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
                    $forall (Entity evid evento,Entity _ esporte, Entity _ local) <- eventos
                        <tr>
                            <td>
                                #{eventoNome evento}
                            <td>
                                #{eventoHora evento}
                            <td>
                                DATA
                            <td>
                                #{localNome local}
                            <td>
                                #{esporteNome esporte}
        |]
        
        
