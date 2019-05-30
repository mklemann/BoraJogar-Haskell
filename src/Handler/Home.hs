{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Database.Persist.Postgresql

-- SHAKESPEAREAN TEMPLATES
-- whamlet => html
-- julius => javascript
-- lucius|cassius => css
getHomeR :: Handler Html
getHomeR = do
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
                
                margin-top:80px;
            }
            #end{
                float: right;
                widght:100px;
                height:150px;
            }
            div{
                align-items: center;
                display: flex;
                flex-direction:column;
                flex-wrap: wrap;
                justify-content: center;
            }
            #init{
                float:left;
                widght:100px;
                height:150px;
            }
          
            ul{
               flex-direction:column; 
            }
            
        |]
        [whamlet|
            $maybe sessao <- sess    
                Ola #{sessao}
            $nothing    
                <h1 class>
                    <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                    
                        Bem Vindo ao Bora Jogar
                        
                    <img src=@{StaticR imgs_boraJogar_jpg} id="end">        
            <br>
            <br>
            <br>
            <br>
            <br>
            <br>
            <br>
        <div>    
            <ul class="list-group">
               <br>
                    <li class="list-group-item">
                        <a href=@{UsuarioR}>
                            <button>
                                Cadastrar Novo Usuario
                $maybe _ <- sess 
                    <li class="list-group-item">
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Sair">
                $nothing
                
                    <li class="list-group-item">
                        <a href=@{LoginR}>
                            <button>
                                Entrar
                    
        |]

