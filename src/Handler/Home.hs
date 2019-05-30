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
                float: left;
                margintop:20px;
            }
            img{
                float: right;
                widght:100px;
                height:150px
            }
            
        |]
        [whamlet|
            $maybe sessao <- sess    
                Ola #{sessao}
            $nothing    
                <h1 class>
                    <center>
                        BEM VINDO AO BORA JOGAR!
            <br>            
            <ul>
               
                    <li>
                        <a href=@{UsuarioR}>
                            <button>
                                Cadastrar Novo Usuario
                $maybe _ <- sess 
                    <li>
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Sair">
                $nothing
                
                    <li>
                        <a href=@{LoginR}>
                            <button>
                                Entrar
                    
               
               <img src=@{StaticR imgs_boraJogar_jpg}>
                        
            <button onclick="teste()"  class="btn btn-primary">
                OK
            
        |]

