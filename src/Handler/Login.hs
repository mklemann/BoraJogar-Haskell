{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Database.Persist.Postgresql

formLogin :: Form Usuario
formLogin = renderBootstrap $ Usuario 
        <$> areq emailField "E-mail:" Nothing
        <*> areq passwordField "Senha: " Nothing
    
getLoginR :: Handler Html
getLoginR = do 
    (widget,enctype) <- generateFormPost formLogin

    msg <- getMessage
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
            input: {
                margin: 5px;
            }
            
        |]
        
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}

            <div class="container">
                    <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                    
                    <h2 calss="h2">
                        Entre com seus dados para logar
                        
                    <img src=@{StaticR imgs_boraJogar_jpg} id="end">  
                        
            <br>
            <br>
            <br>


                <div id="divExterna">
                    <div id="divCentral">
                        <div class="card" style="width: 230px; height: 300px;">
                            <ul class="list-group list-group-flush">
                                <li class="list-group-item">Entre com seus dados!
                                    <form action=@{LoginR} method=post>
                                        ^{widget}
                                        <input type="submit" value="entrar" class="btn btn-primary mb-2">
                                        <a href=@{HomeR}>
                                            <input type="submit" value="Voltar" class="btn btn-primary mb-2">
        |]


postLoginR :: Handler Html
postLoginR = do
    ((res,_),_) <- runFormPost formLogin
    case res of
        FormSuccess (Usuario "root@root123.com" "root") -> do 
            setSession "_ID" "root"
            redirect AdminR
        FormSuccess usuario -> do
            usuBanco <- runDB $ getBy $ UniqueRestEmail (usuarioEmail usuario)
            case usuBanco of 
                Just usuarioValido -> do 
                    if ((usuarioSenha usuario) == (usuarioSenha $ entityVal usuarioValido)) then do 
                        setSession "_ID" (usuarioEmail $ entityVal usuarioValido)
                        redirect HomeLogadoR
                    else do
                        setMessage [shamlet|
                            <h1>
                                Senha invalida
                        |]
                        redirect LoginR
                        
                Nothing -> do
                    setMessage [shamlet|
                        Usuario não encontrado
                    |]
                    redirect LoginR
        _ -> redirect HomeR
    
postLogoutR :: Handler Html
postLogoutR = do 
    deleteSession "_ID"
    redirect HomeR
    
getAdminR :: Handler Html
getAdminR = do 
    defaultLayout $ do 
        [whamlet|
            <h1>
                OLA SUPER USUARIO, VOCE TEM O CONTROLE TOTAL DO SISTEMA!!!
        |]