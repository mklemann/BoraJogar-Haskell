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
            #divCentral {
                margin: 0 auto;
                width: 300px;
                height: 300px;
            }
            #divExterna{
                align-items: center;
                display: flex;
                flex-direction: row;
                flex-wrap: wrap;
                justify-content: center;
            }
            
        |]
        
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
            <div id="divExterna">
                <div id="divCentral">
                    <div class="card" style="width: 300px; height: 300px;">
                        <ul class="list-group list-group-flush">
                            <li class="list-group-item">Entre com seus dados!
                                <form action=@{LoginR} method=post>
                                    ^{widget}
                                    <input type="submit" value="entrar">
        
        |]
        
        toWidgetHead [julius|
            const div = document.getElementsByTagName("div");
            
            let teste = [];
            teste = [...div];
            
            console.log(div[0])
            console.log(div);
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