{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Database.Persist.Postgresql

-- <$>, <*> CAP 7.4 do LIVRO!
formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap $ (,)
    <$> (Usuario 
        <$> areq emailField "E-mail:" Nothing
        <*> areq passwordField "Senha: " Nothing)
    <*> areq passwordField "Confirmacao: " Nothing  

getUsuarioR :: Handler Html
getUsuarioR = do 
    (widget,enctype) <- generateFormPost formUsuario
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
            
        |]
    
    
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
                
            <div class="container">
                    <img src=@{StaticR imgs_boraJogar_jpg} id="init">
                    
                    <h2 calss="h2">
                        Entre com seus dados para o cadastro
                        
                    <img src=@{StaticR imgs_boraJogar_jpg} id="end">  
                        
            <br>
            <br>
            <br>
        
                <a href=@{HomeLogadoR}>
                    <input type="submit" value="Voltar">

                <div id="divExterna">
                    <div id="divCentral">
                        <div class="card" style="width: 230px; height: 300px;">
                            <ul class="list-group list-group-flush">
                                <li class="list-group-item">Entre com seus dados!
                                    <form action=@{UsuarioR} method=post>
                                        ^{widget}
                                        <input type="submit" value="cadastrar">
        |]

postUsuarioR :: Handler Html
postUsuarioR = do
    ((res,_),_) <- runFormPost formUsuario
    case res of
        FormSuccess (usuario,confirmacao) -> do
            if (usuarioSenha usuario) == confirmacao then do 
                runDB $ insert usuario
                setMessage [shamlet|
                <h5 style="text-align: center;" class="h5">
                    USUARIO CADASTRADO COM SUCESSO!
            |]
                redirect HomeR
            else do
                setMessage [shamlet|
                    <h1>
                        Usuario e senha nao batem
                |]
                redirect UsuarioR
        _ -> redirect HomeR
    