{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Text.Lucius
import Text.Julius
import Import
import Prelude

getHomeR :: Handler Html
getHomeR = do
		defaultLayout $ do 
			addStylesheet $ StaticR css_bootstrap_css
			toWidgetHead $(juliusFile "templates/homepage.julius")
			toWidget $(luciusFile "templates/homepage.lucius")
			$(whamletFile "templates/homepage.hamlet")
