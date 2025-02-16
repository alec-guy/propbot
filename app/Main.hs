{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-} 

module Main where
import LogicTypes
import Parser  
import Data.Text.IO as TIO
--import Data.Text.Internal.IO as TIIO
import Data.Text as T
import Data.Text.Lazy as TL 
-- import Control.Lens 
import Calamity 
import qualified Polysemy as P
import Calamity.Types.Model.Channel.Message as Message
import Calamity.Commands as Commands 
import Calamity.Commands.Context 
import Calamity.Metrics.Noop
import Calamity.Cache.InMemory 
import Text.Megaparsec as Megapars 
import Control.Monad (void)
import Di
import DiPolysemy
import Data.List as L 
import Data.Either
import Text.Blaze.Html
-- import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA  
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze as Blaze
import Data.String (fromString)
import System.Process 
import Data.ByteString as BS
import System.IO 
import Data.Word 
import Data.Char (ord)
import Data.Text.Encoding as E
 
{-
sToWords :: String -> [Word8]
sToWords s = (fromIntegral . ord) <$> s

sToBS :: String -> BS.ByteString 
sToBS s = BS.pack $ sToWords s 
-}
-- Hello 
main :: IO ()
main =  do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8 
  Di.new $ \di -> 
     void 
     . P.runFinal 
     . P.embedToFinal @IO  
     . (runDiToIO di) 
     . runCacheInMemory 
     . runMetricsNoop 
     . useFullContext
     . (useConstantPrefix "!") 
     . runBotIO (BotToken "") defaultIntents
     $ do 
        DiPolysemy.info @T.Text "Setting up commands and handlers.."  
        addCommands $ do
          helpCommand
          Commands.command @'[] "keyboard" $ \ctx -> void $ tell ctx (intoMsg keyboard)
          Commands.command @'[[T.Text]] "table" $ \ctx expressions1 -> 
               let response = case Megapars.parse parsePropositions "" (mconcat $ L.intersperse " " expressions1)of 
                               Left e -> Left $ errorBundlePretty e 
                               Right p -> Right $ ptToHtml $ makePropTable False p  
               in case response of 
                  Left s -> void $ tell ctx (intoMsg $ T.pack s)
                  Right ht -> do 
                       html' <- P.embed $ webpage "proptable.css" ht 
                       void $ P.embed  $ BS.writeFile "proptable.html" (E.encodeUtf8 $  TL.toStrict $ renderHtml html')
                       void $ P.embed $ (callProcess "wkhtmltoimage" [ "proptable.html", "proptable.jpg"])
                       tableBS <- P.embed $ BS.readFile "proptable.jpg"
                       void $ tell ctx (intoMsg $ messageOptions tableBS) 
          Commands.command @'[[T.Text]] "equiv" $ \ctx expressions -> 
              let response = case Megapars.parse parseEquiv "" (mconcat $ L.intersperse " " expressions) of 
                              Left e  -> Left (errorBundlePretty e)
                              Right p -> Right $ ptToHtml $ makePropTable True p

              in case response of 
                  Left  s  -> void $ tell ctx (intoMsg $ T.pack s)
                  Right ht -> do 
                       html' <- P.embed $ webpage "proptable.css" ht 
                       void $ P.embed  $ BS.writeFile "proptable.html" (E.encodeUtf8 $  TL.toStrict $ renderHtml html')
                       void $ P.embed $ (callProcess "wkhtmltoimage" [ "proptable.html", "proptable.jpg"])
                       tableBS <- P.embed $ BS.readFile "proptable.jpg"
                       void $ tell ctx (intoMsg $ messageOptions tableBS )

          Commands.command @'[[T.Text]] "check" $ \ctx argument -> 
              let truthtable = case Megapars.parse parseArgument "" (mconcat $ L.intersperse " " argument) of
                                Left e    -> case Megapars.parse parseErrorMsg "" (T.pack $ errorBundlePretty e) of
                                              Left errr -> Left (errorBundlePretty errr)
                                              Right m   -> Left m 
                                Right arg -> Right (makeTruthTable arg)  
              in case truthtable of 
                  Left s -> void $ tell ctx (intoMsg $ T.pack s) 
                  Right tt-> do
                               let table = ttToHtml tt
                               html' <- P.embed $ webpage "table.css" table
                               void $ P.embed  $ BS.writeFile "index.html" (E.encodeUtf8 $ TL.toStrict $ renderHtml html') 
                               void $ P.embed $ (callProcess "wkhtmltoimage" [ "index.html", "table.jpg"])
                               tableBS <- P.embed $ BS.readFile "table.jpg"
                               void $ tell ctx (intoMsg $ messageOptions tableBS )
                               
messageOptions :: ByteString -> CreateMessageOptions
messageOptions bs = CreateMessageOptions 
                  { content = Nothing
                  , nonce = Nothing
                  , tts   = Nothing
                  , attachments = Just $ [CreateMessageAttachment{filename = "table.png",description = Just "Truth-table", content = BS.fromStrict bs}]
                  , embeds =Nothing 
                  , allowedMentions = Nothing
                  , messageReference = Nothing
                  , components  = Nothing
                  } 
webpage :: FilePath -> H.Html -> IO H.Html
webpage filename table = do 
    css <- TIO.readFile filename
    return $ 
      H.docTypeHtml $ 
       (H.head $ (H.style $ (toHtml css)) <> (H.meta Blaze.! (HA.charset "UTF-8")))  <> 
         (H.body table) 

ptToHtml :: (PropTable,Maybe Bool) -> H.Html 
ptToHtml (proptable, equivalent) = 
    let headers' = case (headersP proptable) of 
                    (HeadersP{variablesP=v1,propositions = props})  -> 
                        H.tr 
                        $
                        (H.th (toHtml ("Variables" :: T.Text))) Blaze.! (HA.colspan $ fromString $ show $ (L.length v1))
                        <> 
                        (H.th (toHtml ("Propositions" :: T.Text)) Blaze.! (HA.colspan $ fromString $ show $ (L.length props)))
        table    = H.table 
                   $ 
                   H.thead 
                   $ 
                   (
                   headers' 
                   <> H.tr (mconcat $ [(H.th $ toHtml $ T.unpack v) Blaze.! (HA.style "min-width: 80px") | v <- (variablesP $ headersP proptable)]  
                                      <> 
                                      [(H.th $ toHtml $ T.unpack prop) Blaze.! (HA.style "min-width: 120px") | prop <- (propositions $ headersP proptable)]
                           )
                   ) 
                   <> 
                   (
                    H.tbody 
                    $ mconcat [H.tr $ mconcat cells 
                              | cells <- [ [let cellContent = (H.td $ toHtml $ if c == '1' then "True" :: T.Text else "False" :: T.Text) 
                                            in  cellContent
                                           | c <- (T.unpack r) 
                                           ] 
                                         | r <- ((rowsP proptable)) 
                                         ] 
                              ]
               )
    in case equivalent of 
        Nothing  -> (H.h2 $ toHtml ("Truth-Table" :: T.Text)) <> table 
        (Just b) -> (H.h2 $ toHtml ("These Propositions are Logicially " <> (if b then "Equivalent" :: T.Text else "Inequivalent" :: T.Text))  ) <> table

ttToHtml :: TruthTable -> H.Html  
ttToHtml tt = 
   let headers' = case headers tt of 
                   Headers{variables=v,prems=ps,conc=conclusionn} -> 
                        H.tr 
                        $ 
                        (H.th (toHtml ("Variables" :: T.Text))) Blaze.! (HA.colspan (fromString $ (show $ (L.length v) )))
                        <> 
                        (H.th (toHtml ("Premises" :: T.Text))) Blaze.! (HA.colspan (fromString $ (show $ ((L.length ps)) ) ))
                        <> 
                        (H.th (toHtml ("Conclusion" :: T.Text))) Blaze.! (HA.colspan "1")
       
       table = (  
               H.table
               $ 
               H.thead 
               $ 
               (
               headers'
               <> H.tr (mconcat $ [(H.th $ toHtml $ T.unpack v) Blaze.! (HA.style "min-width: 80px") | v <- (variables $ headers tt)] 
                                  <> 
                                  [(H.th $ toHtml $ T.unpack p) Blaze.! (HA.style "min-width: 120px") | p <- (prems $ headers tt)]
                                  <> 
                                  [((H.th $ toHtml $ T.unpack (conc $ headers tt)) Blaze.! (HA.style "min-width: 100"))]
                       )
               ) 
               <> 
               (
               H.tbody 
               $ mconcat  [H.tr $ mconcat cells 
                          | cells <- [ [let cellContent = (H.td $ toHtml $ if c == '1' then "True" :: T.Text else "False" :: T.Text) 
                                        in if trueprems then cellContent Blaze.! (HA.class_ "trueprems") else cellContent
                                       | c <- (T.unpack r) 
                                       ] 
                                     | (r, trueprems) <- (rows tt) 
                                     ] 
                          ]
               )

               )
    in  (H.h2 $ toHtml ("This Argument is Deductively " <> (T.pack $ show $ validity tt))  ) <> table
