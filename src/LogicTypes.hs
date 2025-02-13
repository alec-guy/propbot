{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}  

module LogicTypes where 

-- import Control.Lens



import Data.Text as T
import Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import Data.Set as Set 
import Data.Map as Map
import Control.Monad (replicateM,sequence_)
import Data.Maybe (fromJust)  
import Data.Text.IO as TIO
import Calamity.Types.Model.Channel.Embed (Embed(..), EmbedImage(..))  
import Data.List as L


data Proposition = Var Char 
                 | And Proposition Proposition 
                 | Or Proposition Proposition
                 | Xor Proposition Proposition
                 | If Proposition Proposition
                 | Iff Proposition Proposition
                 | Not Proposition
                 | Nor Proposition Proposition 
                 | Nand Proposition Proposition
                 deriving (Eq, Show) 

data Argument = Argument 
              { premises   :: [Proposition] 
              , conclusion :: Proposition
              } deriving (Show, Eq)  

data TruthTable  = TT {headers :: [Text], rows :: [(Text,Bool)], validity :: Validity} deriving (Eq) 
    
data Validity = Valid | Invalid deriving (Eq) 

keyboard :: Text 
keyboard = "Negation: ¬" 
           <> "\nAnd: ∧" 
           <> "\nOr: ∨" 
           <> "\nIf Then: →" 
           <> "\nExclusive Disjunction: ⊻" 
           <> "\nBiconditional: ↔" 
           <> "\nNand ⊼"
           <> "\nNor ⊽"

collectVars :: Proposition -> Text 
collectVars prop = 
   case prop of 
    (Var c) -> T.singleton c 
    (And prop1 prop2)  -> (collectVars prop1) <> (collectVars prop2) 
    (Or prop1 prop2)   -> (collectVars prop1) <> (collectVars prop2) 
    (Xor prop1 prop2)  -> (collectVars prop1) <> (collectVars prop2)
    (Nand prop1 prop2) -> (collectVars prop1) <> (collectVars prop2)
    (Nor prop1 prop2)  -> (collectVars prop1) <> (collectVars prop2)
    (If  prop1 prop2)  -> (collectVars prop1) <> (collectVars prop2) 
    (Iff prop1 prop2)  -> (collectVars prop1) <> (collectVars prop2)  
    (Not prop)         -> collectVars prop

showProp :: Proposition -> Text 
showProp (Var c)           = T.singleton c 
showProp (And prop1 prop2) = (showProp prop1) <> "&" <> (showProp prop2)
showProp (Or prop1 prop2)  = (showProp prop1) <> "V" <> (showProp prop2) 
showProp (Xor prop1 prop2) = (showProp prop1) <> "xor" <> (showProp prop2)
showProp (Nand prop1 prop2) = (showProp prop1) <> "nand" <> (showProp prop2)
showProp (Nor prop1 prop2)  = (showProp prop1) <> "nor" <> (showProp prop2)
showProp (If prop1 prop2)  =  (showProp prop1) <> "->" <> (showProp prop2) 
showProp (Iff prop1 prop2) =  (showProp prop1) <> "<->" <> (showProp prop2) 
showProp (Not prop1)       = "~" <> (showProp prop1) 


gen :: Text -> [Bool] -> [(Char,Char)]
gen vars l =
  T.zip vars (T.pack $ Prelude.map (\b -> if b then '1' else '0') (l))

fromVarEval :: [(Char,Char)] -> Text 
fromVarEval evals = 
   case evals of
    []           -> T.empty 
    ((_,v) : vs) -> v `T.cons` (fromVarEval vs) 

evalProp :: Proposition -> [(Char,Char)] -> Bool 
evalProp (Var c) assignment           =  let m = fromJust (Prelude.lookup c assignment) 
                                         in if m == '1' then True else False 
evalProp (And prop1 prop2) assignment = (evalProp prop1 assignment) && (evalProp prop2 assignment)  
evalProp (Or prop1 prop2) assignment  = (evalProp prop1 assignment) || (evalProp prop2 assignment) 
evalProp (Xor prop1 prop2) assignment = (evalProp prop1 assignment) /= (evalProp prop2 assignment)
evalProp (Nand prop1 prop2) assignment = not ((evalProp prop1 assignment) && (evalProp prop2 assignment)) 
evalProp (Nor prop1 prop2) assignment = (not $ evalProp prop1 assignment) && (not $ evalProp prop2 assignment)
evalProp (If prop1 prop2) assignment  = (evalProp prop1 assignment) `if1` (evalProp prop2 assignment) 
                                        where if1 :: Bool -> Bool -> Bool 
                                              if1 True False = False
                                              if1 _    _     = True 
evalProp (Iff prop1 prop2) assignment = (evalProp prop1 assignment) == (evalProp prop2 assignment) 
evalProp (Not prop1)       assignment = not $ evalProp prop1 assignment

evalPremises :: [Proposition] -> [(Char,Char)] -> Text 
evalPremises props assignment = 
    case props of
     [] -> T.empty 
     (prop1 : props') -> (if (evalProp prop1 assignment) then '1' else '0') `T.cons` (evalPremises props' assignment)

 
f :: Argument -> [(Char,Char)] -> (Text, Bool)
f arg varEvals = 
  let prems = (evalPremises (premises arg) varEvals)  
  in ((fromVarEval varEvals) <> prems <> (evalPremises ([conclusion arg]) varEvals) , T.all (== '1') prems)
          
makeTruthTable :: Argument -> TruthTable 
makeTruthTable arg = 
   let vars         =  T.pack $ Set.toList $ Set.fromList $ T.unpack $ (mconcat $ Prelude.map collectVars ((conclusion arg) : premises arg)) 
       combos       =  (replicateM (T.length vars) [True,False])
       varRows      = (Prelude.map (gen vars) combos)
       rows'        = (f arg) <$> varRows 
   in TT 
      { headers = (T.pack <$> L.singleton <$> (T.unpack vars)) ++ (Prelude.map (\t -> "Premise:\n" <> showProp t ) (premises arg)) ++ ["Conclusion: \n" <> showProp (conclusion arg)] 
      , rows    =  rows'
      , validity = let removedVars = (L.drop (T.length vars)) <$> (T.unpack <$> fst <$> rows')
                       removeLastEl = (\l -> (\l2 -> (l2, L.last l)) . L.reverse . (L.drop 1) . L.reverse $ l) <$> removedVars  
                       cond         = let allTrue = L.filter (\(l,_) -> L.all (== '1') l) removeLastEl
                                      in case allTrue of
                                          [] -> True
                                          _  -> L.all (\(l,c) -> c == '1') allTrue 
                   in case cond of
                       True -> Valid
                       False -> Invalid
                                              
      } 
 


emptyEmbed :: Embed
emptyEmbed = Embed 
           { title = Nothing
           , type_ = Nothing
           , description = Nothing
           , url = Nothing
           , timestamp = Nothing
           , color = Nothing
           , footer = Nothing
           , image = Nothing
           , thumbnail = Nothing
           , video = Nothing
           , provider = Nothing
           , author = Nothing
           , fields = [] 
           } 

            
