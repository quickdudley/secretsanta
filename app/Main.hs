{-# LANGUAGE ApplicativeDo #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment
import System.IO
import System.Random
import Codec.Phaser
import Codec.Phaser.Common
import Codec.Phaser.UTF8

data Annotation =
  NoAnnotation |
  Already String |
  Assisted [String]

main :: IO ()
main = getArgs >>= \args -> case args of
  [] -> hPutStrLn stderr "Usage: secretsanta <filename>"
  _ -> forM_ args $ \fn -> parseFile
   (utf8_stream >># normalizeNewlines >># trackPosition >># efile_p) fn >>=
   \pf -> case pf  of
     Left el -> do
       hPutStrLn stderr $ "Problem reading \"" ++ fn ++ "\""
       forM_ el $ \(p,el') -> do
         hPutStrLn stderr $ "  " ++ show p
         forM_ el' $ \em ->
           hPutStrLn stderr $ "    " ++ em
     Right (m:_) -> do
       rng0 <- getStdGen
       let
         (rng1,cm) = completeMatching m rng0
         (rng2,fa) = chooseAssistance m cm rng1
       setStdGen rng2
       forM_ (M.toList fa) $ \(s,r) -> writeFile (s ++ ".txt") $
         intercalate "\n\r" $ map (\(ss,rcp) -> conj ss ++ rcp) $ M.toList r
 where
  conj "You" = "You are giving to "
  conj n = n ++ " is giving to "

completeMatching :: RandomGen g => M.Map String Annotation -> g ->
  (g, M.Map String String)
completeMatching m0 rng0 = case attemptMatching m0 rng0 of
  (rng1,Nothing) -> completeMatching m0 rng1
  (rng1,Just r) -> (rng1, r)

attemptMatching :: RandomGen g => M.Map String Annotation -> g ->
  (g,Maybe (M.Map String String))
attemptMatching sm rng0 = let
  us0 = M.keysSet $
    M.mapMaybe (\c -> case c of {Already _ -> Nothing; _ -> Just ()})
    sm
  ur0 = M.keysSet sm S.\\ S.fromList [x | Already x <- M.elems sm]
  m0 = M.mapMaybe (\an -> case an of {Already x -> Just x; _ -> Nothing}) sm
  go rng us ur m = if S.null us || S.null ur
    then (rng, Just m)
    else let
      (s,us') = S.deleteFindMin us
      (rng', r, ur') = randomSetMember ur rng
      in if s == r
        then (rng', Nothing)
        else go rng' us' ur' (M.insert s r m)
  in go rng0 us0 ur0 m0

chooseAssistance :: RandomGen g =>
  M.Map String Annotation -> M.Map String String -> g ->
  (g, M.Map String (M.Map String String))
chooseAssistance sm allocs rng0 = let
  assisted = M.mapMaybe
    (\a -> case a of {Assisted v -> Just v; _ -> Nothing})
    sm
  acc0 = M.mergeWithKey
    (\s a r -> case a of
      Assisted _ -> Nothing
      _ -> Just (M.singleton "You" r)
     ) (const M.empty) (const M.empty) sm allocs
  go acc ra rng
    | M.null ra = (rng,acc)
    | otherwise = let
      ((ap,hs'), ra') = M.deleteFindMin ra
      rp = allocs M.! ap
      hs = S.delete rp (S.fromList hs')
      (rng',h,_) = randomSetMember hs rng
      acc' = M.alter (fmap (M.insert ap rp)) h acc
      in go acc' ra' rng'
  in go acc0 assisted rng0

randomSetMember :: (RandomGen g, Ord a) => S.Set a -> g -> (g,a,S.Set a)
randomSetMember s rng = let
  (i0,rng') = randomR (1, S.size s) rng
  go i (s1:sr) r = case i `compare` S.size s1 of
    LT -> go i (S.splitRoot s1 ++ sr) r
    EQ -> let
      (e,s2) = S.deleteFindMax s1
      in (e, foldl' S.union (S.union r s2) sr)
    GT -> go (i - S.size s1) sr (S.union s1 r)
  (a,u) = go i0 (S.splitRoot s) S.empty
  in (rng', a, u)

isSpacing = (&&) <$> isSpace <*> (not . (`elem` "\n\r"))

efile_p :: Monoid p => Phase p Char o (M.Map String Annotation)
efile_p = go M.empty where
  go acc = do
    ~(s,r) <- p_p
    let acc' = M.insert s r acc
    return acc' <|> (char '\n' *> (return acc' <|> go acc'))
  p_p = do
    n <- n_p
    pure (n,NoAnnotation) <|> (do
      char ':'
      r <- n_p
      pure (n, Already r)
     ) <|> (do
      char '?'
      r <- sepBy n_p (char '/')
      pure (n, Assisted r)
     )
  n_p = munch isSpacing *> n1 id id
  n1 ca sa = get >>= \c -> case c of
    ':' -> fail "Unexpected \":\""
    '?' -> fail "Unexpected \"?\""
    '\\' -> fail "Names containing backslashes wouldn't work on Windows"
    '/' -> fail "Names containing forward slashes wouldn't work on Unix"
    _ | isSpacing c -> pure (ca []) <|> n1 ca (sa . (c:))
    _ | not (isSpace c) -> let
      ca' = ca . sa . (c:)
      in pure (ca' []) <|> n1 ca' id
    _ -> fail "Unexpected end of line"
