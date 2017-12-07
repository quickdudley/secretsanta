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
       let (rng1,cm) = completeMatching m rng0
       setStdGen rng1
       forM_ (M.toList cm) $ \(s,r) -> writeFile s r

completeMatching :: RandomGen g => M.Map String (Maybe String) -> g ->
  (g, M.Map String String)
completeMatching m0 rng0 = case attemptMatching m0 rng0 of
  (rng1,Nothing) -> completeMatching m0 rng1
  (rng1,Just r) -> (rng1, r)

attemptMatching :: RandomGen g => M.Map String (Maybe String) -> g ->
  (g,Maybe (M.Map String String))
attemptMatching sm rng0 = let
  us0 = M.keysSet $
    M.mapMaybe (\c -> case c of {Nothing -> Just (); _ -> Nothing})
    sm
  ur0 = M.keysSet sm S.\\ S.fromList [x | Just x <- M.elems sm]
  m0 = M.mapMaybe id sm
  go rng us ur m = if S.null us || S.null ur
    then (rng, Just m)
    else let
      (s,us') = S.deleteFindMin us
      (rng', r, ur') = randomSetMember ur rng
      in if s == r
        then (rng', Nothing)
        else go rng' us' ur' (M.insert s r m)
  in go rng0 us0 ur0 m0

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

efile_p :: Monoid p => Phase p Char o (M.Map String (Maybe String))
efile_p = go M.empty where
  go acc = do
    ~(s,r) <- p_p
    let acc' = M.insert s r acc
    return acc' <|> (char '\n' *> (return acc' <|> go acc'))
  p_p = do
    n <- n_p
    pure (n,Nothing) <|> (do
      char ':'
      r <- n_p
      pure (n, Just r)
     )
  n_p = munch isSpacing *> n1 id id
  n1 ca sa = get >>= \c -> case c of
    ':' -> fail "Unexpected \":\""
    '\\' -> fail "Names containing backslashes wouldn't work on Windows"
    '/' -> fail "Names containing forward slashes wouldn't work on Unix"
    _ | isSpacing c -> pure (ca []) <|> n1 ca (sa . (c:))
    _ | not (isSpace c) -> let
      ca' = ca . sa . (c:)
      in pure (ca' []) <|> n1 ca' id
    _ -> fail "Unexpected end of line"
