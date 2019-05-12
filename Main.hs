-- author: Mateusz Biegański
-- mb385162

module Main where

import System.Environment
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Text.Read (readMaybe)
import Data.Bifunctor
import Data.Foldable
import Data.Functor.Identity
import Control.Monad

import Mon
import Lib

data SState = S { stack :: [R],
                  curpoint :: Maybe Point,
                  curpath :: [Point],
                  transforms :: Transform,
                  picture :: Picture
                }
            deriving Show

type StateAlias = State SState -- równoważnie StateT SState Identity

type SStateMon = ExceptT String StateAlias

push :: R -> SStateMon ()
push x = modify (\s -> s {stack = x:(stack s)})

pop :: SStateMon R
pop = do
  lst <- gets stack
  if lst == []
    then throwError "empty pop"
  else
    modify (\s -> s {stack = drop 1 (stack s)})
  return (head lst)

-- zmienia kolejność, tj. wierzchołek stosu jest w snd
pop2 :: SStateMon R2
pop2 = do
  a1 <- pop
  a2 <- pop
  return (a2, a1)

setcurpoint :: Point -> SStateMon ()
setcurpoint pt = modify (\s -> s {curpoint = Just pt})

newpath :: Point -> SStateMon ()
newpath p = modify (\s -> s {curpath = [p]})

torat :: (Int, Int) -> (R, R)
torat pt = bimap fromIntegral fromIntegral pt

addline :: Point -> Point -> SStateMon ()
addline p1 p2 = do
  modify ( \s -> s {curpath = p2:(curpath s)} )
  modify ( \s -> s {picture = (picture s) & Pic [[p1, p2]]} )
  setcurpoint p2

addtransform :: Transform -> SStateMon ()
addtransform t = modify (\s -> s {transforms = (transforms s) >< t})

doadd, dosub, domul, dodiv :: SStateMon ()

doadd = pop2 >>= push . (uncurry (+))

dosub = pop2 >>= push . (uncurry (-))

domul = pop2 >>= push . (uncurry (*))

dodiv = pop2 >>= (\tup -> if snd tup == 0 then throwError "div by 0" else pure (uncurry (/) tup)) >>= push

tpoint :: Point -> SStateMon Point
tpoint pt = do
  t <- gets transforms
  return $ trpoint t pt

moveto :: SStateMon ()
moveto = do
  pt <- pop2
  tpt <- tpoint $ P pt
  setcurpoint tpt
  newpath tpt

lineto :: SStateMon ()
lineto = do
  to <- pop2
  tto <- tpoint $ P to
  from <- gets curpoint
  maybe (throwError "current point") (flip addline tto) from

closepath :: SStateMon ()
closepath = do
  from <- gets curpoint
  pth <- gets curpath
  when (length pth > 1) (addline (fromJust from) (last pth)) -- przepraszam za to fromJust

dotranslate :: SStateMon ()
dotranslate = do
  tup <- pop2
  addtransform $ translate $ V tup

dorotate :: SStateMon ()
dorotate = do
  degs <- pop
  addtransform $ rotate $ degs / 360 * fullCircle

parse :: String -> SStateMon ()
parse s | s == "add" = doadd
        | s == "sub" = dosub
        | s == "div" = dodiv
        | s == "mul" = domul
        | s == "moveto" = moveto
        | s == "lineto" = lineto
        | s == "closepath" = closepath
        | s == "translate" = dotranslate
        | s == "rotate" = dorotate
        | otherwise = maybe (throwError "input parse") (return . fromIntegral) (readMaybe s) >>= push

strtuple :: (Int, Int) -> String
strtuple (a, b) = (show a) ++ " " ++ (show b)

printContents :: IntRendering -> IO ()
printContents [] = return ()
printContents (x:xs) = putStrLn ((strtuple $ fst x) ++ " moveto " ++ (strtuple $ snd x) ++ " lineto")
  >> printContents xs

printError :: IO ()
printError = putStrLn "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"

doPrint :: Maybe IntRendering -> IO ()
doPrint lines = do
  putStrLn "300 400 translate"
  maybe printError printContents lines
  putStrLn "stroke showpage"

s0 = S [] Nothing [] (T []) (Pic [])

runSStateMon :: SStateMon a -> Either String a
runSStateMon computation = fst $ runIdentity res where
  res = ((flip runStateT s0) . runExceptT) computation

getscale :: IO Int
getscale  = do
  args <- getArgs
  if length args == 0 then return 1 else
    let s = readMaybe $ head args :: Maybe Int in
      maybe (error "argument musi być liczbą całkowitą!") (\a -> return a) s

main :: IO ()
main = do
  scale <- getscale
  input <- getContents
  let comp = traverse parse (words input)
  let res = runSStateMon (comp >> gets picture)
  case res of Left s    -> doPrint Nothing
              Right pic -> doPrint $ Just $ renderScaled scale pic
