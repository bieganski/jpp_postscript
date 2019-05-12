{-# LANGUAGE FlexibleInstances #-}

module Lib where

-- author: Mateusz Biegański
-- mb385162

import qualified Mon
import Data.Fixed
import Data.Bifunctor


type R = Rational

type R2 = (R, R)

data Vec = V R2 deriving (Eq, Show)
data Point = P R2 deriving (Eq, Show)

point :: R2 -> Point
point r2 = P r2

vec :: R2 -> Vec
vec r2 = V r2

zeror2 :: R2
zeror2 = (0, 0)

instance Mon.Mon R2 where
  m1 = zeror2
  (r11, r12) >< (r21, r22) = (r11 + r21, r12 + r22)

instance Mon.Mon Vec where
  m1 = vec Mon.m1
  V tup1 >< V tup2 = V (tup1 Mon.>< tup2)

instance Mon.Mon Point  where
  m1 = point Mon.m1
  P tup1 >< P tup2 = P (tup1 Mon.>< tup2)

data Transform = T [Either Vec R] deriving (Eq, Show) --przesunięcie o wektor bądź obrót w radianach

_pi = toRational pi

fullCircle :: R
fullCircle = 2 * _pi

translate :: Vec -> Transform
translate v = T [Left v]

rotate :: R -> Transform
rotate r = T [Right (r `mod'` fullCircle)]

-- Wikipedia: Bhaskara I's sine approximation formula
mysin :: R -> R
mysin x = 16 * x * (_pi - x) / (5 * _pi^2 - 4 * x * (_pi - x))

mycos :: R -> R
mycos x = (_pi^2 - 4 * x^2) / (_pi^2 + x^2)

_rotate :: R2 -> R -> R2
_rotate (x, y) r = (x * mycos(r) - y * mysin(r), y * mycos(r) + x * mysin(r))

-- łączy transformacje, redukując skrajne, o ile typy się zgadzają
-- założenie: niepuste transformaty jako argumenty
myjoin :: Transform -> Transform -> Transform
myjoin (T xs) (T ys) = case (last xs, head ys)
  of (Right r1, Right r2) -> stick (Right ((r1 + r2) `mod'` fullCircle))
     (Left v1, Left v2) -> stick (Left (v1 Mon.>< v2))
     (_, _) -> T (xs ++ ys)
     where stick mid = T ((take (length xs - 1) xs) ++ [mid] ++ (drop 1 ys))
           
instance Mon.Mon Transform where
  m1 = T []
  T xs >< T ys  | xs == [] || ys == [] = T (xs ++ ys)
                | otherwise = myjoin (T xs) (T ys)

trpoint :: Transform -> Point -> Point
trpoint (T []) pt = pt
trpoint (T (x:xs)) pt = case x of Left (V r2) -> (trpoint (T xs) pt) Mon.>< P r2
                                  Right rad -> P $ _rotate tpt rad where
                                     P tpt = trpoint (T xs) pt

trvec :: Transform -> Vec -> Vec
trvec (T []) v = v
trvec (T (x:xs)) v = case x of Left _ -> trvec (T xs) v
                               Right rad -> V $ _rotate tv rad where
                                 V tv = trvec (T xs) v

data Picture = Pic [[Point]] deriving Show

transform :: Transform -> Picture -> Picture
transform (T []) p = p
transform t (Pic pts) = Pic (map (map (trpoint t)) pts)

line :: (R,R) -> (R,R) -> Picture
line p1 p2 = Pic [[P p1, P p2]]

rectangle :: R -> R -> Picture
rectangle w h = Pic [[P (0,0), P (0, h), P (w, h), P (w, 0), P (0, 0)]]

(&) :: Picture -> Picture -> Picture
(&) (Pic p1) (Pic p2) = Pic (p1 ++ p2)

type IntLine = ((Int, Int), (Int, Int))

type IntRendering = [IntLine]

roundTuple :: (Point, Point) -> IntLine
roundTuple (P tup1, P tup2) = bimap f f (tup1, tup2) where
  f = ((bimap fromIntegral fromIntegral) . (bimap round round))

formatcurve :: [Point] -> [IntLine]
formatcurve [_] = []
formatcurve [p1, p2] = [roundTuple (p1, p2)]
formatcurve (p:ps) = (roundTuple (p, head ps)) : (formatcurve ps)

changeformat :: Picture -> IntRendering
changeformat (Pic pts) = foldr (++) [] (map formatcurve pts)

scalePoint :: Rational -> Point -> Point
scalePoint x (P tup) = P $ bimap (*x) (*x) tup

-- Obrazowanie przy danym współczynniku powiększenia
-- z zaokrągleniem do najbliższych wartości całkowitych
renderScaled :: Int -> Picture -> IntRendering
renderScaled _ (Pic []) = []
renderScaled scale (Pic pic) = changeformat (Pic scaled) where
  scaled = map (map (scalePoint (fromIntegral scale))) pic
