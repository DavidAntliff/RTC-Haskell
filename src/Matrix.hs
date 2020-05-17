{-# LANGUAGE FlexibleInstances #-}

module Matrix ( matrix22
              , matrix33
              , matrix44
              , elementAt
              ) where

import Control.Lens
import Linear ( M22, M33, M44
              , V2 (V2), V3 (V3), V4 (V4)
              , R2, R3, R4
              , _x, _y, _z, _w)
import Linear.Matrix (identity)

type Row2 = (Double, Double)
type Row3 = (Double, Double, Double)
type Row4 = (Double, Double, Double, Double)

data Matrix22 = Matrix22 (M22 Double) deriving (Show)
data Matrix33 = Matrix33 (M33 Double) deriving (Show)
data Matrix44 = Matrix44 (M44 Double) deriving (Show)

matrix22 :: (Row2, Row2) -> Matrix22
matrix22 ( (a00, a01)
         , (a10, a11) ) = Matrix22 (V2 (V2 a00 a01) (V2 a10 a11))

matrix33 :: (Row3, Row3, Row3) -> Matrix33
matrix33 ( (a00, a01, a02)
         , (a10, a11, a12)
         , (a20, a21, a22) ) = Matrix33 (V3 (V3 a00 a01 a02) (V3 a10 a11 a12) (V3 a20 a21 a22))

matrix44 :: (Row4, Row4, Row4, Row4) -> Matrix44
matrix44 ( (a00, a01, a02, a03)
         , (a10, a11, a12, a13)
         , (a20, a21, a22, a23)
         , (a30, a31, a32, a33) ) = Matrix44 (V4 (V4 a00 a01 a02 a03) (V4 a10 a11 a12 a13) (V4 a20 a21 a22 a23) (V4 a30 a31 a32 a33) :: M44 Double)

--elementAt :: (Int, Int) -> Matrix44 -> Double
--elementAt (row, col) m = getRowElement col $ getRow row m

class MatrixAccess a where
  elementAt :: (Int, Int) -> a -> Double
  
instance MatrixAccess Matrix22 where
  --elementAt (row, col) m = getRowElement col $ getRow row m
  elementAt (row, col) (Matrix22 m) = getVectorElement2 col $ getVectorElement2 row m

instance MatrixAccess Matrix33 where
  elementAt (row, col) (Matrix33 m) = getVectorElement3 col $ getVectorElement3 row m

instance MatrixAccess Matrix44 where
  --elementAt (row, col) m = getRowElement col $ getRow row m
  elementAt (row, col) (Matrix44 m) = getVectorElement4 col $ getVectorElement4 row m
  

-- Internal

getVectorElement2 idx v@(V2 _ _) = v ^. ind2 idx
getVectorElement3 idx v@(V3 _ _ _) = v ^. ind3 idx
getVectorElement4 idx v@(V4 _ _ _ _) = v ^. ind4 idx


--getVectorElement :: Int -> (t a) -> a
--getVectorElement idx v&(V4 Double _ _ _ _) = v ^. (ind4 idx)
--getVectorElement idx v&(V2 Double _ _) = v ^. (ind2 idx)
--getVectorElement idx (V4 v) = v ^. ind idx

--class VectorAccess a where
--  getVectorElement :: Int -> a -> b
--
--instance VectorAccess Matrix22 where
--  getVectorElement idx v = v ^. (ind2 idx)
--  
--instance VectorAccess (V2 Double) where
--  getVectorElement idx v = v ^. (ind2 idx)
--
--instance VectorAccess (V4 Double) where
--  getVectorElement idx v = v ^. (ind4 idx)

--getRow :: Int -> Matrix44 -> V4 Double
--getRow row (Matrix44 m) = m ^. (ind row)
--
--getRowElement :: Int -> V4 Double -> Double
--getRowElement col v = v ^. (ind col)

-- map V2 element indices to Lens
ind2 :: (Eq a1, Num a1, Functor f, R2 t) => a1 -> (a2 -> f a2) -> t a2 -> f (t a2)
ind2 0 = _x
ind2 1 = _y
ind2 _ = undefined

-- map V3 element indices to Lens
ind3 :: (Eq a1, Num a1, Functor f, R3 t) => a1 -> (a2 -> f a2) -> t a2 -> f (t a2)
ind3 0 = _x
ind3 1 = _y
ind3 2 = _z
ind3 _ = undefined

-- map V4 element indices to Lens
ind4 :: (Eq a1, Num a1, Functor f, R4 t) => a1 -> (a2 -> f a2) -> t a2 -> f (t a2)
ind4 0 = _x
ind4 1 = _y
ind4 2 = _z
ind4 3 = _w
ind4 _ = undefined


