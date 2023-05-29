{-# UnicodeSyntax #-}

\section{Прямые и плоскости в 3-мерном пространстве}

\begin{code}

module LinesPlanes where

import AnGeo
import Lines
-- import Data.Semigroup
-- import Data.Monoid

\end{code}


Зададим тип данных "плоскость", задаваемый скалярным произведением:
$$
(\vec{r}_0 - \vec{r}) \cdot \vec{n} = 0,
$$
т.е. описываем точки плоскости в которые приходит радиус-вектор $\vec{r}$ с помощью радиус-вектора начальной точки $\vec{r}_0$ и нормали $\vec{n}$.

\begin{code}
data Plane = Pl {mo, normal :: Vec} deriving (Read)
\end{code}

Зададим тип данных "каноническое уравнение плоскости" в соответствии с каноническим уравнением плоскости:
$$
Ax + By + Cz + D = 0.
$$

\begin{code}
data CPlane = CPl {aa,bb,cc,dd :: Double} deriving (Read)
\end{code}

Зададим функцию нахождения нормали для плоскости, заданной в канонической форме

\begin{code}
normalForCPlane :: CPlane -> Vec
normalForCPlane (CPl a b c _) = Vc a b c
-- normalForCPlane (CPl a b c d) = Vc a b c
\end{code}

Зададим функции-конструкторы плоскости:

\begin{code}
planeFromPointAndVec :: Point -> Vec -> Plane
planeFromPointAndVec p u = Pl (fromPoint p) u
\end{code}

(неплохо бы обдумать вырожденные случаи)

begin{code}
planeFrom3Points :: Point -> Point -> Point -> Plane
end{code}

begin{code}
planeFrom2Lines :: Line -> Line -> Plane
end{code}

Преобразование типов плоскостей:

begin{code}
planeToCPlane :: Plane -> CPlane
end{code}

begin{code}
cplaneToPlane :: CPlane -> Plane
end{code}

Красивое отображение канонической плоскости в виде уравнения:

begin{code}
instance Show CPlane where
  show cplane = ...
end{code}

Проверка принадлежности точки плоскости (в обеих формах)

begin{code}
pointOnPlane :: Point -> Plane -> Bool

pointOnCPlane :: Point -> CPlane -> Bool
end{code}

Проверка принадлежности прямой плоскости

begin{code}
lineOnPlane :: Point -> Plane -> Bool

lineOnPlane :: Point -> CPlane -> Bool
end{code}

Проверка совпадения двух плоскостей

begin{code}
instance Eq Plane where

instance Eq CPlane where
end{code}

Проверка параллельности двух плоскостей

begin{code}
planeParall  :: Plane -> Plane -> Bool

cPlaneParall :: CPlane -> CPlane -> Bool
end{code}

Проверка перпендикулярности двух плоскостей

\begin{code}
planePerp :: Plane -> Plane -> Bool
planePerp p1 p2 = (normal p1) `perp` (normal p2)
\end{code}

cPlanePerp :: CPlane -> CPlane -> Bool


Проверка параллельности прямой и плоскости

\begin{code}
lineAndPlaneParall :: Line -> Plane -> Bool
lineAndPlaneParall line plane = (dir line) ┴ (normal plane)
\end{code}

Проверка перпендикулярности прямой и плоскости

begin{code}
lineAndPlaneParall :: Line -> Plane -> Bool
end{code}

Нахождение угла между плоскостями (в градусах бы)...

begin{code}
planeAngle :: Plane -> Plane  -> Double
end{code}

Нахождение угла между прямой и плоскостью (в градусах бы)...

begin{code}
lineAndPlaneAngle :: Line -> Plane  -> Double
end{code}

Нахождение расстояния между точкой и плоскостью

begin{code}
pointToPLaneDistance :: Point -> Plane -> Double

pointToCPLaneDistance :: Point -> CPlane -> Double
end{code}

Нахождение линии пересечения двух плоскостей, заданных уравнением...

begin{code}
lineIntersectionOf2Planes :: Plane -> Plane -> Line
end{code}