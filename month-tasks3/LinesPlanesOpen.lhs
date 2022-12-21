Именно эти задания подлежат проверке и обязательному исполнению в 2022 году


\begin{code}

module LinesPlanesOpen where

import AnGeo
import Lines
import LinesPlanes
\end{code}


Преобразование типов плоскостей:

\begin{code}
planeToCPlane :: Plane -> CPlane
planeToCPlane pl = CPl (vx $ normal pl) (vy $ normal pl) (vz $ normal pl) ( - ((mo pl) `sprod` (normal pl)))

cplaneToPlane :: CPlane -> Plane
cplaneToPlane pl | (aa pl) /= 0 = Pl (Vc (points pl) 0 0) (Vc (aa pl) (bb pl) (cc pl))
                 | (bb pl) /= 0 = Pl (Vc 0 (points pl) 0) (Vc (aa pl) (bb pl) (cc pl))
                 | (cc pl) /= 0 = Pl (Vc 0 0 (points pl)) (Vc (aa pl) (bb pl) (cc pl))
                    where points pl | (aa pl) /= 0 = -((dd pl)/(aa pl))
                                    | (bb pl) /= 0 = -((dd pl)/(bb pl))
                                    | (cc pl) /= 0 = -((dd pl)/(cc pl))
\end{code}

Красивое отображение канонической плоскости в виде уравнения (Ax + By + Cz + D = 0):

\begin{code}
instance Show CPlane where
  show cplane = show (aa cplane) ++ "x + " ++ show (bb cplane) ++ "y + " ++ show (cc cplane) ++ "z + " ++ show (dd cplane) ++ " = 0"
\end{code}

Проверка принадлежности точки плоскости (в обеих формах)

\begin{code}
pointOnPlane :: Point -> Plane -> Bool
pointOnPlane point pl = perp (vect point pl) (normal pl)
  where vect point pl = fromOrSeg $ OrS point (toPoint (mo pl))

pointOnCPlane :: Point -> CPlane -> Bool
pointOnCPlane point pl = if (normalForCPlane pl) `sprod` (fromPoint point) == - (dd pl) then True else False
\end{code}

Проверка принадлежности прямой плоскости
\begin{code}
lineOnPlane :: Line -> Plane -> Bool
lineOnPlane lin pl = if (pointOnPlane (toPoint $ ro lin) pl) && (perp (dir lin) (normal pl)) then True else False

lineOnCPlane :: Line -> CPlane -> Bool
lineOnCPlane lin pl = if (perp (normalForCPlane pl) (dir lin)) && (pointOnCPlane (toPoint $ ro lin) pl) then True else False
\end{code}

Проверка совпадения двух плоскостей

\begin{code}
instance Eq Plane where 
  (==) plane1 plane2 = (pointOnPlane (toPoint $ mo plane2) plane1) && (pointOnPlane (toPoint $ mo plane1) plane2) && (coll (normal plane1) (normal plane2))

instance Eq CPlane where
  (==) plane1 plane2 | ( (aa plane1) /= 0 ) && ( (aa plane2) /= 0 ) = ( (aa plane1)/(aa plane2) * (bb plane2) == (bb plane1) ) && ( (aa plane1)/(aa plane2) * (cc plane2) == (cc plane1) ) && ( (aa plane1)/(aa plane2) * (dd plane2) == (dd plane1) )
                     | ( (bb plane1) /= 0 ) && ( (bb plane2) /= 0 ) = ( (bb plane1)/(bb plane2) * (aa plane2) == (aa plane1) ) && ( (bb plane1)/(bb plane2) * (cc plane2) == (cc plane1) ) && ( (bb plane1)/(bb plane2) * (dd plane2) == (dd plane1) )
                     | ( (cc plane1) /= 0 ) && ( (cc plane2) /= 0 ) = ( (cc plane1)/(cc plane2) * (aa plane2) == (aa plane1) ) && ( (cc plane1)/(cc plane2) * (bb plane2) == (bb plane1) ) && ( (cc plane1)/(cc plane2) * (dd plane2) == (dd plane1) )
                     | otherwise = False
\end{code}

Проверка параллельности двух плоскостей

\begin{code}
planeParall :: Plane -> Plane -> Bool
planeParall plane1 plane2 = (normal plane1) `coll` (normal plane2)

cplaneParall :: CPlane -> CPlane -> Bool
cplaneParall plane1 plane2 = (normalForCPlane plane1) `coll` (normalForCPlane plane2)
\end{code}

Проверка перпендикулярности двух плоскостей

\begin{code}
planePerp :: Plane -> Plane -> Bool
planePerp plane1 plane2 = (normal plane1) `perp` (normal plane2)
\end{code}

\begin{code}
cplanePerp :: CPlane -> CPlane -> Bool
cplanePerp plane1 plane2 = (normalForCPlane plane1) `perp` (normalForCPlane plane2)
\end{code}

Проверка параллельности прямой и плоскости

\begin{code}
lineAndPlaneParall :: Line -> Plane -> Bool
lineAndPlaneParall lin pl = (dir lin) ┴ (normal pl)
\end{code}

\begin{code}
lineAndCPlaneParall :: Line -> CPlane -> Bool
lineAndCPlaneParall lin pl = (dir lin) ┴ (normalForCPlane pl)
\end{code}

Проверка перпендикулярности прямой и плоскости

\begin{code}
linePlanePerp :: Line -> Plane -> Bool
linePlanePerp lin pl = (dir lin) `coll` (normal pl)

lineCPlanePerp :: Line -> CPlane -> Bool
lineCPlanePerp lin pl = (dir lin) `coll` (normalForCPlane pl)
\end{code}

Нахождение угла между плоскостями (в градусах бы)...

\begin{code}
planeAngle :: Plane -> Plane  -> Double
planeAngle pl1 pl2 = 57.2958 * (acos $ (abs $ normal pl1 `sprod` normal pl2) / ((norma $ normal pl1) * (norma $ normal pl2)))

cplaneAngle :: CPlane -> CPlane  -> Double
cplaneAngle pl1 pl2 = 57.2958 * (acos $ (abs $ normalForCPlane pl1 `sprod` normalForCPlane pl2) / ((norma $ normalForCPlane pl1) * (norma $ normalForCPlane pl2)))
\end{code}

Нахождение угла между прямой и плоскостью (в градусах бы)...

\begin{code}
lineAndPlaneAngle :: Line -> Plane  -> Double
lineAndPlaneAngle lin pl = 90 - 57.296 * (acos $ (abs $ dir lin `sprod` normal pl) / ((norma $ dir lin) * (norma $ normal pl)))

lineAndCPlaneAngle :: Line -> CPlane  -> Double
lineAndCPlaneAngle lin pl = 90 - 57.296 * (acos $ (abs $ dir lin `sprod` normalForCPlane pl) / ((norma $ dir lin) * (norma $ normalForCPlane pl)))
\end{code}

Нахождение расстояния между точкой и плоскостью

\begin{code}
pointToPLaneDistance :: Point -> Plane -> Double
pointToPLaneDistance point pl = (1 / (norma $ normal pl)) * ( abs $ ( (normal pl) `sprod` (fromPoint point) ) - ((mo pl) `sprod` (normal pl)))

pointToCPLaneDistance :: Point -> CPlane -> Double
pointToCPLaneDistance point pl = (1 / (norm pl)) * ( abs $ ( (normalForCPlane pl) `sprod` (fromPoint point) ) + (dd pl))
  where norm pl = norma $ normalForCPlane pl
\end{code}

Нахождение линии пересечения двух плоскостей

\begin{code}
lineIntersectionOf2Planes :: Plane -> Plane -> Line
lineIntersectionOf2Planes plane1 plane2 = Ln (vect plane1 plane2 (dr plane1 plane2)) (dr plane1 plane2)
  where dr plane1 plane2 = (normal plane1) `vprod` (normal plane2)
        vect plane1 plane2 v | (vx v) /= 0 = Vc 0 (cY (planeToCPlane plane1) (planeToCPlane plane2)) (cZ (planeToCPlane plane1) (planeToCPlane plane2))
                             | (vy v) /= 0 = Vc (cX (planeToCPlane plane1) (planeToCPlane plane2)) 0 (cZ' (planeToCPlane plane1) (planeToCPlane plane2))
                             | (vz v) /= 0 = Vc (cX' (planeToCPlane plane1) (planeToCPlane plane2)) (cY' (planeToCPlane plane1) (planeToCPlane plane2)) 0
        cX plane1 plane2 = ( (dd plane2) * (cc plane1) - (dd plane1) * (cc plane2) ) / ( (aa plane1) * (cc plane2) - (aa plane2) * (cc plane1) )
        cY plane1 plane2 = ( (dd plane2) * (cc plane1) - (dd plane1) * (cc plane2) ) / ( (bb plane1) * (cc plane2) - (bb plane2) * (cc plane1) )
        cZ plane1 plane2 = ( (dd plane1) * (bb plane2) - (dd plane2) * (bb plane1) ) / ( (bb plane1) * (cc plane2) - (bb plane2) * (cc plane1) )
        cX' plane1 plane2 = ( (dd plane2) * (bb plane1) - (dd plane1) * (bb plane2) ) / ( (aa plane1) * (bb plane2) - (aa plane2) * (bb plane1) )
        cY' plane1 plane2 = ( (aa plane2) * (dd plane1) - (aa plane1) * (dd plane2) ) / ( (aa plane1) * (bb plane2) - (aa plane2) * (bb plane1) )
        cZ' plane1 plane2 = ( (dd plane1) * (aa plane2) - (dd plane2) * (aa plane1) ) / ( (aa plane1) * (cc plane2) - (aa plane2) * (cc plane1) )
\end{code}
