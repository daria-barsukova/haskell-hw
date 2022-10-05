solver :: Double -> Double -> Double -> (Double, Double)
solver a b c | ((a == 0) && (b == 0)) = error "No"
             | ((a == 0) && (b /= 0)) = (x,x)
             | (a /= 0) = (x1,x2)
              where 
                d = b^2 - 4*a*c
                sd = if (d<0) 
                  then error "No roots" 
                  else sqrt d
                x1 = (-b - sd) / (2*a)
                x2 = (-b + sd) / (2*a)
                x = - c/b