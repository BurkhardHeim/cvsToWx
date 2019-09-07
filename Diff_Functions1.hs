-- This module contains some NOT fully differentiable functions 
-- all the functions are indiffertentiable at x=5 ,at the x-axis
-- due to the 'bedingung' (5-x) -> 5 = x 
-- main purpose of this module is to provide functions for plotting

module Diff_Functions1
    where
    
    
 
--ist die grundfunktion in horizontaler richtung hat schönen wendepunkt
-- bei genau der haelfte deswegen 1. bezug zu zwei der Vier
-- und als 2.  ziel der horizontalen richtung
differenzwerte x = let a = x-1
                   in let bedingung = 5-x 
                      in a * (1-1/5)/ (x* bedingung)



--zu zwei der Vier 
differenzwerte4 :: Double -> Double
differenzwerte4 x = let a = x-1
                    in let bedingung = 5-x 
                       in  (3*a * (1-1/5))/ (2*(x* bedingung))    


-- zu Eins der Vier 
differenzwerte5 x = let a = x-1
                    in let bedingung = 5-x 
                       in  (a * (1-1/5))/ (2*(x* bedingung))


differenzwerte3 x = let a = x-1
                    in let bedingung = 5-x 
                       in  (3*a * (1-1/5))/ (5*(x* bedingung))   


 


differenzwerte6 x = let a = x-1
                    in let bedingung = 5-x 
                       in  (1/(a * (1-1/5)))/ (2*(x* bedingung))  

differenzwerte7 x = let a = x-1
                    in let bedingung = 5-x 
                       in  (a * (1-1/5))/ (3*(x* bedingung))

differenzwerte8 x = let a = x-1
                    in let bedingung = 5-x 
                       in  (a * (1-1/5))/ ((2/3)*(x* bedingung))

differenzwerte9 x = let a = x-1
                    in let bedingung = 5-x 
                       in  (a * (1-1/5))/ (5*(x* bedingung))

differenzwerte10 x = let a = x-1
                    in let bedingung = 5-x 
                       in  (2*a * (1-1/5))/ (5*(x* bedingung))







-- Zähler immer erste nummer
--testet in intervallen unter null
--
--
ermtodif1 x = if x < 0.13 then [x, x]
              else if x <= 0.133333333333333 then [(differenzwerte9 x), (differenzwerte4 x) ]
                else if x == 0.13 then [(differenzwerte9 x),(differenzwerte4 x)]
                  else if x <= 0.33333333333333333333333 then [ (differenzwerte5 x), (differenzwerte4 x )] 
                    else if x == 0.3 then [ (differenzwerte5 x), (differenzwerte4 x )] 
                      else if x == 0.4 then [ (differenzwerte9 x), (differenzwerte5 x )] 
                        else if x == 0.5 then [ (differenzwerte5 x), (differenzwerte x )] 
                          else if x <= 0.666666666666666666666666 then [ (differenzwerte x), (differenzwerte4 x )] 
                            else if x <= 0.83333333333333333333333 then [ (differenzwerte5 x), (differenzwerte4 x )] 
                              else [ x, x]
           
                      

ens x = let a = differenzwerte8 x
        in let b = differenzwerte4 x
           in [ a, b]

zwo x = let a = differenzwerte x
        in let b = differenzwerte5 x
           in [ a, b]
  
drei x = let a = differenzwerte4 x
         in let b = differenzwerte5 x
            in [ a, b]

funf x = let a = differenzwerte x
         in let b = differenzwerte9 x
            in [ a, b]
     
