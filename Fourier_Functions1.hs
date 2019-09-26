module Fourier_Functions1
   where
import Control.Monad
import Data.List
import Data.Char


-----------------------------------------------------------------------------------------------------------------------------
-- APLLIES A FUNCTION to input data and exports them to wxmaxima
-- a function can be applied to evey data point (x y)
-- a differenzwerte function is inserted into another function
-- --------------------------------------------------------------

-- foselector is a variable the needs IO input (String) to select The Function to be processed 
-- export shall be place holder for the add or write function 
-- rReturn2 is the fallback link 
selecFUNC3 fosellector focomparePosition fodipfade4 fomore fowievieleMx fowoStehenMx fof6 fof7 fof8 fof9 fof10 fof11 fof12 fof13 fof14  = -- folaengeSet= --  rReturn2 =
                                                             if  fosellector== "1" 
                                                             then do
                                                               let pin0 = (show focomparePosition)
                                                               pin0
                                                             else if  fosellector =="2" 
                                                             then do
                                                               let pin = (show fodipfade4)
                                                               pin
                                                             else if  fosellector=="3"
                                                             then do 
                                                               let pin2 = (show fomore) 
                                                               pin2
                                                             else if  fosellector=="4"
                                                             then do 
                                                               let pin4 = (show fowievieleMx) 
                                                               pin4                 
                                                             else if  fosellector=="5"
                                                             then do 
                                                               let pin5 = (show fowoStehenMx)
                                                               pin5
                                                             else if fosellector=="6"
                                                             then do 
                                                               let pin6 = (show fof7) 
                                                               pin6  
                                                             else if fosellector=="7"
                                                             then do
                                                               let pin7 = (show fof7) 
                                                               pin7  
                                                             else if fosellector=="8"
                                                             then do
                                                               let pin8 = (show fof8) 
                                                               pin8  
                                                             else if fosellector=="9"
                                                             then do
                                                               let pin9 = (show fof9) 
                                                               pin9 
                                                             else if fosellector=="10"
                                                             then do
                                                               let pin10 = (show fof10) 
                                                               pin10 
                                                             else if fosellector=="11"
                                                             then do
                                                               let pin11 = (show fof11) 
                                                               pin11  
                                                            else if fosellector=="l2"
                                                             then do
                                                               let pin12 = (show fof12) 
                                                               pin12  
                                                            else if fosellector=="13"
                                                            then do
                                                               let pin13 = (show fof13) 
                                                               pin13  
                                                            else   
                                                            do
                                                               let pin14 = (show fof14) -- leere ausgabe
                                                               pin14
--selectFUNC4 :: [IO()] -> IO()
selectFUNC4 x func1 func2 func3 = do 
      let solongs = [1..3]
      --tmnnad <- forM (solongs) (\a -> do
        --   let keuze = drop (a-1 ) (take a [func])
           --do (head keuze)
      if x==1 
      then do
        func1
      else if x==2
      then do
        func2
      else if x==3
      then do
       func3
      else "" 
           --let willi 
           --let willi = ("") 
--selectFUNC4 :: [IO()] -> IO()
selectFUNC5 func = do 
      let solongs = let ste1 = length func
                    in [1..ste1]
      tmnnad <- forM (solongs) (\a -> do
         let keuze = head ( drop (a-1 ) (take a func))
         do (keuze)
         return (keuze))
      putStrLn (show "") --(concat tmnnad))
      
     
      --     return ())
     -- putStrLn ("")
--The MQ Random-Test Appraoch
--Due to the given functions :

--a similar incident shall be created that "fits" in Points
--out of dipfadre or even simulated data into the fourier123 fuction.9,0.2--The given dataset being a discret function in wxMaxima
--there will be two necessary steps, in order to ADD the DATA with  fourier123 and get an image of the 
--"fitted"v function in relation to fourie123
--
fofourier1 = [4,1,2]
fofourier2 = [3,5,7]
fofourier3 = [8,1,6]
fourier1 (x) = (sin((head fofourier1)*x)+ sin ((last (take 2 fofourier1))*x) + sin ((last fofourier1)*x))
fourier2 (x) = (sin((head fofourier2)*x)+ sin ((last (take 2 fofourier2))*x) + sin ((last fofourier2)*x))
fourier3 (x) = (sin((head fofourier2)*x)+ sin ((last (take 2 fofourier3))*x) + sin ((last fofourier3)*x))

fourier123 (x) = (fourier1 x + fourier2 x + fourier3 x)*(1/90)


-- one of only three: pandiagonal MQ 4x4 squares
fofourier1MQ4Pandiagonal = [0,7,9,14]
fofourier2MQ4Pandiagonal = [11,12,2,5]
fofourier3MQ4Pandiagonal = [6,1,5,8]
fofourier4MQ4Pandiagonal = [13,10,4,3]

fourier1MQ4 (x) = (sin ( head fofourier1MQ4Pandiagonal)*x)+(sin((head(drop 1 (take 2 ( fofourier1MQ4Pandiagonal))) )*x )) +  (sin((head(drop 2 (take 3 ( fofourier1MQ4Pandiagonal))))*x )) + (sin((head(drop 3 (take 4 ( fofourier1MQ4Pandiagonal))))*x ))  
fourier2MQ4 (x) = (sin ( head fofourier2MQ4Pandiagonal)*x)+(sin((head(drop 1 (take 2 ( fofourier2MQ4Pandiagonal))) )*x )) +  (sin((head(drop 2 (take 3 ( fofourier2MQ4Pandiagonal))))*x )) + (sin((head(drop 3 (take 4 ( fofourier2MQ4Pandiagonal))))*x ))  
fourier3MQ4 (x) = (sin ( head fofourier3MQ4Pandiagonal)*x)+(sin((head(drop 1 (take 2 ( fofourier3MQ4Pandiagonal))) )*x )) +  (sin((head(drop 2 (take 3 ( fofourier3MQ4Pandiagonal))))*x )) + (sin((head(drop 3 (take 4 ( fofourier3MQ4Pandiagonal))))*x )) 
fourier4MQ4 (x) = (sin ( head fofourier4MQ4Pandiagonal)*x)+(sin((head(drop 1 (take 2 ( fofourier4MQ4Pandiagonal))) )*x )) +  (sin((head(drop 2 (take 3 ( fofourier4MQ4Pandiagonal))))*x )) + (sin((head(drop 3 (take 4 ( fofourier4MQ4Pandiagonal))))*x ))  
 
fourierMQ4PAN x = ( fourier1MQ4 x + fourier2MQ4 x + fourier3MQ4 x + fourier4MQ4 x)

 

-- there are 3600 pandiagonal MQ5`S this is one
fofourier1MQ5Pandiagonal = [1,17,9,13,25]
fofourier2MQ5Pandiagonal = [14,23,5,16,7]
fofourier3MQ5Pandiagonal = [20,6,12,24,3]
fofourier4MQ5Pandiagonal = [22,4,18,10,11]
fofourier5MQ5Pandiagonal = [8,15,21,2,19]

fourier1MQ5 (x) = (sin ( head fofourier1MQ5Pandiagonal)*x)+(sin((head(drop 1 (take 2 ( fofourier1MQ5Pandiagonal))) )*x )) +  (sin((head(drop 2 (take 3 ( fofourier1MQ5Pandiagonal))))*x )) + (sin((head(drop 3 (take 4 ( fofourier1MQ5Pandiagonal))))*x ))  +  (sin((head(drop 4 (take 5 ( fofourier1MQ5Pandiagonal))))*x ))   

fourier2MQ5 (x)  =  (sin ( head fofourier2MQ5Pandiagonal)*x)+(sin((head(drop 1 (take 2 ( fofourier1MQ5Pandiagonal))) )*x ))  +  (sin((head(drop 2 (take 3 ( fofourier2MQ5Pandiagonal))))*x )) +(sin((head(drop 3 (take 4 ( fofourier2MQ5Pandiagonal))))*x )) + (sin((head(drop 4 (take 5 ( fofourier2MQ5Pandiagonal))))*x )) 

fourier3MQ5 (x) =   (sin ( head fofourier3MQ5Pandiagonal)*x)+(sin((head(drop 1 (take 2 ( fofourier1MQ5Pandiagonal))) )*x ))  + (sin((head(drop 2 (take 3 ( fofourier3MQ5Pandiagonal))))*x )) +(sin((head(drop 3 (take 4 ( fofourier3MQ5Pandiagonal))))*x )) + (sin((head(drop 4 (take 5 ( fofourier3MQ5Pandiagonal))))*x )) 
fourier4MQ5 (x) =  (sin ( head fofourier4MQ5Pandiagonal)*x)+(sin((head(drop 1 (take 2 ( fofourier1MQ5Pandiagonal))) )*x ))  +  (sin((head(drop 2 (take 3 ( fofourier4MQ5Pandiagonal))))*x )) +(sin((head(drop 3 (take 4 ( fofourier4MQ5Pandiagonal))))*x )) + (sin((head(drop 4 (take 5 ( fofourier4MQ5Pandiagonal))))*x )) 
fourier5MQ5 (x) =  (sin ( head fofourier5MQ5Pandiagonal)*x)+(sin((head(drop 1 (take 2 ( fofourier5MQ5Pandiagonal))))*x )) + (sin((head(drop 2 (take 3 ( fofourier5MQ5Pandiagonal))))*x )) +(sin((head(drop 3 (take 4 ( fofourier5MQ5Pandiagonal))))*x )) + (sin((head(drop 4 (take 5 ( fofourier5MQ5Pandiagonal))))*x ))

 
fourierMQ5PAN123 (x) = (fourier1MQ5 x + fourier2MQ5 x + fourier3MQ5 x + fourier4MQ5 x+ fourier5MQ5 x)*(1/90)
--there is supposed to be NO 6X6 panadiagonal MQ`s 
--just regular MQ6`s about ...? many ? 
fofourier1MQ6NOTPAN = [1,4,13,30,31,32]
fofourier2MQ6NOTPAN  = [35,34,8,23,9,2]
fofourier3MQ6NOTPAN  = [18,15,17,26,16,19]
fofourier4MQ6NOTPAN = [27,14,28,3,29,10] 
fofourier5MQ6NOTPAN  = [25,11,21,22,20,12]
fofourier6MQ6NOTPAN  = [5,33,24,7,6,36]

fourier1MQ6 (x) = (sin ( head fofourier1MQ6NOTPAN)*x)+(sin((head(drop 1 (take 2 ( fofourier1MQ6NOTPAN))) )*x )) +  (sin((head(drop 2 (take 3 ( fofourier1MQ6NOTPAN))))*x )) + (sin((head(drop 3 (take 4 ( fofourier1MQ6NOTPAN))))*x ))  +  (sin((head(drop 4 (take 5 ( fofourier1MQ6NOTPAN))))*x )) + ( sin ((last fofourier1MQ6NOTPAN)*x)) 
fourier2MQ6 (x) =  (sin ( head fofourier2MQ6NOTPAN)*x)+(sin((head(drop 1 (take 2 ( fofourier2MQ6NOTPAN))) )*x )) +  (sin((head(drop 2 (take 3 ( fofourier2MQ6NOTPAN))))*x )) + (sin((head(drop 3 (take 4 ( fofourier2MQ6NOTPAN))))*x ))  +  (sin((head(drop 4 (take 5 ( fofourier2MQ6NOTPAN))))*x )) + ( sin ((last fofourier2MQ6NOTPAN)*x)) 

fourier3MQ6 (x) =  (sin ( head fofourier3MQ6NOTPAN)*x)+(sin((head(drop 1 (take 2 ( fofourier3MQ6NOTPAN))) )*x ))  +  (sin((head(drop 2 (take 3 ( fofourier3MQ6NOTPAN))))*x )) + (sin((head(drop 3 (take 4 ( fofourier3MQ6NOTPAN))))*x ))  +  (sin((head(drop 4 (take 5 ( fofourier3MQ6NOTPAN))))*x )) + ( sin ((last fofourier3MQ6NOTPAN)*x)) 

fourier4MQ6 (x) =  (sin ( head fofourier4MQ6NOTPAN)*x)+(sin((head(drop 1 (take 2 ( fofourier4MQ6NOTPAN))) )*x )) +  (sin((head(drop 2 (take 3 ( fofourier4MQ6NOTPAN))))*x )) + (sin((head(drop 3 (take 4 ( fofourier4MQ6NOTPAN))))*x ))  +  (sin((head(drop 4 (take 5 ( fofourier4MQ6NOTPAN))))*x )) + ( sin ((last fofourier4MQ6NOTPAN)*x)) 

fourier5MQ6 (x) =  (sin ( head fofourier5MQ6NOTPAN)*x)+(sin((head(drop 1 (take 2 ( fofourier5MQ6NOTPAN))) )*x )) +  (sin((head(drop 2 (take 3 ( fofourier5MQ6NOTPAN))))*x )) + (sin((head(drop 3 (take 4 ( fofourier5MQ6NOTPAN))))*x ))  +  (sin((head(drop 4 (take 5 ( fofourier5MQ6NOTPAN))))*x )) + ( sin ((last fofourier5MQ6NOTPAN)*x)) 

fourier6MQ6 (x) =  (sin ( head fofourier6MQ6NOTPAN)*x)+(sin((head(drop 1 (take 2 ( fofourier6MQ6NOTPAN))) )*x ))  +  (sin((head(drop 2 (take 3 ( fofourier6MQ6NOTPAN))))*x )) + (sin((head(drop 3 (take 4 ( fofourier6MQ6NOTPAN))))*x ))  +  (sin((head(drop 4 (take 5 ( fofourier6MQ6NOTPAN))))*x )) + ( sin ((last fofourier6MQ6NOTPAN)*x)) 


fourierMQ6NOPAN123 (x) = (fourier1MQ6 x + fourier2MQ6 x + fourier3MQ6 x + fourier4MQ6 x+ fourier5MQ6 x+fourier6MQ6 x)*(1/90)





  


