module Main
   where

import qualified UsefulFunctions  as Us   
import qualified WriteWXmaximaFile as WX
import qualified Diff_Functions1 as Df
---------------------------------------
--Global Variables
valL = "0.56"
mymax = (maximum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))
criterium = "0.67"
dipfade2 = ["0.45","0.56","45"]
xX = "3" -- howMany lines to process
---------------------------------------
main :: IO ()
main = do 
  print "Done"
-----------------------------------------------------

-----------------------------------------------------
-- yourFunctions = []  ---------------------------------************************ CHOOSE FUNCTIONS TO PLOT
fomyFunctions t = ("differenzwerte"++(show t)++".txt")
myFunctions solong= let ste1 = [1..solong]
               -- choose the functions to plot in this case taken from
               -- the module 'Diff_Functions1'
              in let chooseFuncs = map fomyFunctions ste1
              in chooseFuncs

outPutMaxima3 x = [Df.differenzwerte x, Df.differenzwerte3 x,Df.differenzwerte4 x,Df.differenzwerte5 x,Df.differenzwerte6 x,Df.differenzwerte7 x]
-----------------------------------------------------
-----------------------------------------------------
-- write a WX-Maxima file ----------------------------------------************* MAIN WX-WRITING PIPE
aCsvToWx = do 
    let functionsToPlot = myFunctions
    putStrLn "Choosen functions: myFunctions"
  --- Extraordinary function chooses which AND howmany functions of the list above will be put in output
  -- l: [Int] ~ oder auch welche kombination von funktionen   
    let accesFuncWX l df = let aw1 n = (Us.takerleiN n (show (outPutMaxima3 df) ) )
                       in let wielanGg  = [1..l]
                       in let aw2 = map aw1 wielanGg
                       in let enExp a b sqale1 sqale2 = (WX.aCompleteWX a b xX sqale1 sqale2) -- diese display nach compiliren  vs aCompleteWX2 schreibt display in file     
                     --  in let aw3 =  ceiling (l/2)	
                       in let aw4 = ([wielanGg ,[(l+1)..(l*2) ]])
                       in let aw5 = "0.0" 
                       in let aw6 = mymax
              
                       in enExp (concat aw2) (concat aw4) aw5 mymax --enExp 
    putStrLn "getting there" 
      
   -- insert in the the Main WX wring fnction imported from wir
-----------------------------------------------------
-----------------------------------------------------
 
 
 
