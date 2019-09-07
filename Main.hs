module Main
   where
   
import WriteWXmaximaFile
import Diff_Functions1
-----------------------------------------------------

-----------------------------------------------------
-- yourFunctions = []  ---------------------------------************************ CHOOSE FUNCTIONS TO PLOT
fomyFunctions t = ("differenzwerte"++(show t)++".txt")
myFunctions = let ste1 = [1..solong]
               -- choose the functions to plot in this case taken from
               -- the module 'Diff_Functions1'
              in let chooseFuncs = map fomyFunctions ste1
              in choosFuncs

-----------------------------------------------------

-----------------------------------------------------
-- write a WX-Maxima file ----------------------------------------************* MAIN WX-WRITING PIPE
aCsvToWx = do 

    let functionsToPlot = myFunctions
    putStrLn "Choosen functions: myFunctions"
  --- Extraordinary function chooses which AND howmany functions of the list above will be put in output
  -- l: [Int] ~ oder auch welche kombination von funktionen   
     let accesFuncWX l  = let aw1 n = (takenN n outPutMaxima3)
                       in let wielanGg  = [1..l]
                       in let aw2 = map aw1 wielanGg
                       in let enExp a b sqale1 sqale2 = (aCompleteWX a b xX sqale1 sqale2) -- diese display nach compiliren  vs aCompleteWX2 schreibt display in file     
                     --  in let aw3 =  ceiling (l/2)	
                       in let aw4 = ([wielanGg ,[(l+1)..(l*2) ]])
                       in let aw5 = "0.0" 
                       in let aw6 = max              
                       in enExp aw2 aw4 aw5 max --enExp  
      
   -- insert in the the Main WX wring fnction imported from wir
-----------------------------------------------------
-----------------------------------------------------

 
 
