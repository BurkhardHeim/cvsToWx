module Main
   where
import System.Random
import System.Environment
import System.IO
import Data.Char
import Data.List
import Control.Monad 

import qualified Patternfile as P
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
-- Level 0                     | GUI-Mode vs RAW-Mode  |
-- Level 1                     | print outpt yes or no | 
-- Level 2        |  GUI yes | GUI no  | RAW yes  | RAW no | 
main :: IO ()
main = do
  putStrLn "Enter csv file to read"
  theCsv <- getLine
  putStrLn "Enter file to write e.g:\"seniot.txt\""
  theWriteTxt <- getLine
   
  P.writePatternFile theCsv "senio1.txt"
  -- will just succed for shorter list aprx 300 
  P.aMemory "senio1.txt" (theWriteTxt)

 -- aCsvToWx
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
{-
-----------------------------------------------------
-----------------------------------------------------
-- write a WX-Maxima file ----------------------------------------************* MAIN WX-WRITING PIPE
aCsvToWx = aCsvToWxRaw "diff_blindSpot5.wxm" 

-- foWxmName:String ; e.g "fileToWrite.wxm"
--
aCsvToWxRaw  foWxmName = do 
    let functionsToPlot = myFunctions
    putStrLn "Choosen functions: myFunctions"
  --- Extraordinary function chooses which AND howmany functions of the list above will be put in output
  -- l: [Int] ~ oder auch welche kombination von funktionen   
    let accesFuncWX l df = let aw1 n = (Us.takerleiN n (show (outPutMaxima3 df) ) )
                       in let wielanGg  = [1..l]
                       in let aw2 = map aw1 wielanGg
                       in let enExp a b sqale1 sqale2 = (WX.aCompleteWX a b xX sqale1 sqale2) -- aCompleteWX  vs aCompleteWX2 writes display into the file     
                     --  in let aw3 =  ceiling (l/2)	
                       in let aw4 = ([wielanGg ,[(l+1)..(l*2) ]])
                       in let aw5 = "0.0" 
                       in let aw6 = mymax
              
                       in enExp (concat aw2) (concat aw4) aw5 "1" --mymax --enExp 
    putStrLn (accesFuncWX 3 3)
    writeFile foWxmName ((accesFuncWX 3 3))
    putStrLn ("Wrote "++ foWxmName ++"\n")
    putStrLn "getting there" 
   -}      
   -- insert in the the Main WX writing function imported from module 'WriteWXmaximaFile'
-----------------------------------------------------
-----------------------------------------------------
 
