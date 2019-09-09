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
import qualified GUIMenuGHCtext as GUI
import qualified Data.Time as T

---------------------------------------
--Global Variables
howMany = "100"
valL = "0.56"
mymax = (maximum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))
criterium = "0.67"
dipfade2 = ["0.45","0.56","45"]
xX = "3" -- howMany lines to process
---------------------------------------
-- Level 0                     | GUI-Mode vs RAW-Mode  |
-- Level 1                     | print outpt yes or no | 
-- Level 2        |  GUI yes | GUI no  | RAW yes  | RAW no | 
xRaw = "seniot.txt"
nF = "seniot2.txt"
crit = valL
rnd = "100"
fohans= "1"
dit = do
   foRandTime <- T.getCurrentTime
   let parameters =  lines ((P.timeRNDInts foRandTime (length (lines "34\n 45\n 67")) 4))
   P.avanti parameters

main :: IO () 
main = do
  putStrLn "Enter csv file to read"
  theCsv <- getLine
  putStrLn "Enter file to write e.g:\"seniot.txt\""
  theWriteTxt <- getLine
  input0 <- getLine        
  let chart0 = if input0=="1"
               then do 
                  input <- getLine 
                  let chart1 = if input=="1"
                               then do 
                                  P.writePatternFile theCsv "senio1.txt"
                                  (chart0)
                               else if input=="2"
                               then do
                                  P.aMemoryRAW "1" xRaw theWriteTxt
                                  (chart0)
                               else if input=="3"
                               then do
                        -- set yes-output, read theWritetxt -> writes 
                                  P.aOsZilloskop1RAW "1" xRaw nF crit rnd howMany fohans
                                  (chart0)
                               else if input=="4"
                               then do
                                  P.aCrunchList1  xRaw nF howMany crit
                                  (chart0)
                               else if input=="5"
                               then do
                                  putStrLn "End Compute"
                               else do
                                  (chart0)
                  chart1
               else if input0=="2" ---------------------------------------------------------
               then do              -- "2" prints NO-OUTPUT
                  input <- getLine 
                  let chart2 = if input=="1"
                               then do 
                                  P.writePatternFile theCsv "senio1.txt"
                                  (chart0)
                               else if input=="2"
                               then do
                                  P.aMemoryRAW "2" xRaw theWriteTxt
                                  (chart0)
                               else if input=="3"
                               then do
                        -- set yes-output
                                  P.aOsZilloskop1RAW "2" xRaw nF crit rnd howMany fohans
                                  (chart0)
                               else if input=="4"
                               then do
                                  P.aCrunchList1  xRaw nF howMany crit
                                  (chart0)
                               else if input=="5"
                               then do
                                  putStrLn "End Compute" -- only very basic Data printed
                               else do
                                  (chart0)  
                  chart2            
               else 
                 (main) 
           -- uses input0 to stop computations  
  chart0 
 -- P.writePatternFile theCsv "senio1.txt"
  -- will just succed for shorter list aprx 300 
 -- P.aMemoryRAW "1" "senio1.txt" (theWriteTxt) 

 -- aCsvToWx
 -- print "Done"

guiReturnfunction back1step backAllsteps u = 
                     let ans x1 = if x1=="1" 
                                  then do 
                                    back1step
                                  else
                                    backAllsteps
                     in do  -- with do ghi sees line1 ...guiReturn, line2 ..input1... 
                        (P.foOutput u GUI.guiReturnFunctiontext)
                        inputgui1 <- getLine
                        ans inputgui1

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
 
