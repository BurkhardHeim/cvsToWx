module Main
   where
import System.Random
import System.Environment
import System.IO
import System.Directory
import Data.Char
import Data.List
import Control.Monad 
import Control.Monad.State

import qualified Patternfile as P
import qualified UsefulFunctions  as Us   
import qualified WriteWXmaximaFile as WX
import qualified Diff_Functions1 as Df
import qualified GUIMenuGHCtext as GUI
import qualified Data.Time as T
--import WriteInput
---------------------------------------
--Global Variables
howMany = "100" -- lines to read
valL = "0.56"   -- searcj criteria must be in set ?!
xX = "3" -- howMany lines to process

-- read CSV from this global variable
entersCSV = "durble.csv"
fohans= "1" -- not used?

root = "c:/stack/Stream-Crypt19/"

xRaw = "seniot.txt"
nF = "seniot2.txt"
crit = valL
--rnd = "110"
file22writ = root++"senio1.txt"
file33writ = root++"senio2.txt"

arun = "1" 
------------------------------------
-- a clockvalue + a value ,inserted in a random number generator 
--    in:   [ random (sum [(clockVal),( value )]))  ]   
dit = do
   foRandTime <- T.getCurrentTime
   let parameters =  lines ((P.timeRNDInts foRandTime (length (lines "34\n 45\n 67")) 4))
   P.avanti parameters
--   let loopWrite =
  --      where
    --   dlter1 bus =  removeFile (root++"aaKA/aaKAnsatz"++(show (bus-1)++".txt"))
    
   writeFile (evalToWrite "allTextFiles/RandomList"++ arun++".txt") (unlines parameters)
------------------------------------

avanti = GUI.avanti
-----------------------------------
main :: IO () 
main = do 
  putStrLn "Control via Keyboard or Global Variables? 1==keyboard"
  contrl <- getLine
  dit
  thats <- readFile ("allTextFiles/RandomList"++arun++".txt")   
  let rnd = head(words (replacE (thats)))
  putStrLn ("the Rnd" ++ rnd)

  avanti [("setup... Stream-Crypt 1.1.19..\n\n")++
          ("manual menu?  1==load menu\n")++
          ("              2== Automatic menu\n")++
          ("              3== Easy Access") ] 
  stelsel <- getLine
  putStrLn "show output? 1==show it"
  hideOu <- getLine

  let easyAccess = do
           P.writePatternFile file22writ
           P.aMemoryRAW "1" xRaw file33writ 

  let selectOR = do 
         if stelsel=="1" then do
                 if hideOu == "1" && contrl == "1" then do  fomain "2" "1" rnd selectOR "1" -- keyboard, direktauto make patternfile , show output
                 else if hideOu /= "1" && contrl == "1" then do  fomain "2" "1" rnd selectOR "2" -- no keyboar, hide outputd
                 else if hideOu /= "1" && contrl /= "1" then do  fomain "2" "1" rnd selectOR "1" -- keyboard , hide output 
                 else fomain "2" "1" rnd selectOR "2" -- show output , globalvars
        -- else if stelsel=="1" && contrl /= "1" then do  fomain "2" "1" rnd selectOR "2" --globalvars, menu ,  
         else if stelsel=="2" && hideOu == "1" && contrl == "1" then do fomain "2" "2" rnd selectOR "1" --  "     , no-output , use keyboard
         else if stelsel=="2" && hideOu == "1" && contrl /= "1" then do fomain "2" "2" rnd selectOR  "2" --  "     , no-output ,use GLobal Vars
         else if stelsel=="2" && hideOu /= "1" && contrl == "1" then do fomain "2" "1" rnd selectOR  "1" -- yes output , keyboard
         else if stelsel=="2" && hideOu /= "1" && contrl /= "1" then do fomain "2" "1" rnd selectOR  "2" -- yes output , global var  
         else do
           easyAccess
           main
  selectOR 
-------------------------------------------------------------------------------------
replacE = map (\c -> if c=='[' || c== ',' || c==']' then ' '; else c)

-- will turn on number digit of a Sring +1
--  e.g "aname1.txt" -> "aname2.txt"
--  used in time depending random file writing in 'dit' 
--aString:Sting ; the sourceFile
evalToWrite astrinG = if tzBool>0 then prsRoot++(head tz3)++(show tzInt)++"."++(last tz3)
                      else prsRoot++(head tz3)++("1.")++(last tz3)
     where
    poc1 fOsnd = reverse( fOsnd(break (=='/') (reverse(astrinG))));--prevent '/' cause trouble
    prsInput = poc1 fst;
    prsRoot = poc1 snd;  
    tz0 = (map ord prsInput);
    tz = (filter (>47) (filter (<57)  tz0));
    tzExp = (map chr tz);
    tzBool = length tzExp;
    tzRootSource  = filter (==47); 
    tz1 = tz0 \\ tz;
    tz2 = map (\c -> if c=='.' then ' '; else c);
    tz3 = words (tz2 (map chr tz1));
    tzInt = if tzBool==0 then 1
            else if (read tzExp)<7 then (read tzExp)+1
            else 1 ;

---------------------------------------------------------------------
---------------------------------------
-- Level 0                     | GUI-Mode vs RAW-Mode  |
-- Level 1                     | print outpt yes or no | 
-- Level 2        |  GUI yes | GUI no  | RAW yes  | RAW no | 

---------------------------------------------------------------------   
-- autoInputI if "1" == then read new csv
-- autoInputII "1" == show outputs,  "2" == hide output 
-- autoInputIII "1" == keyboard , "2" == global variables

fomain autoInputI autoInputII foRnd goBack autoInputIII = do
  dit  -- write random.txt for main
  foautInp2 autoInputI "1" entersCSV "Enter csv file to read" -- set read one line
  foautInp2 autoInputI "1" file22writ "Enter file to write e.g senio.txt"
  foOutputBat autoInputII (avanti (GUI.statisticalWarschtext0))
  input <- getLine     
  let chart0 = if input == "1"
               then do
                               --   myReadLine thecsv 
                   let inserTs = (P.writePatternFile "senio1.txt")
                   inserTs
                   goBack 
               else if input=="2"
               then do
                   P.aMemoryRAW autoInputII file22writ file33writ
                   goBack 

              -- OSZILOSKOP1
              ---------------------------------------------------------
              -- KEYBOARD ON              show output &&  keyboard- on ; 
               else if input=="3" && autoInputII=="1" && autoInputIII=="1"
               then do
                -- set yes-output, read theWritetxt -> writes 
                   P.aOsZilloskop1KEY "1" 
                   goBack 
             ---------------------------------------------------------
              -- KEYBOARD ON / SHOW NO OUTPUT    no show output && keyboard- on ; 
               else if input=="3" && autoInputII=="2" && autoInputIII=="1"
               then do
                -- set yes-output, read theWritetxt -> writes 
                   P.aOsZilloskop1KEY "2" 
                   goBack 
            ----------------------------------------------------------
            -- GLOBAL VARIABLES ON        show-output && global variables , yourFunction-off
               else if input=="3" && autoInputII=="1"  && autoInputIII=="2"
               then do
                -- control via Global variablea , set no-output, read theWritetxt -> writes 
                   P.aOsZilloskop1RAW "1" file22writ file33writ crit foRnd howMany fohans "1" 1 1
                   goBack 
              ----------------------------------------------------------
            -- GLOBAL VARIABLES ON / SHOW NO OUTPUT  no-output && global variables ,your functions off
               else if input=="3" && autoInputII=="2"  && autoInputIII=="2"
               then do
                -- control via Global variablea , set no-output, read theWritetxt -> writes 
                   P.aOsZilloskop1RAW "2" file22writ file33writ crit foRnd howMany fohans "1" 1 1
                   goBack 
              ----------------------------------------------------------

              --CRUNCHLIST 
               else if input=="4" && autoInputII=="1"  && autoInputIII=="2" --show-output ; global var
               then do
                 --   read write howmany crit
                   P.aCrunchList1  file22writ file33writ howMany crit
                   goBack 
               else if input=="4" && autoInputII=="2"  && autoInputIII=="2"  -- hide-output ; global var
               then do
                 --   read write howmany crit
                   P.aCrunchList1  file22writ file33writ howMany crit
                   goBack 
               else if input=="4" && autoInputII=="1"  && autoInputIII=="1"  -- show-output ; keyboard-io
               then do
                   P.aCrunchList1KEY "1"
                   goBack
               else if input=="4" && autoInputII=="2"  && autoInputIII=="1"  -- no-output ; keyboard-io
               then do
                   P.aCrunchList1KEY "2"
                   goBack
 
               else if input=="5"
               then do
                   putStrLn "End Compute"   
               else do 
                   goBack  
           -- uses input0 to stop computations  
  chart0 
 -- P.writePatternFile theCsv "senio1.txt"
  -- will just succed for shorter list aprx 300 
 -- P.aMemoryRAW "1" "senio1.txt" (theWriteTxt) 

 -- aCsvToWx
 -- print "Done"
----------------------------------------------------
foOutputBat output paks = foOutputRAW output paks (putStrLn (""))

foOutputRAW output paks pac = if output=="1" 
                       then do
                          paks
                       else do
                          pac

----------------------------------------------------
foautInp2 autoInput solong globalVar aTxT = do
           let customL = [1..(read solong)]
           doesTaht <- forM (customL) (\presetList -> do 
                   if autoInput=="1"
                   then do
                      morM <- forM [1] (\gu -> do
                            putStrLn aTxT -- e.g "Enter csv file to read"

                            exportThis <- getLine
                           -- let goAhead inserT1 = (foFunction inserT1) 
                           -- let inCompute = goAhead exportThis  
                           -- inCompute
                            return (exportThis))
                      putStrLn (unlines morM)
                   else do
                      putStrLn globalVar
                   return ())
           return()

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
 
