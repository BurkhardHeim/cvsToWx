-- provides [String] that are used as Display with Text in an GUI called
-- via 'aOsZilloskop1' via  GUIMenu -> aPatternfile -> IO (GHC text)
module GUIMenuGHCtext
  where
-- Functions for displays:
------------------------------

-- global Function ; exported over the whole program
avanti e =  mapM_ putStrLn $ e

----------------------------------------------------------------------------
----------------------------------------------------------------------------
--four functions for ghci screen (clumsy)
emptii x =  let a = concatMap (replicate x)  [' '] 
           in   a ++ [' ']

minesweepi x =  let a = concatMap (replicate x)  [' '] 
               in   a ++ ['_','_','_','_','_']
emptiSpacei x =  let a = zip [1..x] eineListe 
                        where eineListe = emptii x
                in  (map emptii [1..x] )
                --in let c = map b [1..x]
                --in c


a0 = (emptii 15)
a1 = (emptii 1000)
a2 = (emptii 500)
a3 = (emptii 7)
a4 ="____________________________________________________________"
enterE = "ENTER a NUMBER :"
 

add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  
  
view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  
------------------------------------------------



-------------------------------------------------
-- BELOW a collection of: many [String] -> [ [string]] -> Patternfile
--  -> aOsZilloskop1 -> writes various WX-maxima or txt files
-- used as DISPLAYS and TEXTS
guiReturnFunctiontext =  avanti [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " GO BACK: STATISTICAL ANALYSIS \n"++
           "----------------------------\n"++ 
           "  Options:          1. PREVIOUS STEP \n"++
           "                    2. NEXT STEP      \n "++
           "                    3. MAIN MENU\n\n"++
           "ENTER a NUMBER: 1 or 2 .. or 3"++a2)]

statisticalWarschtext0 =  [("      MAIN MENU +++++  Stream-Crypt 1.1. ++++++                 \n"++ --- ************************ MAIN MENU
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " 1. read Csv ; 2./3. generates Wx-Maxima  \n"++
           "----------------------------\n"++ 
           "  Options:          1. WRITE PATERNFILE -- upload your own csv file must be \n"++
           "                                --Date,-,-,-,Close,Volume-10-Apr-16,-,-,-,0.56,-10-Ar-16...\n"++
           "                    2a. APPLY YOUR FUNCTIONS -- will write list of own functions to maxima\n"++
           "                    2. POINTCLOUD -- get simple statical Overview and plot 3d \n"++
           "                    3. aOSZILLOSKOP1 --plot2d csv data in WX and apply function to it\n"++
           "                    4. aCRUNCHLIST1-- most basic HTA/HTML plotter \n"++
           "                    5. CLOSE\n\n"++
           "                    6. HELP\n\n"++
           "ENTER a NUMBER: 1 or 2 .. or 5\n\n"++a2)]


statisticalWarschtext1 = [("      STATISTICS/PLOT 2D +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " 1./2. statistics 3. write to WX and 'plot2d' \n"++
           "----------------------------\n"++ 
           "  Options:          1. SIMPLE SIMULATION  -- goghist WAY OUT FO NOw\n"++
           "                    2. ALL FUNCTIONS-- the used statistical library\n"++
           "                       more SIMULATION  -- manipulate via addIA\n"++
           "                    3. DISPLAY  writes 'anMaximaFile2.wxm'--\n"++
           "                    4. HELP \n"++
           "                    5. CLOSE\n\n"++
           "ENTER a NUMBER: 1 or 2 .. or 5\n\n"++a2)]

statisticalWarschtext2 = avanti [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " Propabilities. Day  \n"++
           "----------------------------\n"++ 
           "  Options:          1. GOGOS -- % List the Formula-1 propabilities  \n"++
           "                    2. HEET  % of all propabilities \n"++
           "                    2. ZURWAHR 'zurWahrListe' picks of % \n"++
           "                    3. GORCH  similarity propabilities \n"++
           "                    4. GORCHH  ?????\n"++
           "                    5. HUMFREY example of cleaned string of monade \n"++
           "                    6. PREGO e.g (prego 1) Exampe caculates % as Int, alas only N-Num whole numbers\n"++
           "                    7. BRO e.g ( bro 1) connect a counter and a randomnumber generator\n"++
           "                    8. EINDELIJKGO compares rnd number gen with list gogos\n"++
           "                    9. ADDIA e.g (addIA 1729 prego ) adds %  of the additive list to\n"++
           "                   10. GHIJST e.g (ghijst 1) test map additive List with random  \n"++
           "                   11. GOGHIJST              (s. as Liste)\n"++
           "                   12. NEXT STEP      \n "++
           "                   13. BACK\n\n\n"++
           "                   14. MAINMENU\n"++
           "ENTER a NUMBER: 1 or 2 .. or 7\n\n")]

statisticalWarschtext3 = avanti [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " DISPLAY  \n"++
           "----------------------------\n"++ 
           "  Options:          1. MQ3 SIM-- fourier123 und siumulated vals  \n"++
           "                    2. MQ3 REAL --fourier123 ans real input vals  % gesamt  wahrscheinlichkeiten \n"++
           "                    3. MQ4 SIM -- four ..123..PAN4. and simu vals\n"++
           "                    4. MQ4 REAL -- fourierMQ4PAN and REAL V.1\n"++ 
           "                    5. MQ5 SIM -- fourierMQPAN123 and simul. Vals \n"++
           "                    6. MQ5 REAL -- fourierMQPAN123 and Real Vals \n"++
           "                    7. MQ5 & MQ3 \n"++
           "                    8. MQ6 SIM - foutrierMQ6SPAN123 BSp and simul. V. ereinigtes Format string aus monade \n"++
           "                    9. MQ6 REAL e.g (prego 1SPAN)  BSP Prozentrechener als Int leider nur fuer ganze zahlen \n"++
           "                   10. MQ6 MQ5\n"++
           "                   12. MQ6 MQ5 MQ3      \n "++
           "                   13. Simulated Vals\n\n\n"++
           "                   14. Real Vals\n"++
           "ENTER a NUMBER: 1 or 2 .. or 7\n\n")]


statisticalWarschtext4 = avanti [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " DISPLAY  \n"++
           "----------------------------\n"++ 
           "  Options:          1. MQ3 SIM-- fourier123 und siumulated vals  \n"++
           "                    2. MQ3 REAL --fourier123 ans real input vals  % gesamt  wahrscheinlichkeiten \n"++
           "                    3. MQ4 SIM -- four ..123..PAN4. and simu vals\n"++
           "                    4. MQ4 REAL -- fourierMQ4PAN and REAL V.1\n"++ 
           "                    5. MQ5 SIM -- fourierMQPAN123 and simul. Vals \n"++
           "                    6. MQ5 REAL -- fourierMQPAN123 and Real Vals \n"++
           "                    7. MQ5 & MQ3 \n"++
           "                    8. MQ6 SIM - foutrierMQ6SPAN123 BSp and simul. V. ereinigtes Format string aus monade \n"++
           "                    9. MQ6 REAL e.g (prego 1SPAN)  BSP Prozentrechener als Int leider nur fuer ganze zahlen \n"++
           "                   10. MQ6 MQ5\n"++
           "                   12. NEXT STEP      \n "++
           "                   13. BACK\n\n\n"++
           "                   14. MAINMENU\n"++
           "ENTER a NUMBER: 1 or 2 .. or 7\n\n")]


simulateStreamtext11  = avanti [("      SIMULATE   +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " SIMULATE: DATA STREAM \n"++
           "----------------------------\n"++ 
           "  Options:          1. RANDOM NUMBER \n"++
           "                    2. RANDOM JAHR \n"++
           "                    3. RANDOM MONAT \n\n"++
           "                    4. CATALOGE OF ALL fUNCTIONS\n\n"++
           "                    5. HELP \n"++
           "                    7. NEXT STEP      \n "++
           "                  8. BACK\n\n\n"++
           "ENTER a NUMBER: 1 or 2 .. or 7\n\n"++a2)]

statisticalAnalysistext1 = avanti [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " MAIN: STATISTICAL ANALYSIS \n"++
           "----------------------------\n"++ 
                                 "  Options:          1. WAHRSCHEINLICHKEITEN                 \n"++
           "                    2. RANDOM NUMBERS \n"++
           "                    3. NEXT STEP \n"++
           "                    4. HELP \n"++
           "                    5. OSZILLOSKOP      \n "++
           "                    6. BACK\n"++
           "                    7. MAINMENU\n"++
           "ENTER a NUMBER: 1 or 2 .. or 7"++a2)]

-- Jahresauswahl 
statisicalAnalysistext2 = avanti [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.0. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " JAHRESAUSWAHL \n"++
           "----------------------------\n"++ 
                                 "  Options:          a. FILTERYEARS X N :1526  \n"++
           "                    b. LINEOVERVIEW N  --  zeigt alle Stellen ALLER Zeilen\n"++
           "                                       -- EINES Jahres n aus wostehenWerte  \n"++
           "                    c. SETEIN -- einzelne Zeile MONTH X \n"++
           "                    d. snbs -- hohlt daten aus jahrInterval Pipe \n"++
           "                    e. plug -- Not sure ???\n")]


statisicalAnalysistext21 = avanti [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " LINE CHOOSER "++a2++"\n"++
           "----------------------------\n"++
           "  Enter: Line \n")]
           
statisicalAnalysistext22 = avanti [("  Enter: Year (e.g. 1)\n")]
          
statisicalAnalysistext23 h f = avanti [("  Options:          a. LINEOVERVIEW -- all lines of year: 1578 "++f++"\n"++
           "                    b. MAPCRITI   -- DreiBandDATA :1506 all Data of one line       \n"++
           "                    c. FILTERYEARS :1525 \n"++
           "                    d. MACHWAHR3  --  LENGTH VAL LINE: "++h++" ALL YEARS \n"++
           "                    e. SETEIN -- single line MONTH X s.a.\n")]

statisicalAnalysistext23N h f = avanti [("  Options:          a. IntfoPlug -- which year "++f++"\n"++
           "                    b. staerKA   -- Strength of Sets       \n"++
           "                    c. fofoA -- increase/rate of growth in % \n"++
           "                    d. snd polishDiamonds  -- (Gaussfaktor,Val o. month) \n"++
           "                    e. SETEIN -- einzelne Zeile MONTH X s.o.\n"++
           "                    f. relGroesse -- = sum spektrum /eines monats\n"++
           "                    g. singleLine4monthselector -- (lineX;month Y;breaker1;breaker2)\n"++
           "                    h. selectMonthDisplay -- momatsuebersicht" )]



statisicalAnalysistext24 h f = avanti [("  Options:          a.  -- alle Lines des Jahres : 1578 "++f++"\n"++
           "                    b. MAPCRITI   -- DreiBandDATA :1506 alle Daten einer Zeile       \n"++
           "                    c. FILTERYEARS :1525 \n"++
           "                    d. MACHWAHR3  --  LAENGE VAL LINE: "++h++" ALLE JAHRE \n"++
           "                    e. SETEIN -- einzelne Zeile MONTH X s.o.\n")]


statisticalAnalysisMonatAuswahl1 = avanti [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " MONATSAUSWAHL \n"++
           "----------------------------\n"++ 
           "  Options:          a. STEVEAPPROACH -- ALL LINES MONTH X IN ORDER  :2069  \n"++
           "                                     --IOinput: whichFu intfoplug month\n"++
           "                    b. setein -- single line MONTH X s.o.\n"++
           "                    c. VERGLMONATE :1526  \n"++
           "                    d. MONATLLIST -- Overview all lines month X\n :2035"++ 	    
           "                    e. PLUGINVAL -- year breaker1 breaker2  haeufigste \n"++
           "                                    vals Monat Y jahr X \n"++
           "                                       -- EINES Jahres n aus wostehenWerte  \n"++
           "                    f. FOMONAT -- die 12 haeufigstne monat X \n"++
           "                    g. SINGLELINE4MONTHSELECTOR  -- (lineX;month Y;breaker1;breaker2) overview \n"++
           "                    h. SELECT MONTH DISPLAY --\n\n"++
           "                    i. OVERVIEW MONTH      \n\n"++
           "ENTER a NUMBER: 1 or 2 .. or 9\n\n")]

--preselect IO for Monatauswahl
statisticalAnalysisMonatAuswahl2 = avanti  [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " PRE SELECT IO: MONATS AUSWAHL\n"++
           "----------------------------\n"++ 
           "  Enter: Jahr\n"++a2)]

statisticalAnalysisWriteWxMaxima1 = avanti  [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " WRITE WXMAXIMA FILE: MONATS AUSWAHL\n"++
           "----------------------------\n"++ 
           "                    Can only write NEW WXMAXMa, limited funct ! \n"++ 
           "  Enter: Jahr\n")]

statisticalAnalysisWriteWxMaxima2 = avanti  [ ("  Enter: Monat\n"++a2)]

statisticalAnalysisWriteWxMaxima3 = avanti  [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " WRITE WXMAXIMA FILE: MONATS AUSWAHL\n"++
           "----------------------------\n"++ 
           "                    Can only write NEW WXMAXMa, limited funct  \n"++ 
           "  Options:          1. mANUALLY CREATE VAL wx --VIEW SNBS -- monate:Int ; fomap [Int]\n"++
           "                                 -- von STEVE Approach -> in 2 Graphen\n"++
           "                                 -- gewandelt werden Min und Max Graphen\n"++  
           "                    2. CREATE mONTH OVERVIEW FILE --VIEW TRIGGER HAPPY monate --all lines month x, year: IO \n"++
           "                    3. CREATE RANDOM GRAPH-- multiple Line in Maxima format\n"++
           "                    4. ADD ANY FUNCTION WX- single Line in Maxima format\n"++
           "                    5. HELP \n"++
           "                    6. NEXT STEP      \n "++
           "                   7. BACK\n\n"++
           "ENTER a NUMBER: 1 or 2 .. or 7\n\n")]

statisticalAnalysisWriteWxMaxima4 = avanti  [("      STATISTICAL ANALYSIS +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           " WRITE WXMAXIMA FILE: SET UP WX\n"++
           "----------------------------\n"++ 
           "                    Can only write NEW WXMAXMa, limited funct ! \n"++ 
           "  Options:          1. Enter: for Wx Screen Number of X Values\n"++
           "                    2. Enter: for Wx Screen Min Y Value\n"++
           "                    3. Enter: for Wx Screen Max Y Value\n\n"++
           "                    4. HELP \n"++
           "                    5. NEXT STEP      \n "++
           "                   6. BACK\n\n"++
           "ENTER a NUMBER: 1 or 2 .. or 6\n\n")]



pointCloudtext0 = [("    3D PLOT +++++  Stream-Crypt 1.1. ++++++                 \n"++
           "*******************   A DATA ANALYSING/WRITING PROGRAM ****************\n\n" ++
           (minesweepi 0 )++"_______________________\n"++
           "  3dplot in WX-Maxima write wxm \n"++
           "----------------------------\n"++ 
           "  Options:          1. PointcloudII  -- two sources in 1 random cloud\n"++
           "                    2. PointcloudIII  -- three sources in 1 random cloud\n"++
           "                    3. PointcloudIV  -- four sources in 1 random cloud\n"++
           "                    4. PointcloudV  -- five sources in 1 random cloud\n"++
           "                    6. 3D MQ6  -- a unique setup )\n"++
           "                       more SIMULATION  -- manipulate via addIA\n"++
        --   "                    3. DISPLAY  writes 'anMaximaFile2.wxm'--\n"++
           "                    4. HELP \n"++
          -- "                    5. NEXT STEP      \n "++
         --  "                    6. BACK\n"++
           "                    5. CLOSE\n\n"++
           "ENTER a NUMBER: 1 or 2 .. or 5\n\n"++a2)]


--- AN DATA LIST CRUNCHER 
----------------------------------------------
--
--
--a = string name file to write file name
--b = string Name der Quell datei
--b2 = string IO input, wieviele zeilen eingelesen 
--c = string name
--d = string min 
-- e  = string val einzel
-- e2 = String val vorkommen 
-- f = string crit stellen
-- "*This Tool is to b eused to determine how to proceed when reading \n"++
                      --       " DATA. The next step is indicated by the type command that will fire\n"++
               
         --     " wxMAxima programm to plot and opening MAxima sessions." 
listCruncher a b b2 c d e e2 f = 
                    avanti [("\n\nANALYSE PATTERFILE     +++++  Stream-Crypt 1.1.19 ++++++                 \n"++
                             "____________ AN DATA ANALYSING/WRITING PROGRAM ___________\n" ++
                             "WRITING FILE:    \""++a++"\"                              |\n"++
                             "|CRUNCHLIST ,      Content: \""++b++ "\" Lines: "++b2++" \n"++
                             "|   MAX Val:\""++( c)++",\" \" \n"++
                             "|   Min Val:   \""++( d)++"\"\n"++
                             "|         |_CRITERIUM VAL_______|______1 Val____|_aus zui_|\n"++
                             "|             "++(show e)++"\n"++
                             "|                                               00001\n"++
                             "|         | OCCURANCE CRITERIUM |               |         |\n"++
                             "|         |  "++ (show f) ++" \n"++
                             "|         |                     |               |         |\n"++
                             "|         | SPEKTRUM   |        |               |         |\n"++
                             "|         |  "++ (show e2)  ++"\n"++
                             "|         |            |________|_______________|_________|\n"++
                             "|         |  type:     | aOsZilloskop1 x nF crit          |      \n")]

listCruncherRaw a b b2 c d e e2 f =
                           [( "|.........|.SPEKTRUM...|........|...............|.........|\n<br>"++
                              "|.........|  "++ (show e2)  ++"\n<br>"++


                                                          "|CRUNCHLIST ,      Content: \""++b++ "\" Lines: "++b2++" \n<br>"++
                             "|...MAX Val:\""++( c)++",\" \" \n<br>"++
                             "|...Min Val:   \""++( d)++"\"\n<br>"++
                             "|_________|_CRITERIUM VAL_______|______1 Val____|_aus zui_|\n<br>"++
                             "|.........|...."++(show e)++"\n<br>"++
                             "|_______________________________________________| 00001   |\n<br>"++
                             "|.........|.OCCURANCE CRITERIUM |               |         |\n<br>"++
                             "|.........|  "++ (show f) ++" \n"++
                             "|_________|_____________________|_______________|.........|\n<br>"++
                              "WROTE FILE:    \""++a++"\"                              |\n<br>")]

buttonNew = "<button onclick=\"Call Run('C:/Users/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/Logini17/Neuer Ordner/BackBoneOldFiles/HaskellSVG_XHTML2017/aaKA/KAnsatzLOGINI2.0/BatFiles/JustRunexampleWARSCH.bat')\">Data</button>"

buttonView = "<button onclick=\"all Run('C:/Usersc/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/stream-crypt/src/Bats/showHubbort.bat')\">View</button>"

buttonEdit ="<button onclick=\"Call Run('C:/Users/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/Logini17/Neuer Ordner/BackBoneOldFiles/HaskellSVG_XHTML2017/aaKA/KAnsatzLOGINI2.0/BatFiles/JustRunexampleWARSCH.bat')\">Edit</button>"

buttonWx = "<button onclick=\"Call Run('C:/Users/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/Logini17/Neuer Ordner/BackBoneOldFiles/HaskellSVG_XHTML2017/aaKA/KAnsatzLOGINI2.0/BatFiles/JustRunexampleWARSCH.bat')\">WxMaxima</button>"

buttonMQ6= "<button onclick=\"Call Run('C:/Users/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/Logini17/Neuer Ordner/BackBoneOldFiles/HaskellSVG_XHTML2017/aaKA/KAnsatzLOGINI2.0/BatFiles/JustRunexampleWARSCH.bat')\">MQ6</button>"

buttonHelp = "<button onclick=\"Call Run('C:/Users/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/Logini17/Neuer Ordner/BackBoneOldFiles/HaskellSVG_XHTML2017/aaKA/KAnsatzLOGINI2.0/BatFiles/JustRunexampleWARSCH.bat')\">Help</button>"


--c2= Anzahl Aktive Spuren
oszilloskop a b b2 c c2 d e e2 f = 
                  avanti   [("WRITING FILE:    \""++a++"\"   ____________________\n"++
                             "|OSZILLOSKOP ,      Content: \""++b++ "\" Lines: "++b2++" \n"++
                             "|   MAX Val:\""++( c)++",\"  ACTIVE SPUREN: "++c2++" \" \n"++
                             "|   Min Val:   \""++( d)++"\"\n"++
                             "|         |_CRITERIUM VAL_______|______1 Val____|_aus zui_|\n"++
                             "|             "++(show e)++"\n"++
                             "|                                               00001\n"++
                             "|         | VORKOMMEN CRITERIUM |               |         |\n"++
                             "|         |  "++ (show f) ++" \n"++
                             "|         |                     |               |         |\n"++
                             "|         | SPEKTRUM   |        |               |         |\n"++
                             "|         |  "++ (show e2)  ++"\n"++
                             "|         |            |________|_______________|_________|\n"++
                             "|         |  type:     | aOsZilloskop1 x nF crit          |      \n"++
                             "|         |            |                                  |   \n" ++                           
                             "                                                          |\n" )]


funktionOszilloskop =
              avanti [("| CHOOSE FUNCTION      |            |________|_______________|_________|\n"++
                      "|         |  type:     | aOsZilloskop1 x nF crit          |      \n"++
                      "|         |            |                                  |   \n" ++                           
                      "                                                          |\n" )]




