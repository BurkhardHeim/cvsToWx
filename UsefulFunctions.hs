-- upDated 21-08-19
-- must be updated with UseflFunction19 in forGlade program
module UsefulFunctions where
-- always color sceME 'delek'

import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO
import Data.Time as T
import System.Locale
import System.Random


-- 21-08-19
simpeLength k = do 
         foLaen <- readFile k
         let soviel = length (lines foLaen)
         print soviel
         



replaceColon = map (\c -> if c==',' then ' '; else c)


-- source for rndlist 
zufallsBasic1 t a x = (take t  (randomRs (1,a) (mkStdGen x)))::[Int]

rndLength1 = zufallsBasic1 3 1000000000 10
easyfoNowRun1 wieviele = zufallsBasic1 wieviele 5000 10
-- rndList:[String] e.g ["1","55","4","34566543"] any N number which is I

-- foTime: IO must read Time
-- soMany : Int; length of outputList :soMany lines
-- digits: Int ; lenght og each line 
getInts foTime soMany digits = let prepStringtoInt = (map chr (filter (<58)(filter (>47)(map ord (show foTime)))))
                               in let plugRandom wieviele = zufallsBasic1 wieviele (read prepStringtoInt) digits
                               in (show (plugRandom soMany)) 
time :: IO ()
time = do
  myTime <- getCurrentTime
  putStrLn $ "It's currently: " ++ show myTime

  let fiveHours = 5 * 60 * 60
  let later = fiveHours `addUTCTime` myTime
  putStrLn $ "Five hours later it will be: " ++ show later

  let format = formatTime T.defaultTimeLocale "%Y/%m/%e %l:%M %P"
  putStrLn $ "Formatted, that is: " ++ format later

  let christmasDay = T.fromGregorian 2013 12 25
      n = christmasDay `diffDays` utctDay myTime
  putStrLn $ "Only " ++ show n ++ " days to go until Christmas!"
-----------------------------------


-----------------------------------
-- x = how man 2 take
-- y = which function 2 iterate
-- z = of which value y(z) 
unto x y z = take x ( iterate y z)
-----------------------------------

replaceE = map (\c -> if c=='e' then '1'; else c)


------------------------------------------------------------------
--fo MYWHIZ a functio that flattens lists sort of concat
--it finds out how many concat do to by itself
concatM x= let a = map ord x
           in let b = filter (==91) a
           in let c = filter (==93) a
           in let mer = let ant l k =  l \\ k
                        in let be  = ant a b
                        in let ce = ant be c
                        in ce 
              
           in let back = map chr mer
           in back
----------------------------------------------




----------------------------------------------
-- the same that filters '(' ')' can be used 
-- to unzip IF contents of (a , b) are the same typ 
--____________________________________________
unPair x = let a = map ord x
           in let b = filter (==40) a
           in let c = filter (==41) a
           in let mer = let ant l k =  l \\ k
                        in let be  = ant a b
                        in let ce = ant be c
                        in ce
           in let back =  mer
           in back
----------------------------------------------



----------------------------------------------
--___________________________________________
-- take x lists of the length y of a [Int]
-- can process flattend lists
--
takeMY x y z = (mits y) (hits z)
  where
   mits f = (take f);
   hits = repeat.(mits x)   
        --   in let wider = map b l --l: liste der laenge l von
        --
 
 
-------------------------------------------
--
--ALLTIME FAVORITE
--Nimmt zeile aus String
--x: Int ; y:String
takerleiN x y = drop (x -1)(take x (lines y))


reaDin = do 
    thisfi <- readFile "dininWalAllPath.txt"
    let inse = mymonaD thisfi 9
    putStrLn (show inse)
--_________________________________________________
--filtert String z nach zeichenzahl (s. dropwhile ..)
--in 'wolfram' werden leerstellen noch angezeigt (concat entfernt diese)
--in 'rythmuS' werden die stellen der valieden pfade als [Int] ausgegeben
--MOMENTAN GETRIGGERT AUF : rythmuS
--z: inputDatei String
--d: liste der laenge von input int

mymonaD z d = let wolfram = let mof z d = map (takerleiN d) (z)
                            in let mymonad z d = map (mof z) d --maped [string] ueber [Int] (select)
                            in let momonad = let fo = concat.concat
                                             in let iNN = fo (mymonad (lines z) ([1..d]))
                                             in let mYY = group (map length iNN)
                                             in let unconcat = (map show mYY) -- gegenteil von concat
                                             in let hg = (head mYY)
                                             in map (dropWhile (<1000)) mYY
                            in momonad 
   
              in let rythmuS  = let prep = group (zip [1..d] ( wolfram))
                                in let dorf = map (dropWhile (\(num,lis) -> lis == [] ) ) prep -- durch map wird die ganze liste bearbeitet   
                            --(dropWhile (\(num,lis) -> lis == [] )  prep) -so ohne map   
                                in  map fst ((concat ( dorf))) 
              in ( rythmuS) -- oder rythmus oder concat wolfram             
--         putStrLn "\n\n____Wolfram:\n"          
  --    print wolfram
    --  putStrLn "\n\n____NONUMbers: flattend length of valid paths\n" 
      --print (concat wolfram) 
     -- putStrLn "\n\n____The Rythem of valid paths: flattend position of valid paths\n"
    --  print rythmuS 
   --   putStrLn "\n"         
   --
   --
   --
   --

--____________________________________________________________________________
--nimmt EINEN valieden pfad aus einer beliebigen text datei
--aus der zeile x  (valide zeilen bekannt durch mymonaD)
--getriggered mit mymonaD (<zeichenzahl)  und takerlein also 
--string wird mit Int geemapped, in evallis wird eine
-- [Int] gegen [String] gemapped
--
--x: int ->  valieder pfad  in zeile ..
--s: string -> eingelesene pfade
evallis x s =  let prqw u t =  (take u) t  
               in let vorbe = (mymonaD s x)
               in let erg = let tzu = prqw x (lines s) -- 1 variable nimmt validen 'path' string
                            in tzu 
               in let fodritte = let preb u =  prqw u erg  --waehlt von erg (alle input) nur die
                                 in  map preb vorbe -- mit vorbe werde valide pfade ausgewaehlt 
                                        -- in take a erg  
               in let nooky = let buena = take (length vorbe) ( (fodritte))
                              in ( ( (buena))) -- mymonaD (concat(concat buena)) (take a vorbe)
               in nooky  


--gibt Liste der validen pfade [string] aus
--v: [Int] -> liste der LAENGE: anzahl valide pfade
--s: String ->  der gleiche wie in mymonaD  und evallis
desper v s = let aa j u  = evallis  u j 
             in let ab j u =  map (aa j) u
             in let ac = [1..(length v)]
             in ab s ac  
--brauch och print befehl fuer IO
--
-------------------------------------------------------------------------------------------


haalTal :: String -> Int
haalTal s = groesserneun
  where  
    groesserneun = let verwerk = map ord s
                   in let a  = filter (<=57) verwerk
                   in read (map chr a)

{-
------------------------------------------------------------------------------------------------------------------
-- Funktion berechnet monats ende und anfang  in tagen
-- Ist gregorianischer Kalendar jahreslaenge 365 tage
-- wird benoetigt fuer mehrzeilige Monats uebersicht
-- dies ist die Analyse der daten BSP fuer spaetere
-- simulation      
monatllist jahrzahl laengezeit monaT = 
              let faktorM = (365 * laengezeit)	
                  jahreE = (jahrzahl/4) 
                  schjahrlistTrue = [31,29,31,30,31,30,31,31,30,31,30,31]
                  schjahrlistFalse = [31,28,31,30,31,30,31,31,30,31,30,31]
                  takese wd wquel= drop (wd-1) (take wd wquel) 
                  kesNoDrop wd wquel= (take wd wquel) 
                  
                   -- filter . test auf teiler4 -> schaltjahr?
                  schjahrtest k = let dezimalstellenOnly = let tochr = map ord (show jahreE)
                                                               breakeR = snd(partition (==46) tochr)--breaK AT '.'
                                                               breakeR2 = length breakeR
                                                               prozesPaar k g = (takese k g)
                                                           in let staun1 = if (breakeR2>1) then ((prozesPaar k schjahrlistFalse),(kesNoDrop k schjahrlistFalse)  )
                                                                           else ((prozesPaar k schjahrlistTrue), (kesNoDrop k schjahrlistTrue))
                                                                  -- nim ganze liste fuer abzaehl datum
                                                       {-    in let staun2 = if (breakeR2>1) then (kesNoDrop k schjahrlistFalse)
                                                                           else (kesNoDrop k schjahrlistTrue)-}

                                                          in (staun1)
                                  in  dezimalstellenOnly
                  einsetzenJahrMax k = ((sum(snd (schjahrtest k)))  + (365*(laengezeit-1))) 
                  einsetzenJahrDurch k = (fst (schjahrtest k))--((einsetzenJahrMax k) - (einsetzenJahrMax(k-1)))
                  einsetzenJahrMin k =  ((einsetzenJahrMax k) - ((einsetzenJahrMax k) - (einsetzenJahrMax(k-1))) )
                  selectMax x = if (laengezeit==1) then (einsetzenJahrMax x) --(fst(schjahrtest x))
                                else (einsetzenJahrMax x) --(einsetzenJahr x) -- if (fak1== ) then ( 
                  selectMin x = if (laengezeit==1) then (einsetzenJahrMin x) --(fst(schjahrtest x))
                                else (einsetzenJahrMin x)          
              in ((einsetzenJahrDurch monaT),((selectMin monaT), (selectMax monaT)) )
  -- singleLine4MonthSelector 

 -}

-- builds SVG Data like Backbone, builts SVG Animated Groups (<g>..</g>)
-- that will be inserted into activeField
--theDNA g  = do 
      
-- SUPER !! 1. gebe Wort aus <g>..</g> file ein
--          2. filtert Automatisch Koordinaten x y 
--          3. Bewegt Inhalt um 2 Felder
--            (mittles 'position1 vektor')
--          4. gibt neue Koordinaten im fertigen <g>..</g> String aus 
-- filtert das Wort P F A D aus foKoordinates
-- zeigt Stelle fuer Break Kriteria fuer Einfuegen
-- von item Koordinaten
--  EXPORTS: fertigen <g>..</g> String mit FERTIGE ANIMATION
--  token:: Int
itemReaderPfadFinder file vektor token = do
        afile <- readFile file --e.g. "GraphicEngine17/Backbone/TheGameItemCircleWORKS.txt"
        let criterium = ("1122")
        let stepper = [1..(length (lines afile))]-- findet raus wie lang input file ist	
        iteM <- forM (stepper) (\ a -> do
             let auswahL = map ord (unwords (takerleiN a afile))

             let check = foKoordinates auswahL
           
             let checkPfad = if (check)==([112,97,116,104]) then (a) -- P F A D
                             else (0)
             let hohlPfad = let as1 = [0..checkPfad]
                          --  in let as2 = zip [1..] ( as1)
                            --in let as3 = length [1..as2]
                            in (maximum( as1)) --welche Zeile MATCHED ?  
             return (hohlPfad) )
        let hohlersteZeilen = let as1 = take (maximum iteM) (lines afile) --nimm erste zeilen aus afile
                              in let as2 = tail ( reverse as1 ) -- drop last 
                              in reverse as2  
-- tail : SVG </g> drop everything before path only take the rest
        let hohlRest =  let as1 = drop (maximum iteM) (take (length stepper) (lines afile)) 
                        in as1
        let inseRRt = ("GraphicEngine17/Backbone/VektorBewegung/GamePosi"++ ( token)++".txt")
        josefGassner <- readFile inseRRt
        let checkAlles = (((unlines hohlersteZeilen)) ++("         "++( josefGassner )++"\n")++ (unlines( hohlRest)))  
        putStrLn (show iteM) --zeilen match PFAD
  --      putStrLn (show hohlersteZeilen) -- Header der <g>..</g> File
  --      putStrLn (show stepper) laenge input file
        putStrLn (show checkAlles) -- EXPORT: fertigen <g>..</g> String
        writeFile ("GraphicEngine17/Backbone/VektorBewegung/GamePosiWORKS"++ (token)++".txt") ( checkAlles)

 -- nur kleine Buchstaben; Heisst:
            -- zahlen weg
            -- leerstellen weg
            -- anfuehrungszeichen weg
-- Funktion fuer ItemReaderFinder s.u.
-- um P F A D zu filtern   
foKoordinates stream = let as2 = stream 
                       in let as6 = filter (>=97) as2
                in as6

-- turn "123" into Int 123

scanChar :: Char -> Int

scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

--scanString :: String -> Int a -> Double 
scanString = let ste = (go 0) 
             in zipWith (/) [1] 
    where go a [] = a
          go a (x:xs) | 0 <= sc && sc <= 9 = go (10*a+sc) xs
                      | otherwise = 0
              where sc = scanChar x

