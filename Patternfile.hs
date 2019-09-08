--This module exports a Patternfile-Reader and a Patternfile-Writer
--a Patternfile is a txt file with
-- line-format: type:String
--  (val,year,month,day) e.g > (0.59,16,Apr,9)
--
module Patternfile (
    writePatternFile -- Patternfile-Writer
  , aMemory  -- Patternfile-Reader
 -- , originalTxt
    ) where

-- import System.Data
import System.Random
import System.Environment
import System.IO
import Data.Char
import Data.List
import Control.Monad 

-----------------------------------------------------
--Global Variables
root = "c:/stack/..." 
token = 4 -- how many buttons to insert
bild1 = "c:/stack/forGlade/src/Many3.html" -- a storage file conected to 'Colored_2_3_5_Counter.hs'
writeHTAorHTML = 1 -- if == 1 then write HTA 
-----------------------------------------------------
--SelectorFunctions
ausw w fa = drop (w-1) (take w fa)
takerleiN w fa = ausw w fa
takerlein w fa = takerleiN w fa
getRid x = let a = (( x))
           in let b = reverse a
           in let c = truncate (read b) 
           in c

-----------------------------------------------------
--'searchCrit' must be part of the given DATA Set to succeed
-- 'searchCrit':String ; e.g> "0.56" 
searchCrit = "0.56"
-- fi:String which file to read e.g> "seniot.txt"
searchCritAdvance fi = do
    anyWalk <- readFile fi

    let bobbo2 =  ([(read startLine)..(read endline)])
    dipfade2 <- forM (bobbo2) (\ a -> do
          let auswahl      = let an =takerlein a (lines anyWalk)
                             in an

          let val = let an =  cvsZeilenBrea (concat auswahl)
                    in let boss = filter (>40) an 
                    in let go = map chr boss
                    in go
          let process = (concat auswahl) \\ show val
 
          let findYear =  let an =  (cvsZeilenBrea2 (process))
                          in map chr an 

          let year  = jahr findYear

          let finddate =  let an =  (cvsZeilenBrea3 (process))
                          in map chr an
     
          let monat =  month finddate
          let tag =day finddate
          let modus = if (read fomuster) == 1 then val --choose value
                      else if (read fomuster) == 2 then year
                      else if (read fomuster) == 3 then monat
                      else if (read fomuster) == 4 then tag
                      else val

          let dataTyp =  (modus)
          return (dataTyp) ) --a also nur zahlen
    let criterium = searchCrit
    let maxO = (maximum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))

    putStrLn (" the data is (ohne die extra Leerzeile) : " ++ maxO)

--------------
startLine = "1" 
endline = "990"
fomuster = "1" -- set to val
fohans = "1" -- Enter a value that does not occure and see its propability , set to fixed value  
--------------------------------------------
-- Write an simple csv reader
----------------------------------------------------------
-- enter x , the name of the Patternfile 
-- entername of the returned File, NF 
-- enter Begin und End Lines as (Int)
-- enter Criterium as an Int !!! BEARBEITET AB/einschlieslich ERSTES KRITERIUM!!!!
-- returns Max und Min Val 
-- returns Spot of Max vals int othe words with regards to the criteria
-- returns based on dipfade4 (Ocourance of the Groups a values ordered by their value  
-- returns  given values of the Spektrums of 0.56(min)...max, and its propability of ocourance
-- returns  missing values of the Spektrum  
aMemory x nF = do
   --  putStrLn$ "Enter starting Line:"
     let anfang = startLine -- anfang <- getLine
   --  putStrLn$ "How many Lines to change?:"
     let xX = endline -- xX <- getLine     
   --  database <- readFile "milch.txt"
     --let machAdd f = add [ nF, f]
     anyWalk <- readFile x -- z.b. "mageWalAllPath.txt"--durble.csv

   --  machAdd dataTyp 
     --let bilderno = length anyWalk  
     let bobbo =  ([(read anfang)..(read xX)])
     dipfade <- forM (bobbo) (\ a -> do
          let auswahl      = let an =takerlein a (lines anyWalk)
                             in an

          let val = let an =  cvsZeilenBrea (concat auswahl)
                    in let boss = filter (>40) an 
                    in map chr boss
          let process = (concat auswahl) \\ show val
 
          let findYear =  let an =  (cvsZeilenBrea2 (process))
                          in map chr an 

          let year  = jahr findYear

          let finddate =  let an =  (cvsZeilenBrea3 (process))
                          in map chr an
     
          let monat =  month finddate
          let tag = day finddate
          let mo3 = show(unlines(words finddate))
 
          let spuren = (einTyp2 (getRid (val))) 
          let dataTyp = (show spuren) --)++"\n"-- (show apunkt)++ "(Just "++(show tag)++ (show monat) ++ ")\n"


          let procIntern = head spuren 
          return (dataTyp) ) --a also nur zahlen
     putStrLn " the data (without empty spaces) : "
     mapM print(dipfade)
   --  add [nF, (concat dipfade)]
     writeFile nF (concat dipfade)
  --   putStrLn "enter what to edit val:1 ; year:2 ; month:3 ; tag:4 einfach Int eingeben"
     let muster = fomuster
  --   putStrLn " enter criterium: that is a value (nur Int eingeben)"
     let criterium = searchCrit -- criterium <- getLine ----------------------------********************* SEARCH ONE VAL IN THE SET
     let bobbo2 =  ([(read anfang)..(read xX)])
     dipfade2 <- forM (bobbo2) (\ a -> do
          let auswahl      = let an =takerlein a (lines anyWalk)
                             in an

          let val = let an =  cvsZeilenBrea (concat auswahl)
                    in let boss = filter (>40) an 
                    in let go = map chr boss
                    in go
          let process = (concat auswahl) \\ show val
 
          let findYear =  let an =  (cvsZeilenBrea2 (process))
                          in map chr an 

          let year  = jahr findYear

          let finddate =  let an =  (cvsZeilenBrea3 (process))
                          in map chr an
     
          let monat =  month finddate
          let tag =day finddate
          let modus = if (read muster) == 1 then val --choose value
                      else if (read muster) == 2 then year
                      else if (read muster) == 3 then monat
                      else if (read muster) == 4 then tag
                      else val

          let dataTyp =  (modus)
          return (dataTyp) ) --a also nur zahlen
  --   putStrLn " the data is (ohne die extra Leerzeile) : "
  --   putStrLn (show dipfade)
   --   let bobbo2 =  ([(read anfang)..(read xX)])
     let wievieleMx = length(snd (partition (>criterium) (dropWhile (\(val) -> val < criterium ) ( dipfade2))))
     
     dipfade3 <- forM (bobbo2) (\ a -> do
          
          
          let auswahl      = let an =takerlein a (lines anyWalk)
                             in an

          let val = let an =  cvsZeilenBrea (concat auswahl)
                    in let boss = filter (>40) an 
                    in let go = map chr boss
                    in go
          let process = (concat auswahl) \\ show val
 
          let findYear =  let an =  (cvsZeilenBrea2 (process))
                          in map chr an 

          let year  = jahr findYear

          let finddate =  let an =  (cvsZeilenBrea3 (process))
                          in map chr an
          let monat = month finddate
          let tag = day finddate
                        
          let modus = if (read muster) == 1 then val --choose value
                      else if (read muster) == 2 then year
                      else if (read muster) == 3 then monat
                      else if (read muster) == 4 then tag
                      else val
                       
          let punkte name a1 a2 a3 a4 a5  =  Punkt name a1 a2 a3 a4 a5
          let apunkt = punkte val Nothing Nothing Nothing Nothing Nothing 
       --   seeit <- getLine
          let apunkt2 = apunkt
          let dataTyp3 =  (apunkt)
          return (dataTyp3) ) --a also nur zahlen
     putStrLn " die Letzte der Ausgaben wo die Zeilen eine Refernezliste bekommen : "
  --   putStrLn (show dipfade)     
     
     let more = ((dropWhile (\(val) -> val < criterium ) ( dipfade2)))
     let laengeSet = length (dropWhile (\(val) -> val < criterium ) ( dipfade2)) --wie lang ist set
     let maxO = (maximum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))
     let minO = (minimum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))
     let wievieleMx = length(snd (partition (>criterium) (dropWhile (\(val) -> val < criterium ) ( dipfade2))))
     let wostehenMx = criterium `elemIndices` (dropWhile (\(val) -> val < criterium ) ( dipfade2))
     
     let comparePosition = let hochundtiefPunkt = if ""++(show laengeSet) == (criterium) then putStrLn "Found Maximal/Minimal Turning Point"
                                                  else putStrLn "\n\n\nNo Max or Minimal Turning Point found"
                           in hochundtiefPunkt

     mapM print (dipfade2)
     putStrLn ((show laengeSet) ++ "where is the criterium in the Sets")
     putStrLn ((show maxO) ++" maximum value of the Sets" )
     putStrLn ((show minO) ++ " minimm value of the Sets")
     putStrLn ((show wievieleMx)++ "how many maxima")
     putStrLn ((show wostehenMx)++ " where are the maxima\n\n")
 
    -- let mowork = filter 
     comparePosition 
     let lister2  = let a=  maxO
                    in let b = (read a)
                    in let a2 = (read minO)
                    in let c = ((b)-0.01)
                    in let d = [(b),(c)..(a2)]
                  --  in let e = drop 4 (take 5 d)
                    in let f = map round (map (*100) d)
                    in f
   
     let fuerdieFehlen = let a=  maxO
                         in let b = (read a)
                         in let a2 = (read minO)
                         in let c = ((b)-0.01)
                         in let d = [(b),(c)..(a2)]
                         in d     
    -- let neen er = let b = map er lister2
      --             in b
     
     
     let wos =  group more
                 

    -- let numliste = let a =  
       
     let spuren g h= (einTyp3 (getRid (g h)))
     dipfade4 <- forM (bobbo2) (\ a -> do
          
          
          let auswahl      = let an = drop (a-1) (take a (wos))
                             in   ( an)
 
       --   let bosi = let a = map ord ((show auswahl))
         --            in let b = break (==44) a 
           --          in map chr (fst b )


          let rosi =  ( take 1 (map length (concat auswahl)))

         -- putStrLn (show bosi)
          putStrLn (show rosi) 


          let dataTyp3 =  (auswahl)
          return (dataTyp3) ) 
         -- return (spuren) ) 
    
     putStrLn (show(dipfade2))
     putStrLn (show more) 
     putStrLn (show (lister2)++" the 'spectrum' including max und min in 0.1 steps\n\n")
     let zui = map length (group (concat (sort (concat dipfade4))))
     let zui11 = (map head (concat dipfade4))
    
     let zui1 = map length (group (concat (concat dipfade4)))-- die val in Zeitgruppen eines Intervalls  
     let zuu2 =  sort (nub ((concat((concat dipfade4)))))
     let zuu22 d =  map round (map (*100) d)
     let zuu3 =  (lister2 \\ (zuu22 (map read zuu2)) )
     putStrLn ((show dipfade4)++ "    Data of computation (run) 'dipfade4' needed for propability calculation\n")
     putStrLn ((show zui1)++ "  the occouring groups of numbers ordered in TIME ")
     putStrLn ((show zui11)++"  The values of the groups of numbers (see above)") 
     putStrLn ((show zui)++ "  The ocourance of values in spectrum min to Max")
     putStrLn ((show zuu2)++"  Occuring values connected with th line above(s.a)") 
     putStrLn ((show zuu3)++"  Not occuring values of the Set  x 100"   ) 
     let normWahrsch = let spektrum = lister2 --schliesst das ansolute min und max ein

---                  0    -> nuller Set um dem Computer Raum zu geben Werte zu 
--                   max     
--                   min
--                   0        waehlen im Backtest die noch NICHT vorkamen
                       in let extra = let a = length lister2
                                      in let b = a
                                      in let c = concatMap (replicate b) [0..0]
                                      in [c,lister2,c] 

                       in extra

  
     let normWahrsch1 = let dieZeut = sum zui1
                        in let dieAkteure = zui
                        in let rechner = let a z t = z/t
                                         in a 1 2

                        in [[dieZeut], zui]

     let form t c z = (100/(t+(2*c)))* z   
     let formelNormWahr dr = let a1 = let mitSpielraum = (length lister2)
                                      in let dieZeut =  ( sum zui)
                                      in show dieZeut 
                             in let a2 = show (length lister2)
                             in let a3 = zui
                             in let b1 i ii iii = show (form i ii iii)
                        --  in let b2 = b1 (a1 a2 (head a3)) 
                          
                             in b1 (read a1) (read a2) dr --[[a1],[a2],a3] 
     let hans = fohans -- enter non realized values , will be put 
     let gogos = let a =  map show zui 
                 in let b =  map formelNormWahr (map read a)
                 in  b 

     let heet = length( gogos)  
    
     let zurWahr de =  let a0 de = head de
                       in let a1 de = de \\ [(a0 de)]
                       in let a2  = iterate a1 de 
                       in (take heet a2)

     let gogo2 = gogos
                 
    -- let gogo3 = let a = map ord gogo2
               --  in let b = filter (=='"') a  
            --     in  a --b  
    -- let zurWahr2   =   ((zurWahr gogos))
                        -- let a0 de =  (head de)
                        --in let a1 de = (de) \\ [(a0 de)]
                        --in let a2  = iterate a1 de 
                        --in let a3 =  ((take heet a2))
                     --   map read gogos  

     putStrLn (show normWahrsch)
     putStrLn (show normWahrsch1)
     putStrLn (( formelNormWahr (read hans))++"% The propability to unrealized values in percent")
     putStrLn (show ( gogos)++"% The List of Formula-1 propabilities ")
     putStrLn (show ( heet)++"% total of all propabilities ")
     putStrLn (show ( zurWahr gogos )++"a Counter ")
    -- putStrLn (show ( gogo3  )++"fuer waehler ")


--Step4
--Zeilenbreak criterium fuer CVS datei  
--access to date: day   
cvsZeilenBreak s = let a = map ord s  --ord '-'=45
                   in let teile = takeWhile (/=45) a
                   in teile

--acces to date: Month,year
cvsZeilenBreak2 s = let a = map ord (s)  --ord '-'=45
                    in let teile = takeWhile (>=45) a
                    in teile

--acces to val
cvsZeilenBreak3 s = let a = map ord (s)  --ord '-'=45
                    in let teile = dropWhile (>=45) a
                    in teile

cvsZeilenBreak4 s = let a = map ord (s)  --ord '-'=45
                    in let teile = dropWhile (>45) a
                    in teile

--Step4
--Acces 4 readymade Pattern files
--Zeilenbreak criterium fuer CVS datei  
--access to date: day   
cvsZeilenBrea  s = let a = map ord s  --ord '-'=45
                   in let teile = takeWhile (/=44) a
                   in teile

--acces to val
cvsZeilenBrea2  s = let a = map ord (s)  --ord '-'=45
                    in let teile = break (>=57) a
                    in fst teile

--acces to val
cvsZeilenBrea3  s = let a = map ord (s)  --ord '-'=45
                    in let teile = break (>=57) a
                    in snd teile

cvsZeilenBrea4  s = let a = map ord (s)  --ord '-'=45
                    in let teile = dropWhile (>45) a
                    in teile

 






--schreibt neue Patternfile
--fills it with Data: val,year,month,day
--aus input CSV x 
--x: CSV-RAW (string) ;
--nF: name der zu bearbeitenden File (string)
--started 2. Zeile siehe bobbo d.h. PAtternfile ohne 
--Ueberschrift
--Bobbo wird nur der Laenge nach bearbeitet
--startet [2.... endet 50]
writePatternFile x nF= do
   --  database <- readFile "milch.txt"
     --let machAdd f = add [ nF, f]
     anyWalk <- readFile x -- z.b. "mageWalAllPath.txt"--durble.csv
     let leane = length anyWalk
     putStrLn ("File : "++ x++ "has: "++ (show leane) ++ " Lines\n"++" Which lines do you want to change enter start line:")

     startline <- getLine
     putStrLn ("Enter end line:") 
     endline <- getLine
   --  machAdd dataTyp 
     --let bilderno = length anyWalk  
     let bobbo =  ([(read startline)..(read endline)])
     dipfade <- forM (bobbo) (\ a -> do
          let auswahl      = let an = takerlein a (lines anyWalk)
                             in an

          let tag = let an =  cvsZeilenBreak (concat auswahl)
                    in map chr an
          let process = (concat auswahl) \\ show tag
 
          let findYear =  let an =  (cvsZeilenBreak2 (process))
                          in map chr an 

          let year  = let an = show findYear
                      in let b = map ord an
                      in let c = filter (>45) b --schmeisst kommas raus
                      in let d = filter (<65) c -- laesst nur Zahlen ueber 
                      in map chr d -- bleiben nur Jahreszahlen
     
          let monat = let an = show findYear
                      in let b = map ord an
                      in let c = filter (>45) b --
                      in let d = filter (>64) c -- laesst nur Buchstaben ueber 
                      in map chr d -- bleiben nur Jahreszahlen
             
 
          let findval =  let an =  (cvsZeilenBreak3 (process))
                         in map chr an
          let val = let an = show findval
                    in let b = map ord an
                    in let c = filter (>45) b 
                    in map chr c 
          let mo3 = show(unlines(words findval))

          let dataTyp =  ""++"("++val++","++year++","++monat++","++tag++")\n"
          return (dataTyp) ) --a also nur zahlen
     putStrLn " the data is without empty spaces : "
     mapM putStrLn (dipfade)
     writeFile nF (concat dipfade)

--Beispiel Random Function
-- a :: Int 
zufallsGen a x = (take a  (randomRs (1,6) (mkStdGen x)))::[Int] -- Random zwischen 1 und 6, der laenge a immer 6 Startwert
zufallsGen2 a = (take a  (randomRs (1,a) (mkStdGen 10)))::[Int] -- Ran. 1 bis a laenge a 
zufallsGen3 a = (take a  (randomRs (1,(12*a)) (mkStdGen a)))::[Int] -- Ran
zufallsGen4 t a x = (take t  (randomRs (1,a) (mkStdGen x)))::[Int]


tre = random (mkStdGen 12) :: (Int, StdGen)
tre1 = random (mkStdGen 13) :: (Int, StdGen)
tre2 = random (mkStdGen 14) :: (Int, StdGen)

tre3 b = let a1 = random (mkStdGen b) :: (Int, StdGen)
         in abs (fst a1)  

aRandom x = random (mkStdGen x) -- wird mit 

-- ist eine Zahl die drei Werte beinhalted um 
-- fuer jede ZAhl aus jedem Set eine Individuelle
-- zahle erzeugt
-- x: name der datei


forRandom x val  
                = let nameanders = (map ord x)
                  in let weiter =  (concat((map (:val) [(length nameanders)])))
                 
                  in head weiter --[weiter,weiter2,val,day,month]
              
weiter2 x = (sum (map ord x))
takeval val = val
takedate day = day
id1 month = month 


value ausw= let an =  cvsZeilenBrea (concat ausw)
            in let boss = filter (>40) an 
            in map chr boss

jahr findY  = let an = show findY
              in let b = map ord an
              in let c = filter (>45) b --schmeisst kommas raus
              in let d = filter (<65) c -- laesst nur Zahlen ueber 
              in map chr d -- bleiben nur Jahreszahlen

month findda = let an = show findda
               in let b = map ord an
               in let c = filter (>45) b --
               in let d = filter (>64) c -- laesst nur Buchstaben ueber 
               in map chr d -- bleiben nur Jahreszahlen

day dindda = let an = show dindda
             in let b = map ord an
             in let c = filter (>45) b
             in let d = filter (<65) c 
             in map chr d
             

--
einTyp f = [((plugit f 2),(stelle2er f)),((plugit f 3),(stelle3er f)),((plugit f 5) ,(stelle5er f))] 

einTyp2 f = [((plugit2 f 2),(stelle2er f)),((plugit2 f 3),(stelle3er f)),((plugit2 f 5) ,(stelle5er f))] 
einTyp3 f g = [(plugit2 f g)]

--------------------------------------------------------------
--------------------------------------------------------------
--Colored Circles/FARBGEBER KUGELN
--------------------------------------------------------------

bougaer x= let gaga =(read x)
           --in let step1 = counterDreierKugel x
           in let tessen = ((gaga)-1)  
           in let boui = stelle3er gaga --  if (charly x) == True then counterDreierKugel (show tessen)
                          --else step1
           in let step2 = if (boui) == 1 then show "green"
                          else if (boui) == 2 then show "red"
                          else  "blue" --if (boui) == 3 then show "blue"
                         -- else if (boui) == 4 then show"green"
                       --   else if (boui) == 5 then show "red"
                         -- else show "green"
--in let step3 = if step2 == 1 then  show "green"
  --                        else if step2 == 2 then show "red"
                  --        else show "blue" 
           in step2


bougaer2 x= let gaga =(read x)
           --in let step1 = counterDreierKugel x
            in let tessen = ((gaga)-1)  
            in let boui = stelle3er gaga --  if (charly x) == True then counterDreierKugel (show tessen)
                         
            in let step2 = if (boui) == 1 then 1
                           else if (boui) == 2 then 2
                           else  3 --if (boui) == 3 then show "blue"
                         
            in step2
-- 
-- Der Datentyp fuer die 2er,3er,5er listen
--  2er: type: 2 -> [1,2,] [3,4]... der Farbcode
--  3er : type: 3 -> [1,2,3] [4,5,6]...   "
--  5er. type 5 -> [1,2,3,4,5] [6,7,8,9,10] ...
--  wird in Farbgeber (baugaer) eingep
--  y = choose modus 2,3,5
--  x = input
arc t f = (t/f)

datenTypZahlen y x =( take y (concatMap (replicate x) [1..y]))



datenTypZahlen2 x f = let a = drop (f-1) (take f (concat (take f (repeat [1..x])))) 
              --in let listel  = last (take f a)
                     in a 
-- 

-- ---------------------------------------------------
-- ENTGUELTIGE FARBAUSGABE DER KUGELN
-- ----------------------------------------------------
-- choose modus s. oben 
plugit input chosemodus =    let a x = map bougaer (map show x)
                             in let b = (datenTypZahlen input chosemodus) 
                             in let bbeide = a b
                             in last bbeide
--------------------------------------------------------------
plugit2 input chosemodus =   let a x = map bougaer2 (map show x)
                             in let b = (datenTypZahlen input chosemodus) 
                             in let bbeide = a b
                             in last bbeide
--------------------------------------------------------------




--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-- SPUREN 2ER 3ER 5ER
-- -----------------------------------------------------------
stelle2er f = let a = concat (take f (repeat [1,2]))
              in let listel  = last (take f a)
              in listel 


stelle3er f = let a = concat (take f (repeat [1,2,3]))
              in let listel  = last (take f a)
              in listel 

----------------------------------------------------
----------------------------------------------------



stelle5er f = let a = concat (take f (repeat [1,2,3,4,5])) 
              in let listel  = last (take f a)
              in listel 


--check also :2731 for usage as GUI Arcitecture in jahresInterval

data Punkt = Punkt {name::String, mother::Maybe Punkt, father::Maybe Punkt,mother2::Maybe Punkt,father2:: Maybe Punkt,middle::Maybe Punkt}
-- Punkt "what" 

-- we show sheep by name
instance Show Punkt where
  show s = show (name s)
	

-- traceFamily is a generic function to find an ancestor
richtungHoeheY :: Punkt -> [ (Punkt -> Maybe Punkt) ] -> Maybe Punkt
richtungHoeheY s l = foldM getParent s l
  where getParent s f = f s

-- we can define complex queries using traceFamily in an easy, clear way
--BEISPIEL:
--paternalGrandmother        ssnd (break (==46) ab)snd (break (==46) ab) = traceFamily s [father, mother]
--mothersPaternalGrandfather s = traceFamily s [mother, father, father]

-- AN DIESERR STELLE KAN EIN ANDERES SYSTEM ZUR 
-- BESCHREIBUNG DES GEWAEHLTEN PFADES EINGSETZT WEREDEN
-- ZUR vEREINFACHUNG WIRD ERST ALLES WIE IM BEISPIEL DataReihen.hs
-- PROGRAMMMIERT

logikPfadinYRichtung        s = richtungHoeheY s [mother, father] --[geradeaus, rechts]
mothersPaternalGrandfather s = richtungHoeheY s [mother, father, father]
--
-- 1.DIE OBIGEN pFAD SUCHFUNKTIONEN
-- 2.DIE STAMBAUMFUNKTION (INTERGRIERTER BINAEBAUM)
--
--
--
--DEM SET KOMMT EINE BESONDERE BEDEUUTNG ZU DA
--ES IN EINEM ES EINE QUELLE IN SICH SELBST IST 
--(INJECTIV 
--UND  
--
--
see s = richtungHoeheY s [father,father]
-- this allows the user to name the mother and father functions on the command line
-- kann hier nichteuklidsche logik programmiert werden?
getFunctionByName :: String -> (Punkt -> Maybe Punkt)
getFunctionByName "father" = father
getFunctionByName "mother" = mother
getFunctionByName "mother2" = mother2
getFunctionByName _        = error "Invalid function name - not 'mother' or 'father'"


bogus = "Greenthump"
-- this builds our sheep family tree
zweierDiagram :: Punkt

zweierDiagram  = let twos = Punkt "Gorden" Nothing Nothing Nothing Nothing Nothing
                 in let three = Punkt "red" Nothing Nothing Nothing Nothing Nothing               
                 in let two = Punkt "father"   (Just twos) Nothing Nothing Nothing Nothing       
                 in two


