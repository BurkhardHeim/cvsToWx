--This module exports a Patternfile-Reader and a Patternfile-Writer
--a Patternfile is a txt file with
-- line-format: type:String
--  (val,year,month,day) e.g > (0.59,16,Apr,9)
--
module Patternfile (
    writePatternFile -- Patternfile-Writer
  , aMemory  -- Patternfile-Reader
  , ausw
 -- , originalTxt
    ) where

-- import System.Data
import System.Random
import System.Environment
import System.IO
import Data.Char
import Data.List
import Control.Monad

-- my modules
import Diff_Functions1 as Df
import Fourier_Functions1
import WriteWXmaximaFile
import GUIMenuGHCtext	 

-----------------------------------------------------
--Global Variables
root = "c:/stack/..." 
token = 4 -- how many buttons to insert
bild1 = "c:/stack/forGlade/src/Many3.html" -- a storage file conected to 'Colored_2_3_5_Counter.hs'
writeHTAorHTML = 1 -- if == 1 then write HTA
fostartLine = "1" -- startline pattermfile-reader in 'aMemory'
foendline = "3000"-- endline patternfile-reader in 'aMemory'
rnd = "\"100\"" -- for random number generator
fomuster = "1" -- set to val
fohans = "77" -- Enter a value that does not occure and see its propability , set to fixed value  
 
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

    let bobbo2 =  ([(read fostartLine)..(read foendline)])
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
--------------------------------------------
-- Write an simple csv reader
----------------------------------------------------------
-- enter x , the name of the Patternfile 
-- entername of the returned File, NF 
-- enter Begin und End Lines as "Int" ( a String representing an Int)
-- enter Criterium as a String !!! BEARBEITET AB/einschlieslich ERSTES KRITERIUM!!!!
-- returns Max und Min Val 
-- returns Spot of Max vals with regards to the criterium
-- returns based on dipfade4 (Ocourance of the Groups a values ordered by their value  
-- returns  given values of the Spektrums of 0.56(min)...max, and its propability of ocourance
-- returns  missing values of the Spektrum  
aMemory x nF = do
   --  putStrLn$ "Enter starting Line:"
     let anfang = "1" --fostartLine -- anfang <- getLine
   --  putStrLn$ "How many Lines to change?:"
     let xX = "365" --foendline -- xX <- getLine     
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
   --  writeFile nF (concat dipfade)
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

   --  mapM print (dipfade2)
     putStrLn ("just red set of length:"++ (show laengeSet) ++ " where is the criterium in the Sets")
     putStrLn ((show maxO) ++" maximum value of the Sets" )
     putStrLn ((show minO) ++ " minimm value of the Sets")
     putStrLn ((show wievieleMx)++ "how many maxima")
     putStrLn ((show wostehenMx)++ " where are the maxima\n\n")
 
    -- let mowork = filter 
    -- comparePosition 
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
         -- putStrLn (show rosi) 


          let dataTyp3 =  (auswahl)
          return (dataTyp3) ) 
         -- return (spuren) ) 
    
   --  putStrLn (show(dipfade2))
   --  putStrLn (show more) 
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

                 
    -- let gogo3 = let a = map ord gogo2
               --  in let b = filter (=='"') a  
            --     in  a --b  
    -- let zurWahr2   =   ((zurWahr gogos))
                        -- let a0 de =  (head de)
                        --in let a1 de = (de) \\ [(a0 de)]
                        --in let a2  = iterate a1 de 
                        --in let a3 =  ((take heet a2))
                     --   map read gogos  
     ------------------------------------------------- +++++++++++++++++++++++++++++++++++ inserted 08-08-2019
     ---------------------------------------------------------------
--Monade  RAUSCHGENERATORS/ZUFALLSGENERATORS
-- in diade werden die vals in Zufallsgen eingesetzt um
-- um fuer (val,zeit)-Paare ein indiviuelle id zu schaffen  
     let gogo2 = gogos
     let bobbo44 =  ([(read anfang)..(read xX)])
     diade <- forM (bobbo44) (\ a -> do
          let auswahl      = let an =takerlein a (lines anyWalk)
                             in an
          let val = value auswahl
          let process = (concat auswahl) \\ show val
 
          let findYear =  let an =  (cvsZeilenBrea2 (process))
                          in map chr an 
          let year  = jahr findYear 
          let finddate =  let an =  (cvsZeilenBrea3 (process))
                          in map chr an
          let monat = month finddate
          let tag = day finddate
                   
          let mitZufall t x c = zufallsGen4 t x c  -- :520
--bestimmt einen individuellen Int Wert fuer zeilen ist aber pro Set nur 
--einmal anwendbar da sich ausgabe nie aendert
          let signatur = let a = 100* (read val)
                         in truncate (sum [a , (read tag ) , (read year),(monthTime monat)])------------------------------------------INPUT UM ZUFALLSGENERATOR ZU MANPULIEREN 
--bestimmt die Laenge der Liste aus zufalsgen die eingelesen wird
          let makene = (length dipfade2)
          let einsatz =  mitZufall  makene (read rnd) (signatur) 

            
          let dataTyp = (show einsatz)

 --grossesSPektrum mit formel : form2

          let bert = formelNormWahr (read val) 

          let chooser = let a1 ert= drop (a-1)(take a ert)
                        in  let b1 = (show (a1 einsatz))
                        in let c1 =   (a1 bert)
                        in let b11 =  let ans =  (map ord b1)
                                    -- filtert "
                                      in let bns = filter (>35) ans

                                      in let bnf = filter (<91) bns
                                      in map chr bnf
                       -- in if  b11<=c1 then b1
                         --  else (b11)  -- ansatz zum berechnen von TAKER fuer % auswahlfu
                        in (b11)

          let chooser2 = let nutzeAlle = filter (/= a) [1..makene]  -- zeige alle Randoms die chooser nicht brauchte
                         in (nutzeAlle ) --mehr) --(nutzeAlle) --(hohlRando 1) ---b11 a)-- (meinA 2) --ungenutzte -- nutzeAlle --(meinA (4)) --nutzeAlle --( ungenutzte)

          return (chooser) ) --a also nur zahlen
  --   putStrLn ((head  diade)) -- 36,31,62

-- : ende grosse Klammer 6254
    -- putStrLn (show (diade)++"\n")
     let humfrey = let apart = "1" -- (head diade)
                   in let a1  =  (map ord (unlines ( diade)))
                   --in let a2 = filter (<5) a1
                   --in let a22 = filter (<57) a2
                   in  (words (map chr a1))
  
     let zulu = humfrey
---  shows example how Rules for Interval can be programmed 
     let fogorch = (map read (concat(concat dipfade4))) 
     let gorch =  similaritYvalue fogorch [11,12,34,44]
  
     let gorchH = zui 


------------------------------------------------------------------------------------
-- BSP fuer String Spektrum umwandlung string -> Int 
-- Berechnet die Werte im PICKER d.h. hier wandert
-- eine Zaehlfunktion durch die liste aller Wahrscheinlichkeiten
-- des geg. Sets 
-- sie soll Intervalle schaffen durch welche sich der PICKER ORIENTIERT

-- mittels pick < (takerlein n prego) ....Anhand der            
     let prego xw= let a00 =   xw
                   in let a1 a00= drop (a00-1) (take a00 (zurWahr  gogos))
                   in let a2 = ((show ((concat (a1 a00)))))
                   in let foPick g = map isNumber g
                   in let b11 =  let ans =  (map ord a2)
                                 -- filter punkt
                                 in let bns = filter (>46) ans
 
                                 in let bnf = filter (<91) bns
                                 in  ( map chr bnf) -- map digitToInt ( map chr bnf)
                             --  in map 
                             --
                  --WWWWWWWWWWWWWIIIIICCCHHHTIIIIIIIIGGGGG   
                   in let b22 = let am = map digitToInt b11 -- schafft Int fuer prozess
                                in let am2 =  am --prozentrechnung
                                in  am2
                   in let b3  = let a2 = (take 2 b22) 
                                 in sum (zipWith (*) [1,1] a2)
                   in let b33  = let a21 = take 2 b22 
                                 in let a3 = sum (zipWith (*) [10,1] a21)
                                 in let bhz = (drop 2 )(take 4 b22)
                                 in  a3

                 --  in let b44 = take 4
                 
                   in let a5 = let a2a = take 4 b22  --Int
                               in let a2aa = drop 3 (take 4 a2)  -- String
                               in let a4 = head (drop 3 a2a) -- Int 
                               in let a4a = head (drop 2 (take 3 b22))
                              -- in let teilerr = [(nullStell a4a a4)] 
                               in if  ((foPick ( a2aa))== [True]) then [b33]
                                  else let zuuu = b33 --a2a--(a4/10)
                                       in (take 1 b22)  --zuuu --a2aa
                                  
                --   in let picker = let a111 = drop (3) (take 4 foPick)
                  --                 in let b = if (a111 == [True]) then [b33]
                    --                          else  [b3] --(take 1 a1)
                      --             in b 
                                            
                  -- in let bchoose = if x == 1 then 
                   in a5 -- (take 7 a2) --picker --(a2,b11,b22,b3,picker)

----------------------------------------------------------------------------
--  ALTERNATIVER ANSATZ Formeln: prego,
-- hier Wird versucht die Umwandlung von zufallsgen
-- in simuliete Zahlen einfacher zu prgrammieren
--
-- ALTERNATIVER ANSATZ ZU PREGO s.o.
-- Function um Formel1Wahrscheinlichkeitsliste zu
-- vervielfaeltigen um Genauigkeit zu erhoehen
-- z.B. prego: ["65.3454...3","24,6775..4"]
-- IO: Line 3566
     let chgeWahr2Prego   = let --a1 d = prego d                              
                              --  recurse2Str x y = (\x y -> [x,y])x y
                              --  recurse1Str x = (\x -> [x]) x
                                a2 = ((show gogos)) --(unlines gogos)
                                filtern = let a11 = (map ord a2) -- [Str] -> [Int]
                                              a22 = filter (/=34) a11 --filter ""
                                          in map chr a22 --(map chr a22)
                            in (filtern) -- a2 --(filtern) --(a1 d)     
     let addder = let zdr = (chgeWahr2Prego)
                     in ( zdr)

------------------------------------------------------------
     let fert f = let a1 = [1..(length gogos)]
                  in let b1  = concat (map f a1) 
                  in let nehmer n  = take n b1
                  in let giver = map nehmer a1 
                  in map sum giver --map sum giver  

     let gert f   = let a1 = [1..(length gogos)]
                    in let b1  = concat (map f a1) 
                    in let nehmer n  = take n b1
                    in let giver = map nehmer a1 
                    in giver --map sum giver 
     let bro n= let a1 = bobbo
                in let nehmer n m = take n m
                in let zufall1 n  = nehmer n (fert prego)
                in let zufall2 n =  nehmer n humfrey
                in let valuees n= nehmer n zuu2
                in let giver1 = last (map zufall1 a1)
                in let giver2 = last (map zufall2 a1) 
               
 
                in let gurt n = let zfg = (length (concat(drop (n-1) (take n giver2)))) 
                                in zfg
                in let picker = map gurt a1                  
                in picker --(giver1,giver2,givVal,chngeStr,gurt n, picker) --map sum giver  
                
--diese Funktion wandelt die zufallsliste aus zufallsgen -> String
--in Int um , xw= Int , wird in gert gemapped 
     let preAI xw g = let im1 = bobbo
                   in let nehmer n m = take n m
                   in let zufall1 n  = nehmer n (fert prego)
                   in let zufall2 n =  nehmer n humfrey
                   in let valuees n= nehmer n zuu2
                   in let giver1 = last (map zufall1 im1)
                   in let giver2 =  last (map zufall2 im1) 
                   in let givVal = last (map valuees im1)
                   in let 
                   in let a00 =   xw
                   in let a1 a00= drop (a00-1) (take a00 (giver2))
                   in let a2 = ((show ((concat (a1 a00)))))
                   in let foPick g = map isNumber g
                   in let b11 =  let ans =  (map ord a2)
                                 in let bns = filter (>46) ans
 
                                 in let bnf = filter (<91) bns
                                 in  ( map chr bnf) -- map digitToInt ( map chr bnf)
                             --  in map 
                             --
                  --WWWWWWWWWWWWWIIIIICCCHHHTIIIIIIIIGGGGG   
                   in let b22 = let am = map digitToInt b11 -- schafft Int fuer prozess
                                in let am2 =  am --prozentrechnung
                                in  am2
                   
                   in b22 -- (take 7 a2) --picker --(a2,b11,
--mapped preAI abhaengig von laenge wahrscheinlichkeits liste zum abzaehlen
     let eindelijk f = let ar fu = (preAI fu 1)
                      in let ar2 fu = length (ar fu)
                      in let arGo = map ar bobbo44
                      in let rech fu = sum(zipWith (*) [10,1] (ar fu))
                      in if ((ar2 f) == (2) ) then rech f
                         else  sum (ar f)
  
     let eindelijkGo = map eindelijk bobbo 
 
     let ghijst n  = let art z = drop (z-1) (take z eindelijkGo)-- int zufallsgen vorkommnis
                     in let art2 z =  drop (z-1) (take z (fert prego)) -- int abzaehlliste
                     in let art22 z =  drop (z) (take (z+1) (fert prego)) -- int abzaehlliste
                     in let art23 z =  drop (z+1) (take (z+2) (fert prego)) -- int abzaehlliste
                     
                     in let art3 z =  drop (z-1) (take z (zuu2))-- val die vorkommen
                     in let bart =   (map art bobbo) 
                     in let bart2 = ( map art2 bobbo)
                     in let bart22 =  (map art22 bobbo)
                     in let bart23 =  (map art23 bobbo)
                     
                                  
                     in let rtzu = sort (concat [((art n)), (fert prego)])--spektrum mit vorkommnis
                     in let fowo g t = (head (g t))
                     in let gthe = let gds = partition (<= (fowo art n)) rtzu
                                   in gds
                     in let gthe2  = length (fst gthe)
                     in let gthe3 = art3 gthe2
                     in ((art n , gthe), (gthe2, gthe3))
{-             
                     in if ((art n) >= (art2 n) && (art n) <= (art (n+1))) then (art3 (2))
                        vorkommnis 
                        else if ((art n) >= (art2 n) && (art n) <= (art2 (n+2)))  then (art3 (3))
                        else if ((art n) <= (art2 n) && (art n) <= (art (n+1))) then (art3 (1)) 
                        else (art3 3)
 -}
     let goghijst = let wer k = (snd(snd (ghijst k)))
                    in (map wer bobbo)
     --let gerri =  (map ghijst bobbo)
     putStrLn ((show zui)++ "  Ist das Vorkommen der Zahlen im Spektrum von min nach Max")
     putStrLn ((show zuu2)++"  Die Zahlen die Vorkmmen in Verbindung mit s.o.") 
    
     let addIA d gz = let theOther = d : (fert gz)
                      in theOther

--How to make an Interval
-- Der erste durchschnittliche Wert vor einem Hochpunkt 
--  "   "         "              " nach   "     "      
--  Wir brauchen einen Gruppen-Paarungs Durchschnitt
--  d.h. jedes Jahr hat einen individuellen zeitliche Gruppen
--  vorkommensliste die Anhand von Zeitintervallpunkten
--                 ___
--      /\     ___/   \
--     /  \___/        \___/
--     _____________________________
--      | |   |   |   ||   |  time-interval-points
--
--      die ueber n Jahre miteinander verglichen werden
--      daraus ergibt sich zu welcher zeit bestimmte Punkte
--      in einer wahrscheinlichkeit vorkommen koennten
-- wir brauchen einen mehrjaehrigen Ueberblick. D.H die einzugebende Liste muss 
-- am besten so viele jahre wie moeglich beinhalten. Diese werden statistisch
-- mit den zeitintervallpunkten  berechnet. 
    -- let yearAnlys = let zu3 = concat goghijst
      --                 in let zu32 = length zu3
        --               in let zu33 = take 20 zu3
          --             in let zu34 = zu3 \\ zu33 
            --           in  (reverse (sort zu33))
  -- 
     -- putStrLn (show normWahrsch)
    -- putStrLn (show normWahrsch1)
     putStrLn (( formelNormWahr (read hans))++"% Es gibt auch eine Chance auf nicht verwirklichte Werte")
     putStrLn (show ( gogos)++"% Dies ist die Liste mit Formel 1 wahrscheinlichkeiten ; function: gogos")
    -- putStrLn (show ( heet)++"% gesamt  wahrscheinlichkeiten ")
   --  putStrLn (show ( zurWahr gogos )++"zurwahrliste schafft PICKER fuer % ; function: zurWahr gogos ")
    -- putStrLn (show (gorch )++"Ahenlichkeit wahrscheinlichkeit ")
     --putStrLn (show (gorchH )++" ")
   --  putStrLn (show ( humfrey )++"BSp bereinigtes Format string aus monade; function: humfrey ")
     putStrLn (show (prego  1)++ " BSP Prozentrechener als Int leider nur fuer ganze zahlen; function: prego 1 " )
     putStrLn (show (fert prego  )++ " Die Additive Liste zum Abzaehlen und einfuegen von Wahrscheinlichkeitsaenderung\n"++ "; function: fert prego"  )
    -- putStrLn (show (bro 1 ) ++ " vergleich von Zaehler und Zufallsgenerator")
    -- putStrLn (show (preAI 2 2 ) ++ " vergleich fuer Zufallsgenerator einzelne zahl")
    -- putStrLn (show (eindelijkGo ) ++ " vergleich fuer Zufallsgenerator liste gogos")
     putStrLn (show (addIA 1729 prego ) ++ " fuegt prozent der additiven liste zu; function: addIA 1729 prego")
    -- putStrLn (show (ghijst 1 ) ++ " test fuer map additive Liste mit zufall ")
    -- putStrLn (show (goghijst  ) ++ " s.o. Liste " )


     ----------------------------------------------------------------------------------------------------------------------------
     putStrLn (show normWahrsch)
     putStrLn (show normWahrsch1)
     putStrLn (( formelNormWahr (read hans))++"% The propability to unrealized values in percent")
     putStrLn (show ( gogos)++"% The List of Formula-1 propabilities ")
     putStrLn (show ( heet)++"% total of all propabilities ")
     putStrLn (show (head( zurWahr gogos ))++"a Counter ")
      -- putStrLn (show (gorch )++"Ahenlichkeit wahrscheinlichkeit ")
     --putStrLn (show (gorchH )++" ")
    -- putStrLn (show ( humfrey )++"BSp bereinigtes Format string aus monade; function: humfrey ")
     --putStrLn (show ( (prego  1))++ " BSP Prozentrechener als Int leider nur fuer ganze zahlen; function: prego 1 " )
     putStrLn (show ((fert prego ) )++ " Die Additive Liste zum Abzaehlen und einfuegen von Wahrscheinlichkeitsaenderung\n"++ "; function: fert prego"  )
    -- putStrLn (show (bro 1 ) ++ " vergleich von Zaehler und Zufallsgenerator")
    -- putStrLn (show (preAI 2 2 ) ++ " vergleich fuer Zufallsgenerator einzelne zahl")
    -- putStrLn (show (eindelijkGo ) ++ " vergleich fuer Zufallsgenerator liste gogos")
    -- putStrLn (show (addIA 1729 prego ) ++ " fuegt prozent der additiven liste zu; function: addIA 1729 prego")
    -- putStrLn (show (ghijst 1 ) ++ " test fuer map additive Liste mit zufall ")
    -- putStrLn (show (goghijst  ) ++ " s.o. Liste " )

    -- putStrLn (show ( gogo3  )++"fuer waehler ")
    --x nF crit --openB file to open ; -openA file to write ; forD criterium
                                               ------------------------------------------------------------------------------------OSZILLOSKOP SHOS INPUT AND RANDOM
     let mixWithMQ4                           =  let ay2 p = fourier1MQ4 p + fourier2MQ4 p + fourier3MQ4 p + fourier4MQ4 p 
                                                 in let ay3 =  (6.28319901) -- NullStelle Des Intervals
                                                 in let ay4 = (ay3/ (read xX)) -- takes forC howmany lines  
                                                 in let soMany2take fak = ay4 * fak -- determines which value of fourier123 2 select
                                                 in let forThis fak = fourierMQ4PAN (soMany2take fak)
                                                 in let fofoThis = map forThis [1..(10)]
                                                 in fofoThis --intoConsider
     -- lists  real values or simulated ones into a function
-- inotConsider -> realVals or simulVals -> g -> function (g)  
  {-   let intoConsider = let inpU2 =  (concat goghijst) -- quelle simulierte vals
                        in let inpU3 = (map show bobbo) -- laenge val liste 
                        in let inpU4 = concat (concat dipfade4) -- real Input VALS
                        in let inpU5 =  (zipWith (+) ( mixWithMQ4) (map read humfrey))  -- fourier123 and simulated vals
              -- in let inpU6 =  (zipWith (+) ( mixWithMQ4) (read inpU4))  -- fourier123 and REAL input vals
                        in [inpU2,inpU3,inpU4,inpU5] -}
{-
     let outPutMaxima3 x = let generateList x0 = [Df.differenzwerte x0, Df.differenzwerte3 x0,Df.differenzwerte4 x0,Df.differenzwerte5 x0,Df.differenzwerte6 x0,Df.differenzwerte7 x0]
                  in let ofLength = map generateList [1..(x)]
           --       in let fourier4 = head (ausw 3 intoConsider) -- fourier (realVals)
                  in let realVals = (concat goghijst) -- the realVals
                   -- will make neat lists of length 2 that represent one 'discrete 2dplot' point derived from rigt above
                   -- as list of point -> passed to "file2Write.wxm"
                  in let kerry x y = zipWith (\x y -> [x,y]) x y -- a-- (zip a b)
                  in let  fofiltern x y= show (kerry x y)
                  in let filtern  xOn y = let tussenstap  = map ord (fofiltern xOn y)
                                              tss2 = filter (/=34) tussenstap
                                          in map chr (tss2)
                  in [(filtern xOn ofLength),(filtern xOn ofLength)] -- ,Df.differenzwerte4 x,Df.differenzwerte5 x,Df.differenzwerte6 x,Df.differenzwerte7 x]

                   -}
     putStrLn ""

-----------------------------------------------------------------------


--------------------------------------------------------------------------------------------------------------------------------
-- APLLIES A FUNCTION to input data and exports them to wxmaxima
-- a function can be applied to evey data point (x y)

xOn = [[1.0,2.0,3.0]]
-- recieves: the real values (realVals):[String]
--           the simulated (simuVals) (not realized yet)
--           'intoConsider' the fourier Stream : [String]; applied to realVals or simuVals

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
     let leane = length (lines anyWalk)
     
     let startline = "2" -- startLine <- getLine
   --  putStrLn ("Enter end line:") 
     let endline = foendline -- endline <- getLine
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
                      in let c = filter (>45) b --get rid of the commas
                      in let d = filter (<65) c -- just leave number digits  
                      in map chr d -- only the year number remains
     
          let monat = let an = show findYear
                      in let b = map ord an
                      in let c = filter (>45) b --get rid of the commas
                      in let d = filter (>64) c -- just leave letter digits  
                      in map chr d -- only the month letters remains
             
 
          let findval =  let an =  (cvsZeilenBreak3 (process))
                         in map chr an
          let val = let an = show findval
                    in let b = map ord an
                    in let c = filter (>45) b 
                    in map chr c 
          let mo3 = show(unlines(words findval))

          let dataTyp =  ""++"("++val++","++year++","++monat++","++tag++")\n"
          return (dataTyp) ) --a also nur zahlen
     putStrLn ("File : "++ x++ "has: "++ (show leane) ++ " Lines\n")

     mapM putStrLn (dipfade)
     writeFile nF (concat dipfade)
     putStrLn ("read : "++ x ++" -> wrote: "++nF++"  with: "++(foendline)++" of "++(show leane)++" lines selected") 

----------------------------------------------------


----------------------------------------------------
--Example Random Function
-- a :: Int 
zufallsGen a x = (take a  (randomRs (1,6) (mkStdGen x)))::[Int] -- Random between 1 and 6, of length a always 6 as values
zufallsGen2 a = (take a  (randomRs (1,a) (mkStdGen 10)))::[Int] -- Ran. 1 bis a laenge a 
zufallsGen3 a = (take a  (randomRs (1,(12*a)) (mkStdGen a)))::[Int] -- Ran
zufallsGen4 t a x = (take t  (randomRs (1,a) (mkStdGen x)))::[Int]

-- more random number generator functions
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
----------------------------------------------------



--CSV-related selection and thieving functions for main pipes 'aMemory' and 'writePatternFile'
----------------------------------------------------              
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
               in let d = filter (>64) c -- just leaves letters 
               in map chr d -- the month letters remain

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
-- The Datatype suited the 2er,3er,5er listes
--  2er: type: 2 -> [1,2,] [3,4]... with colorcode
--  3er : type: 3 -> [1,2,3] [4,5,6]...   "
--  5er. type 5 -> [1,2,3,4,5] [6,7,8,9,10] ...
--  is inserted into color giver 'baugaer'
--  y = choose mode 2,3,5
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
-- TRACKS 2ER 3ER 5ER
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


----------------------------------------------------------------------------------
-- this function rates the similarity of two
-- Lists vergleicht Aehnlichkeit und gibt 
-- die Prozentzahl der einer stelle 
-- der beiden listen aus
--
foaehnli1 a b = (a-b)
foaehnli2 a b = (b-a)
--umrechnen in % mit a < b und b > a  
aehnlichF a b = let a1 = if a > b then ((foaehnli1 a b)/ (a/100) )
                         else if a<b then  ((foaehnli2 a b)/ (b/100) )
                         else 0
                in let b1 g h = ((g) / ((h)/100)) 
                in a1 

 
similaritYvalue li la = let a = la -- z.b. [11,12,34,44]
                        in let b = li 
                        in let c1 w = sum w
                        in let c2 = c1 a--length
                        in let c3 = c1 b--length
                        in  aehnlichF c2 c3 -- in let d = 
-----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
-- for calendar
monthTime x = if x == "jan" then 1
            else if x =="feb" then 2 
            else if x =="mar" then 3
            else if x =="apr" then 4 
            else if x == "mai" then 5 
            else if x == "jun" then 6 
            else if x =="jul" then 7
            else if x =="aug" then 8
            else if x == "sep" then 9
            else if x == "oct" then 10
            else if x =="nov" then 11
            else if x =="dec" then 12     
            else 19  
----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
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

aOsZilloskop1 x nF crit rnd= do
  --   putStrLn$ "Enter starting Line:"
     let anfang = ("1")
     putStrLn$ "How many Lines to change?:"
     xX <- getLine
     
   --  database <- readFile "milch.txt"
     --let machAdd f = add [ nF, f]
     anyWalk <- readFile x -- z.b. "mageWalAllPath.txt"--durble.csv
     let outRead = length anyWalk
     putStrLn$ (""++ (show outRead))
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
     putStrLn " the data is (ohne die extra Leerzeile) : "
  --   mapM print(dipfade)
   --  add [nF, (concat dipfade)]
    -- writeFile nF (concat dipfade)
     let muster = ("1")
     let criterium = crit 
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
                      else (val++tag)

          let dataTyp =  (modus)
          return (dataTyp) ) --a also nur zahlen
     putStrLn " the data is (ohne die extra Leerzeile) : "
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
     let max = (maximum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))
     let min = (minimum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))
     let wievieleMx = length(snd (partition (>criterium) (dropWhile (\(val) -> val < criterium ) ( dipfade2))))
     let wostehenMx = criterium `elemIndices` (dropWhile (\(val) -> val < criterium ) ( dipfade2))
     
     let comparePosition = let hochundtiefPunkt = if ""++(show laengeSet) == (criterium) then putStrLn "Found Hoch/Tiefpunkt"
                                                  else putStrLn "\n\n\nkein hoch oder tiefPunkt"
                           in hochundtiefPunkt

    -- mapM print (dipfade2)
     putStrLn ((show laengeSet) ++ "an WELCHER Stelle des Sets steht criterium")
     putStrLn ((show max) ++" max Wert des Sets" )
     putStrLn ((show min) ++ " min Wert des Sets")
     putStrLn ((show wievieleMx)++ "wieviele Critria")
     putStrLn ((show wostehenMx)++ " wo stehen die Critria\n\n")
 
    -- let mowork = filter 
     comparePosition 
-- Funktion wandelt "0.24" format in "24" um 
-- damit str -> int erfolgen kann 
-- benoetigt fuer berechnung von Wahrscheinlichkeiten 
     let lister2  = let a=  max
                    in let b = (read a)
                    in let a2 = (read min)
                    in let c = ((b)-0.01)
                    in let d = [(b),(c)..(a2)]
                  --  in let e = drop 4 (take 5 d)
                    in let f = map round (map (*100) d)
                    in f
   
       
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
       --   putStrLn (show rosi) 


          let dataTyp3 =  (auswahl)
          return (dataTyp3) ) 
         -- return (spuren) ) 
    
    -- putStrLn (show(dipfade2))
     putStrLn (show more) 
     putStrLn (show (lister2)++" Ist das Spektrum zwischen max und min in 0.1 Schritten\n\n")
     let zui = map length (group (concat (sort (concat dipfade4))))
     let zui11 = (map head (concat dipfade4))
    
     let zui1 = map length (group (concat (concat dipfade4)))-- die val in Zeitgruppen eines Intervalls  
     let zuu2 =  sort (nub ((concat((concat dipfade4)))))
     let zuu22 d =  map round (map (*100) d)
     let zuu3 =  (lister2 \\ (zuu22 (map read zuu2)) )
--     putStrLn ((show dipfade4)++ "    Daten aus dipfade4 fuer Wahrscheinlichkeits-Rechnung benoetigt\n")
--     putStrLn ((show zui1)++ "  Das Vorkommen der Zahlengruppen in ZEITLICHER Abfolge")
--     putStrLn ((show zui11)++"  Die Werte der Zahlengruppen s.o.") 
     putStrLn ((show zui)++ "  Ist das Vorkommen der Zahlen im Spektrum von min nach Max")
     putStrLn ((show zuu2)++"  Die Zahlen die Vorkmmen in Verbindung mit s.o.") 
     putStrLn ((show zuu3)++"  Die Zahlen des Spektrums die nicht vrommen  x 100"   ) 
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

------------------------------------------------------------
--Funktionen Sammlung ueber Wahrscheinlichkeitsrechnung
-- und Zufallsgenrator 
-- a) vals -> Wahrscheinlickeit
-- b) sum (val,year,month,day) -> Zufallsgenerator
--    erzeugt individuelle id ?? jedes tages und vals
--    jedenfalls in dem ersten set
-- ---------------------------------------------------------
-- Formel zur bBerechnung der proz Wahrscheinlichkeit
-- laesst Raum fuer offene Proznte, d,h, die liste der 
-- Normwahrscheinlichkeit wird immer unter 100% bleiben 
-- problem kleine Wahrscheinlichkeiten unter 1% werden 
-- nichtmehr erfasst; 
-- -> begrenzt den Zufalls generator , schraenkt die bandbreite
-- d.h das Spektrum der simulierten vals ein 
-- eine loesung: der io string wird  multipliziert
 --             t:  laenge Set; c : die Nuller setS doppelte laenge des Spektrums 
 --             z:
     let form t c z = (100/(t+(2*c)))* z

-- Formel fuer NORMALE Wahrscheinlichkeit
-- wird getested um Verhalten im Memory2 in hinblick auf 
-- bessere simulierte vals
-- -> gibt volles Spektrum der simul val an volle Bandbreite

     let form2 t c z = 100/(t* (c/c)*(1/z))
-- t :laengeset; c: 1 wird nur eingesetz um form s.o zu ersetzen
--           z: vorkommen des Wertes   
     let formN t c z = 100/(t* (c/c)*(1/z))

-- momentan form2 : grossesSPektrum   
     let formelNormWahr dr = let a1 = let mitSpielraum = (length lister2)
                                      in let dieZeut =  ( sum zui)
                                      in show dieZeut 
                             in let a2 = show (length lister2)
                             in let b1 i ii iii = show (form2 i ii iii)
                        --  in let b2 = b1 (a1 a2 (head a3)) 
                          
                             in b1 (read a1) (read a2) dr --[[a1],[a2],a3] 
   

     
     hans <- getLine
-------------------------------------------------------------- 
-- LINK ZWISCHEN Input UND SIMULIERTEN VALS
   -- formel entscheidet WELCHE Lister weite prozessiert wird
   -- monetan: zui
    -- mappt die Vorkommens liste im Spektrum mit Prozentformel
     let gogos = let a =  map show zui 
                 in let b =  map formelNormWahr (map read a)
                 in  b 
--------------------------------------------------------------

--------------------------------------------------------------
-- Formel Paar das die Wahrscheinlichkeit der geg. val
-- in zurWahr auswaehlt, damit jeder einzelne Wert der
-- Wahrschliste (gogos) s.o. einzeln in die Fert liste 
-- eingesetzt werden kann 
     let heet = length( gogos)  
--   
     let zurWahr de =  let a0 de = head de
                       in let a1 de = de \\ [(a0 de)]
                       in let a2  = iterate a1 de 
                       in (take heet a2)

     --putStrLn $ (show " Enter for Random)"
---------------------------------------------------------------

---------------------------------------------------------------
--Monade  RAUSCHGENERATORS/ZUFALLSGENERATORS
-- in diade werden die vals in Zufallsgen eingesetzt um
-- um fuer (val,zeit)-Paare ein indiviuelle id zu schaffen  
     let gogo2 = gogos
     let bobbo44 =  ([(read anfang)..(read xX)])
     diade <- forM (bobbo44) (\ a -> do
          let auswahl      = let an =takerlein a (lines anyWalk)
                             in an
          let val = value auswahl
          let process = (concat auswahl) \\ show val
 
          let findYear =  let an =  (cvsZeilenBrea2 (process))
                          in map chr an 
          let year  = jahr findYear 
          let finddate =  let an =  (cvsZeilenBrea3 (process))
                          in map chr an
          let monat = month finddate
          let tag = day finddate
                   
          let mitZufall t x c = zufallsGen4 t x c  -- :520
--bestimmt einen individuellen Int Wert fuer zeilen ist aber pro Set nur 
--einmal anwendbar da sich ausgabe nie aendert
          let signatur = let a = 100* (read val)
                         in truncate (sum [a , (read tag ) , (read year),(monthTime monat)])------------------------------------------INPUT UM ZUFALLSGENERATOR ZU MANPULIEREN 
--bestimmt die Laenge der Liste aus zufalsgen die eingelesen wird
          let makene = (length dipfade2)
          let einsatz =  mitZufall  makene (read rnd) (signatur) 

            
          let dataTyp = (show einsatz)

 --grossesSPektrum mit formel : form2

          let bert = formelNormWahr (read val) 

          let chooser = let a1 ert= drop (a-1)(take a ert)
                        in  let b1 = (show (a1 einsatz))
                        in let c1 =   (a1 bert)
                        in let b11 =  let ans =  (map ord b1)
                                    -- filtert "
                                      in let bns = filter (>35) ans

                                      in let bnf = filter (<91) bns
                                      in map chr bnf
                       -- in if  b11<=c1 then b1
                         --  else (b11)  -- ansatz zum berechnen von TAKER fuer % auswahlfu
                        in (b11)

          let chooser2 = let nutzeAlle = filter (/= a) [1..makene]  -- zeige alle Randoms die chooser nicht brauchte
                        --  in let meinA z =  head (drop (z-1) (take z nutzeAlle)) -- mein neues a 
                       --   in let a1 z ert= drop (a-1)(take (z) ert) -- z=Int gibt (makene-1) viele Randoms von einer Val Zeile
                      --    in let takeNeu z = a1 z bert -- zum mappen
                      --    in let ungenutzte = map takeNeu [1..(makene -1)]  -- liste mit allen bisher ungenutzten
                       --   in let exportTake = drop (a-1) (take a ungenutzte) --  nimm eins davon 
                         -- in let b1 = ( (exportTake)) 
                       --   in let hohlRando r = (a1 r einsatz) -- get Randomfunction
                        --  in let meinReturn r = (map meinA (hohlRando r))
                        --  in let endlich n = a1 n (meinReturn n)
                        --  in let mehr = map endlich (nutzeAlle) 
                        --  in let hohlVal v = (a1 v bert)
 
                    --    in let c1 z =   (a1 z bert)
                       --   in let b11 v =  let ans v =  (map ord (hohlVal v))
                         --             -- filtert "
                           --             in let bns v = filter (>35) $ ans v
  
                             --           in let bnf v = filter (<91) (bns v)
                               --         in map chr (bnf v)
                         -- in if  b11<=c1 then b1
                           --  else (b11)  -- ansatz zum berechnen von TAKER fuer % auswahlfu 
                         in (nutzeAlle ) --mehr) --(nutzeAlle) --(hohlRando 1) ---b11 a)-- (meinA 2) --ungenutzte -- nutzeAlle --(meinA (4)) --nutzeAlle --( ungenutzte)

          return (chooser) ) --a also nur zahlen
     putStrLn ((head  diade)) -- 36,31,62

-- : ende grosse Klammer 6254
    -- putStrLn (show (diade)++"\n")
     let humfrey = let apart = "1" -- (head diade)
                   in let a1  =  (map ord (unlines (diade)))
                   --in let a2 = filter (<5) a1
                   --in let a22 = filter (<57) a2
                   in  (words (map chr a1))
  
     let zulu = humfrey
---  shows example how Rules for Interval can be programmed 
     let fogorch = (map read (concat(concat dipfade4))) 
     let gorch =  similaritYvalue fogorch [11,12,34,44]
  
     let gorchH = zui 


------------------------------------------------------------------------------------
-- BSP fuer String Spektrum umwandlung string -> Int 
-- Berechnet die Werte im PICKER d.h. hier wandert
-- eine Zaehlfunktion durch die liste aller Wahrscheinlichkeiten
-- des geg. Sets 
-- sie soll Intervalle schaffen durch welche sich der PICKER ORIENTIERT

-- mittels pick < (takerlein n prego) ....Anhand der            
     let prego xw= let a00 =   xw
                   in let a1 a00= drop (a00-1) (take a00 (zurWahr  gogos))
                   in let a2 = ((show ((concat (a1 a00)))))
                   in let foPick g = map isNumber g
                   in let b11 =  let ans =  (map ord a2)
                                 -- filter punkt
                                 in let bns = filter (>46) ans
 
                                 in let bnf = filter (<91) bns
                                 in  ( map chr bnf) -- map digitToInt ( map chr bnf)
                             --  in map 
                             --
                  --WWWWWWWWWWWWWIIIIICCCHHHTIIIIIIIIGGGGG   
                   in let b22 = let am = map digitToInt b11 -- schafft Int fuer prozess
                                in let am2 =  am --prozentrechnung
                                in  am2
                   in let b3  = let a2 = (take 2 b22) 
                                 in sum (zipWith (*) [1,1] a2)
                   in let b33  = let a21 = take 2 b22 
                                 in let a3 = sum (zipWith (*) [10,1] a21)
                                 in let bhz = (drop 2 )(take 4 b22)
                                 in  a3

                 --  in let b44 = take 4
                 
                   in let a5 = let a2a = take 4 b22  --Int
                               in let a2aa = drop 3 (take 4 a2)  -- String
                               in let a4 = head (drop 3 a2a) -- Int 
                               in let a4a = head (drop 2 (take 3 b22))
                              -- in let teilerr = [(nullStell a4a a4)] 
                               in if  ((foPick ( a2aa))== [True]) then [b33]
                                  else let zuuu = b33 --a2a--(a4/10)
                                       in (take 1 b22)  --zuuu --a2aa
                                  
                --   in let picker = let a111 = drop (3) (take 4 foPick)
                  --                 in let b = if (a111 == [True]) then [b33]
                    --                          else  [b3] --(take 1 a1)
                      --             in b 
                                            
                  -- in let bchoose = if x == 1 then 
                   in a5 -- (take 7 a2) --picker --(a2,b11,b22,b3,picker)

----------------------------------------------------------------------------
--  ALTERNATIVER ANSATZ Formeln: prego,
-- hier Wird versucht die Umwandlung von zufallsgen
-- in simuliete Zahlen einfacher zu prgrammieren
--
-- ALTERNATIVER ANSATZ ZU PREGO s.o.
-- Function um Formel1Wahrscheinlichkeitsliste zu
-- vervielfaeltigen um Genauigkeit zu erhoehen
-- z.B. prego: ["65.3454...3","24,6775..4"]
-- IO: Line 3566
     let chgeWahr2Prego   = let --a1 d = prego d                              
                              --  recurse2Str x y = (\x y -> [x,y])x y
                              --  recurse1Str x = (\x -> [x]) x
                                a2 = ((show gogos)) --(unlines gogos)
                                filtern = let a11 = (map ord a2) -- [Str] -> [Int]
                                              a22 = filter (/=34) a11 --filter ""
                                          in map chr a22 --(map chr a22)
                            in (filtern) -- a2 --(filtern) --(a1 d)     
     let addder = let zdr = (chgeWahr2Prego)
                     in ( zdr)

------------------------------------------------------------
     let fert f = let a1 = [1..(length gogos)]
                  in let b1  = concat (map f a1) 
                  in let nehmer n  = take n b1
                  in let giver = map nehmer a1 
                  in map sum giver --map sum giver  

     let gert f   = let a1 = [1..(length gogos)]
                    in let b1  = concat (map f a1) 
                    in let nehmer n  = take n b1
                    in let giver = map nehmer a1 
                    in giver --map sum giver 
     let bro n= let a1 = bobbo
                in let nehmer n m = take n m
                in let zufall1 n  = nehmer n (fert prego)
                in let zufall2 n =  nehmer n humfrey
                in let valuees n= nehmer n zuu2
                in let giver1 = last (map zufall1 a1)
                in let giver2 = last (map zufall2 a1) 
               
 
                in let gurt n = let zfg = (length (concat(drop (n-1) (take n giver2)))) 
                                in zfg
                in let picker = map gurt a1                  
                in picker --(giver1,giver2,givVal,chngeStr,gurt n, picker) --map sum giver  
                
--diese Funktion wandelt die zufallsliste aus zufallsgen -> String
--in Int um , xw= Int , wird in gert gemapped 
     let preAI xw g = let im1 = bobbo
                   in let nehmer n m = take n m
                   in let zufall1 n  = nehmer n (fert prego)
                   in let zufall2 n =  nehmer n humfrey
                   in let valuees n= nehmer n zuu2
                   in let giver1 = last (map zufall1 im1)
                   in let giver2 =  last (map zufall2 im1) 
                   in let givVal = last (map valuees im1)
                   in let 
                   in let a00 =   xw
                   in let a1 a00= drop (a00-1) (take a00 (giver2))
                   in let a2 = ((show ((concat (a1 a00)))))
                   in let foPick g = map isNumber g
                   in let b11 =  let ans =  (map ord a2)
                                 in let bns = filter (>46) ans
 
                                 in let bnf = filter (<91) bns
                                 in  ( map chr bnf) -- map digitToInt ( map chr bnf)
                             --  in map 
                             --
                  --WWWWWWWWWWWWWIIIIICCCHHHTIIIIIIIIGGGGG   
                   in let b22 = let am = map digitToInt b11 -- schafft Int fuer prozess
                                in let am2 =  am --prozentrechnung
                                in  am2
                   
                   in b22 -- (take 7 a2) --picker --(a2,b11,
--mapped preAI abhaengig von laenge wahrscheinlichkeits liste zum abzaehlen
     let eindelijk f = let ar fu = (preAI fu 1)
                      in let ar2 fu = length (ar fu)
                      in let arGo = map ar bobbo44
                      in let rech fu = sum(zipWith (*) [10,1] (ar fu))
                      in if ((ar2 f) == (2) ) then rech f
                         else  sum (ar f)
  
     let eindelijkGo = map eindelijk bobbo 
 
     let ghijst n  = let art z = drop (z-1) (take z eindelijkGo)-- int zufallsgen vorkommnis
                     in let art2 z =  drop (z-1) (take z (fert prego)) -- int abzaehlliste
                     in let art22 z =  drop (z) (take (z+1) (fert prego)) -- int abzaehlliste
                     in let art23 z =  drop (z+1) (take (z+2) (fert prego)) -- int abzaehlliste
                     
                     in let art3 z =  drop (z-1) (take z (zuu2))-- val die vorkommen
                     in let bart =   (map art bobbo) 
                     in let bart2 = ( map art2 bobbo)
                     in let bart22 =  (map art22 bobbo)
                     in let bart23 =  (map art23 bobbo)
                     
                                  
                     in let rtzu = sort (concat [((art n)), (fert prego)])--spektrum mit vorkommnis
                     in let fowo g t = (head (g t))
                     in let gthe = let gds = partition (<= (fowo art n)) rtzu
                                   in gds
                     in let gthe2  = length (fst gthe)
                     in let gthe3 = art3 gthe2
                     in ((art n , gthe), (gthe2, gthe3))
{-             
                     in if ((art n) >= (art2 n) && (art n) <= (art (n+1))) then (art3 (2))
                        vorkommnis 
                        else if ((art n) >= (art2 n) && (art n) <= (art2 (n+2)))  then (art3 (3))
                        else if ((art n) <= (art2 n) && (art n) <= (art (n+1))) then (art3 (1)) 
                        else (art3 3)
 -}
     let goghijst = let wer k = (snd(snd (ghijst k)))
                    in (map wer bobbo)
     --let gerri =  (map ghijst bobbo)
     putStrLn ((show zui)++ "  Ist das Vorkommen der Zahlen im Spektrum von min nach Max")
     putStrLn ((show zuu2)++"  Die Zahlen die Vorkmmen in Verbindung mit s.o.") 
    
     let addIA d gz = let theOther = d : (fert gz)
                      in theOther

--How to make an Interval
-- Der erste durchschnittliche Wert vor einem Hochpunkt 
--  "   "         "              " nach   "     "      
--  Wir brauchen einen Gruppen-Paarungs Durchschnitt
--  d.h. jedes Jahr hat einen individuellen zeitliche Gruppen
--  vorkommensliste die Anhand von Zeitintervallpunkten
--                 ___
--      /\     ___/   \
--     /  \___/        \___/
--     _____________________________
--      | |   |   |   ||   |  zeitintervallpunkte
--
--      die ueber n Jahre miteinander verglichen werden
--      daraus ergibt sich zu welcher zeit bestimmte Punkte
--      in einer wahrscheinlichkeit vorkommen koennten
-- wir brauchen einen mehrjaehrigen Ueberblick. D.H die einzugebende Liste muss 
-- am besten so viele jahre wie moeglich beinhalten. Diese werden statistisch
-- mit den zeitintervallpunkten  berechnet. 
    -- let jahresAnlys = let zu3 = concat goghijst
      --                 in let zu32 = length zu3
        --               in let zu33 = take 20 zu3
          --             in let zu34 = zu3 \\ zu33 
            --           in  (reverse (sort zu33))
  -- aufbauen
  -- d aud die more funktion wird ein stringlaenge test auf die einezelnen
  -- listenwerte gegeben
--     let moreTag = let are = more--die tageszahl ist dem string des datenwertes begefuegt
  --                 in let are2 n m = take n m
 --
   --                in let are3 = map are2 more
     --              in let  messen = 
       --            in if   
{-                 
     let pregoM =let a1 v =  map prego v
                 in let b11 =  let ans =  (map ord gogo)
                               in let bns = filter (>46) ans
   
                               in let bnf = filter (<91) bns
                               in  ( map chr bnf) -- map digitToInt ( map chr bnf)
                             --  in map 
                             --
                  --WWWWWWWWWWWWWIIIIICCCHHHTIIIIIIIIGGGGG   
                 in let b22 = let am = map digitToInt b11 -- schafft Int fuer prozess
                              in let am2 =  am --prozentrechnung
                              in  am2
                 in let b3  = let a2 = (take 1 b22)
                              in  a2
                 in let b33  = let a2 = (take 2 b22) 
                               in sum (zipWith (*) [10,1] a2)
                 in let picker = let a111 = drop (1) (take 2 foPick)
                                 in let b = if (a111 == [False]) then [b33]
                                            else   b3 --(take 1 a1)
                                 in b
  -}  

   
                
    -- putStrLn (show normWahrsch)
    -- putStrLn (show normWahrsch1)
     putStrLn (( formelNormWahr (read hans))++"% Es gibt auch eine Chance auf nicht verwirklichte Werte")
     putStrLn (show ( gogos)++"% Dies ist die Liste mit Formel 1 wahrscheinlichkeiten ; function: gogos")
    -- putStrLn (show ( heet)++"% gesamt  wahrscheinlichkeiten ")
     putStrLn (show ( zurWahr gogos )++"zurwahrliste schafft PICKER fuer % ; function: zurWahr gogos ")
    -- putStrLn (show (gorch )++"Ahenlichkeit wahrscheinlichkeit ")
     --putStrLn (show (gorchH )++" ")
     putStrLn (show ( humfrey )++"BSp bereinigtes Format string aus monade; function: humfrey ")
     putStrLn (show (prego  1)++ " BSP Prozentrechener als Int leider nur fuer ganze zahlen; function: prego 1 " )
     putStrLn (show (fert prego  )++ " Die Additive Liste zum Abzaehlen und einfuegen von Wahrscheinlichkeitsaenderung\n"++ "; function: fert prego"  )
    -- putStrLn (show (bro 1 ) ++ " vergleich von Zaehler und Zufallsgenerator")
    -- putStrLn (show (preAI 2 2 ) ++ " vergleich fuer Zufallsgenerator einzelne zahl")
    -- putStrLn (show (eindelijkGo ) ++ " vergleich fuer Zufallsgenerator liste gogos")
     putStrLn (show (addIA 1729 prego ) ++ " fuegt prozent der additiven liste zu; function: addIA 1729 prego")
     putStrLn (show (ghijst 1 ) ++ " test fuer map additive Liste mit zufall ")
    -- putStrLn (show (goghijst  ) ++ " s.o. Liste " )
    -----------------------------------------------------------------------------------------------------------------------------------------------SCHNITTSTELLE: WX AND MQ 3  6
       ----------------------------------------------------------------------------------------------------------------------------------------
    ----------------------------------------------------------------------------------------------------------------------------------------------
    ----------------------------------------------------------------------------------------------------------------------------------------------------MQ_Section 
     --x nF 
     -- "Enter starting Line:"
     --anfang  
     -- "How many Lines to change?:"
    -- xX <- getLine
     
   --  database <- readFile "milch.txt"
     --let machAdd f = add [ nF, f]
   --  anyWalk <- readFile x -- z.b. "mageWalAllPath.txt"--durble.csv

   --  machAdd dataTyp 
     --let bilderno = length anyWalk  
   --  let bobbo =  ([(read anfang)..(read xX)])
    -- putStrLn "enter what to edit val:1 ; year:2 ; month:3 ; tag:4 einfach Int eingeben"
     --muster <- getLine
     --putStrLn " enter criterium: that is a value (nur Int eingeben)"
     --criterium <- getLine
 

     let mixWithMQ3 --x nF crit --openB file to open ; -openA file to write ; forD criterium
                                               ------------------------------------------------------------------------------------OSZILLOSKOP SHOS INPUT AND RANDOM
                                              =  let ay1= aOsZilloskop1 x nF criterium (read rnd) -- nF crit
                                                 in let ay2 p = fourier1 p + fourier2 p + fourier3 p
                                                 in let ay3 =  (6.28319901) -- NullStelle Des Intervals
                                                 in let ay4 = (ay3/ (read xX)) -- takes forC howmany lines  
                                            	     in let soMany2take fak = ay4 * fak -- determines which value of fourier123 2 select
                                                 in let forThis fak = fourier123 (soMany2take fak)
                                                 in let fofoThis = map forThis [1..(read xX)]
                                                 in fofoThis --intoConsider

     let mixWithMQ4 --x nF crit --openB file to open ; -openA file to write ; forD criterium
                                               ------------------------------------------------------------------------------------OSZILLOSKOP SHOS INPUT AND RANDOM
                                              =  let ay1= aOsZilloskop1 x nF criterium (read rnd) -- nF crit
                                                 in let ay2 p = fourier1MQ4 p + fourier2MQ4 p + fourier3MQ4 p + fourier4MQ4 p 
                                                 in let ay3 =  (6.28319901) -- NullStelle Des Intervals
                                                 in let ay4 = (ay3/ (read xX)) -- takes forC howmany lines  
                                                 in let soMany2take fak = ay4 * fak -- determines which value of fourier123 2 select
                                                 in let forThis fak = fourier123 (soMany2take fak)
                                                 in let fofoThis = map forThis [1..(read xX)]
                                                 in fofoThis --intoConsider


     let mixWithMQ5 --x nF crit --openB file to open ; -openA file to write ; forD criterium
                                               ------------------------------------------------------------------------------------OSZILLOSKOP SHOS INPUT AND RANDOM
                                              =  let ay1= aOsZilloskop1 x nF criterium (read rnd) -- nF crit
                                                 in let ay2 p = fourier1MQ5 p + fourier2MQ5 p + fourier3MQ5 p + fourier4MQ5 p + fourier5MQ5 p
                                                 in let ay3 =  (2.35011) -- NullStelle Des Intervals
                                                 in let ay4 = (ay3/ (read xX)) -- takes forC howmany lines  
                                                 in let soMany2take fak = ay4 * fak -- determines which value of fourier123 2 select
                                                 in let forThis fak = fourierMQ5PAN123 (soMany2take fak)
                                                 in let fofoThis = map forThis [1..(read xX)]
                                                 in fofoThis --intoConsider

     let mixWithMQ6 --x nF crit --openB file to open ; -openA file to write ; forD criterium
                                               ------------------------------------------------------------------------------------OSZILLOSKOP SHOS INPUT AND RANDOM
                                              =  let ay1= aOsZilloskop1 x nF criterium (read rnd) -- nF crit
                                                 in let ay2 p = fourier1MQ6 p + fourier2MQ6 p + fourier3MQ6 p + fourier4MQ6 p + fourier5MQ6 p + fourier6MQ6 p 
                                                 in let ay3 =  (6.28319901) -- NullStelle Des Intervals
                                                 in let ay4 = (ay3/ (read xX)) -- takes forC howmany lines  
                                                 in let soMany2take fak = ay4 * fak -- determines which value of fourier123 2 select
                                                 in let forThis fak = fourierMQ6NOPAN123 (soMany2take fak)
                                                 in let fofoThis = map forThis [1..(read xX)]
                                                 in fofoThis --intoConsider


     putStrLn$ ("The NEW STUFF 18 .10 .17"++ (show mixWithMQ3))
     let intoConsider =   let inpU1 = (show mixWithMQ3)
                          in let inpU2 =  (concat goghijst) -- quelle simulierte vals
                          in let inpU3 = (map show bobbo) -- laenge val liste 
                          in let inpU4 = concat (concat dipfade4) -- real Input VALS
                          in let inpU5 =  (zipWith (+) ( mixWithMQ3) (map read humfrey))  -- fourier123 and simulated vals
                          in let inpU6 =  (zipWith (+) ( mixWithMQ3) (map read inpU4))  -- fourier123 and REAL input vals
                          in let inpU7 =  (zipWith (+) ( mixWithMQ5) (map read humfrey)) 
                          in let inpU8 =  (zipWith (+) ( mixWithMQ5) (map read inpU4)) 
                          in let inpU9 =  (zipWith (+) ( mixWithMQ5) (mixWithMQ3)) 
                          in let in1MQ6 =  (zipWith (+) ( mixWithMQ6) (map read humfrey)) 
                          in let in2MQ6 =  (zipWith (+) ( mixWithMQ6) (map read inpU4)) 
                          in let inMQ6MQ3 =  (zipWith (+) ( mixWithMQ6) ( mixWithMQ3)) 
                          in let inMQ5MQ6 =  (zipWith (+) ( mixWithMQ5) (mixWithMQ6)) 
                          in let inMQ6MQ5MQ3 = (zipWith (+) mixWithMQ6 (zipWith (+) ( mixWithMQ5) (mixWithMQ3)))
                          in let in1MQ4 = (zipWith (+) (mixWithMQ4) (map read humfrey)) 
                          in let in2MQ4 =  (zipWith (+) (mixWithMQ4) (map read inpU4)) 
                                                                                                      
                          in [inpU5,inpU6,in1MQ4,in2MQ4,inpU7,inpU8,inpU9,in1MQ6,in2MQ6,inMQ6MQ3,inMQ5MQ6,inMQ6MQ5MQ3,(map read inpU2),(map read inpU4)] --(filtern inpU1 (show (concat inpU4)))
                        --in map step1 humfrey
     putStrLn$ ("The NEW STUFF 18 .10 .17"++ (show intoConsider))

  ------------------------------------------------------------------------------------------------------------------
  -- The Connection  between goghijst and WxMAxime via intoConsider
     --Enter Spuren
     putStrLn$ ("Enter Wieviele Spurensollen angezeigt werden?")
-- Das Spektrum: alle Vals die Vorkommen von min nach max
-- Output for #Maxima
-- function changes simulated numbers !!!!
-- of goghijst to string in pair form for maxima 
--
-- let more = ((dropWhile (\(val) -> val < criterium ) ( dipfade2)))
--     let laengeSet = length (dropWhile (\(val) -> val < criterium ) ( dipfade2)) --wie lang ist set
--     let max = (maximum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))
--     let min = (minimum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))
--     let wievieleMx = length(snd (partition (>criterium) (dropWhile (\(val) -> val < criterium ) ( dipfade2))))
--     let wostehenMx = criterium `elemIndices` (dropWhile (\(val) -> val < criterium ) ( dipfade2))
     
--     let comparePosition = let hochundtiefPunkt = if ""++(show laengeSet) == (criterium) then putStrLn "Found Hoch/Tiefpunkt"
      --                                            else putStrLn "\n\n\nkein hoch oder tiefPunkt"
  --                         in hochundtiefPunkt

--
--
--
--
     let outPutMaxima1 = let a =  (concat goghijst) -- quelle simulierte vals
                             b = (map show bobbo) -- laenge val liste 
                             c = concat (concat dipfade4)
                             kerry x y = zipWith (\x y -> [x,y]) x y -- a-- (zip a b)
                             fofiltern x y= show (kerry x y)
                             filtern  x y = let tussenstap  = map ord (fofiltern x y)
                                                tss2 = filter (/=34) tussenstap
                                            in map chr (tss2)
                         -- ( simuliiert , Quelle)
                          in  (filtern b a)

     let outPutMaxima2 = let a =  (concat goghijst) -- quelle simulierte vals
                             b = (map show bobbo) -- laenge val liste 
                             c = concat (concat dipfade4)
                             e = (map show zui) --das Vorkommen der Vals von min nach Max
                             f =  (map show wostehenMx)
                             --g = (  
                             kerry x y = zipWith (\x y -> [x,y]) x y -- a-- (zip a b)
                             fofiltern x y= show (kerry x y)
                             filtern  x y = let tussenstap  = map ord (fofiltern x y)
                                                tss2 = filter (/=34) tussenstap
                                            in map chr (tss2)
                         -- ( simuliiert , Quelle)
                          in  (filtern b c)
    -- functions aids to shorten code 
    -- x:Int ; n:[Int]
     let takenN x n = concat (drop  (x-1) ( take x n))
---------------------------------------------------------------------------------------bein used imports MQ Functions 
     let outPutMaxima3 = let a =  (concat goghijst) -- quelle simulierte vals
                             b = (map show bobbo) -- laenge val liste 
                             c = concat (concat dipfade4)
                             e = (map show zui) --das Vorkommen der Vals von min nach Max
                             f =  (map show wostehenMx) 
                             g = (show (fert prego  ) )-- " Die Additive Liste zum Abzaehlen und einfuegen von Wahrscheinlichkeitsaenderung\n"++ "; function: fert prego"
                             h =  (map show (head intoConsider)) -- MQ3 and InputVal Kurve
                             mq3H = (map show ( takenN 2 intoConsider)) -- MQ3 Humfrey
                             j =  (map show ( takenN 3 intoConsider)) --MQ5 inp4U
                             mq5H  = (map show ( takenN 4 intoConsider))-- MQ5 Humfrey
                             l = (map show ( takenN 5 intoConsider))
                             mq6H = (map show ( takenN 6 intoConsider)) -- MQ6 Humfrey
                             n = (map show ( takenN 7 intoConsider))-- MQ6 Inp4U
                             o = (map show ( takenN 8 intoConsider))
                             p= (map show ( takenN 9 intoConsider)) 
                             q = (map show ( takenN 10 intoConsider))
                             r= (map show ( takenN 11 intoConsider))
                             mq4H = (map show ( takenN 12 intoConsider)) -- MQ4 Humfrey
                             t= (map show ( takenN 13 intoConsider))
                             u = (map show (takenN 14 intoConsider))
                             v = (map show (takenN 11 intoConsider)) 
                             kerry x y = zipWith (\x y -> [x,y]) x y -- a-- (zip a b)
                             fofiltern x y= show (kerry x y)
                             filtern  x y = let tussenstap  = map ord (fofiltern x y)
                                                tss2 = filter (/=34) tussenstap
                                            in map chr (tss2)
                          in [(filtern b h),(filtern b mq3H),(filtern b j),(filtern b mq5H),(filtern b l),(filtern b mq6H),(filtern b n),(filtern b o),(filtern b p),(filtern b q),(filtern b r),(filtern b mq4H),(filtern b t),(filtern b c),(filtern b v ) ]
{-
     let outPutMaxima4 = let a =  (concat goghijst) -- quelle simulierte vals
                             b = (map show bobbo) -- laenge val liste 
                             c = concat (concat dipfade4)
                             e = (map show zui) --das Vorkommen der Vals von min nach Max
                             f =  (map show wostehenMx) 
                             g = (show (fert prego  ) )-- " Die Additive Liste zum Abzaehlen und einfuegen von Wahrscheinlichkeitsaenderung\n"++ "; function: fert prego"
                             h =  (map show (head intoConsider)) -- MQ3 and InputVal Kurve
                             mq3H = (map show ( takenN 2 intoConsider)) -- MQ3 Humfrey
                             j =  (map show ( takenN 3 intoConsider)) --MQ5 inp4U
                             mq5H  = (map show ( takenN 4 intoConsider))-- MQ5 Humfrey
                             l = (map show ( takenN 5 intoConsider))
                             mq6H = (map show ( takenN 6 intoConsider)) -- MQ6 Humfrey
                             n = (map show ( takenN 7 intoConsider))-- MQ6 Inp4U
                             o = (map show ( takenN 8 intoConsider))
                             p= (map show ( takenN 9 intoConsider)) 
                             q = (map show ( takenN 10 intoConsider))
                             r= (map show ( takenN 11 intoConsider))
                             mq4H = (map show ( takenN 12 intoConsider)) -- MQ4 Humfrey
                             t= (map show ( takenN 13 intoConsider))
                             u = (map show (takenN 14 intoConsider))
                             v = (map show (takenN 11 intoConsider)) 
                         in let  tunKate =  map read 
                           --  kerry x y = zipWith (\x y -> [x,y]) x y -- a-- (zip a b)
               
                           --  fofiltern x y= show (kerry x y)
                           --  filtern  x y = let tussenstap  = map ord (fofiltern x y)
                           --                     tss2 = filter (/=34) tussenstap
                            --                in map chr (tss2)
                          in [(filtern b h),(filtern b mq3H),(filtern b j),(filtern b mq5H),(filtern b l),(filtern b mq6H),(filtern b n),(filtern b o),(filtern b p),(filtern b q),(filtern b r),(filtern b mq4H),(filtern b t),(filtern b c),(filtern b v ) ]

-}
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






 
    -- writeFile ("fomxima2.txt") ((fst outPutMaxima)++"\n\n"++(snd outPutMaxima))
     let moant = ("1")
     let foWxlist1 x y = show ((read x) + y)
     let foWxlist2 x y = show ((read x) + y)

-- SPUREN DES OsZilloskops
--   
     let writerOszillos n = 
           let foTake xyz = (takenN xyz outPutMaxima3)
  --   let write1erWXFile = (aCompleteWX [((outPutMaxima1) )] [[1,2,3],[4,5,6]] xX "0.0" "10")
   --  let write2erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2))] [[1,2],[4,5]] xX "0.0" "10")
           in let write3erWXFile = (aCompleteWX [((takenN 14 outPutMaxima3) ),((takenN 3 outPutMaxima3)),((takenN 7 outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10") --
-- ########  Real Inputval ######### MQ5 inp4U ######## Mq3+input #### 
           in let write4erWXFile = (aCompleteWX [((takenN 1 outPutMaxima3) ),((takenN 3 outPutMaxima3)),((takenN 7 outPutMaxima3)),((takenN 14 outPutMaxima3) )] [[1,2,3,4],[5,6,7,8]] xX "0.0" "10")
           in let write5erWXFile = (aCompleteWX [((takenN 14 outPutMaxima3) ),((takenN 1 outPutMaxima3)),((takenN 3 outPutMaxima3)),((takenN 7 outPutMaxima3)),( ( takenN 15 outPutMaxima3))] [[1,2,3,4,5],[6,7,8,9,10]] xX "0.0" "10")
           in let write7erWXFile = (aCompleteWX [((takenN 14 outPutMaxima3) ),((takenN 1 outPutMaxima3)),((takenN 3 outPutMaxima3)),  ((takenN 7 outPutMaxima3) ),((takenN 5 outPutMaxima3) ), ((takenN 6 outPutMaxima3) ), ((takenN 7 outPutMaxima3) )] [[1,2,3,4,5,6,7],[8,9,10,11,12,13,14]] xX "0.0" "10")
           in let write7bWXFile = (aCompleteWX [((foTake 1) ),((foTake 2)),((foTake 4)),  ((foTake 6) ),((foTake 12) ), ((foTake 14) ), ((takenN 7 outPutMaxima3) )] [[1,2,3,4,5,6],[7,8,9,10,11,12]] xX "0.0" "10")
--     let write7erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
  --   let write8erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
--     let write9erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
    -- let write10erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
  --   let write11erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
--     let write12erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
    -- let write13erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
  --   let write14erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
--     let write15erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
    -- let write16erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
  --   let write17erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
--     let write18erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
    --- let write19erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
  --   let write20erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
--     let write21erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
     --let write22erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
    -- let write23erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
  --   let write24erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
--     let write25erWXFile = (aCompleteWX [((outPutMaxima1) ),(( outPutMaxima2)),((outPutMaxima3))] [[1,2,3],[4,5,6]] xX "0.0" "10")
             in let allWX n = takenN n [write4erWXFile,write7erWXFile]
             in allWX n
     --writerOszillos
--[write1erWXFile,write2erWXFile,write3erWXFile,write4erWXFile,write5erWXFile,write6erWXFile,write7erWXFile,write8erWXFile,write9erWXFile,write10erWXFile,write11erWXFile,write11erWXFile,write12erWXFile,writer13erWXFile,write14erWXFile,write15erWXFile,write16erWXFile,write17erWXFile,write18erWXFile,write19erWXFile,write20erWXFile,write21erWXFile,write22erWXFile,write23erWXFile,write24erWXFile,write25erWXFile]

 
  --   let writeWXFile2 = (aCompleteWX [(concat jahresWxTrend)] [[1,2,3],[4,5,6]] "5" "0.0" "30.0")
-- inserts useless double list in IO for display
    -- let foAutoWrite = [[1,2,3],[4,5,6]]
    -- putStrLn ((show( snbs "3" moant)) ++ "snbs: Stellen min Values jahr3 ") 
    -- putStrLn (show(triggerHappy moant) ++ "triggerHappy:  Stellen 'Min Values', Jahr "++intfoPlug++", monat: "++moant) 
    -- putStrLn (( outPutMaxima moant)++ "outPutMaxima: Check check\n\n")
     putStrLn (show more) 
     putStrLn (show (lister2)++" Ist das Spektrum zwischen max und min in 0.1 Schritten\n\n")
    -- let zui = map length (group (concat (sort (concat dipfade4))))
    -- let zui11 = (map head (concat dipfade4))
    
   --  let zui1 = map length (group (concat (concat dipfade4)))-- die val in Zeitgruppen eines Intervalls  
    -- let zuu2 =  sort (nub ((concat((concat dipfade4)))))
    -- let zuu22 d =  map round (map (*100) d)
     --let zuu3 =  (lister2 \\ (zuu22 (map read zuu2)) )
--     putStrLn ((show dipfade4)++ "    Daten aus dipfade4 fuer Wahrscheinlichkeits-Rechnung benoetigt\n")
--     putStrLn ((show zui1)++ "  Das Vorkommen der Zahlengruppen in ZEITLICHER Abfolge")
--     putStrLn ((show zui11)++"  Die Werte der Zahlengruppen s.o.") 
     putStrLn ((show zui)++ "  Ist das Vorkommen der Zahlen im Spektrum von min nach Max")
     putStrLn ((show zuu2)++"  Die Zahlen die Vorkmmen in Verbindung mit s.o.") 
     putStrLn ((show zuu3)++"  Die Zahlen des Spektrums die nicht vrommen  x 100")   
    -- writeFile ("anMaximaFile2.wxm") (write5erWXFile)
     
--C:\Users\Watson\Documents\DynamicSystems\Experimente\FractionalLearning\stream-crypt\test
          --(buildDisplay  5)

  -- makes squares and passes values
            
--     putStrLn$ ((show autoW2)++"")
   --  let autoW3 = [1..(autoW2)] 
    -- putStrLn$ (write7erWXFile++"\n\nwrote maximaFile: anMaximaFile.wxm" )


    -- fowMaximQ <- readFile "fomxima.txt"
    --
    -- :2369
     putStrLn ("Enter line for Prego function") 
     --fopregos <- getLine
     putStrLn (show(chgeWahr2Prego))
     putStrLn ( addder) 
   --  writeFile ("anMaximaFile2.wxm") (write4erWXFile)
   --  putStrLn (show  (diade)) 
     putStrLn "end" --(show (fst outPutMaxima ) ++ "simulated InputVal ")
    -- putStrLn (show (snd outPutMaxima  ) ++ " Real InputVal wrote file : fomaximA.txt ")



-----------grosse Klammer ende
--------------------------------------------------------------------------
--
--
---------------------------------------------------------------------------------------------------------------------------
--- YEMODA LINK !!!!!!!!!!!!!!!!!!!!!!!!!
-- linke for exporting simu val into patternfile from YeMoDa
-- s : "String" e.g "1"  --("Write/Add to: Vals==1,  Patterfile==2 or Both 3" 

     let eYeMoDa s = yeMoDa s bobbo anyWalk ghijst nF 
     let eYeMoDa2 s ghijsta = yeMoDa s bobbo anyWalk ghijsta nF 


     -- funktion verbindet : die monaliche Preisentwicklung: 
-- snbs "2" monat 
-- triggerHappy moant  , mappt moonat moant n vile jahre 
-- mit : den originalenWerten Dipfade2 und 
   --  let makeWEAVEkaAnsatz1 =  
 
---------------------------------------------------------
     let guiReturnfunction back1step backAllsteps= 
                     let ans x1 = if x1=="1" 
                                  then do 
                                    back1step
                                  else
                                    backAllsteps
                     in do  -- with do ghi sees line1 ...guiReturn, line2 ..input1... 
                        guiReturnFunctiontext
                        inputgui1 <- getLine
                        ans inputgui1

     ------------------------------------------------------------------------
     let statisticalWahrsch = do   -- Stistis analys Wahrscheinlichkeiten  IO :2434
                  statisticalWarschtext1
                  statiswa1 <- getLine 
                  let chartWa1 = if statiswa1=="1" -------- add/write simulated Random Werte
                                 then do
                                   avanti [("Write or ADD; 1==write")] 
                                   wriAdd0 <- getLine
 
                                   avanti [("Write/Add to: Vals==1,  Patterfile==2 or Both 3")]
                                   foyeMo <- getLine

                                   let accesFuncTXT2 =  goghijst -- or (takenN n intoConsider)

                                   let deCid e = if foyeMo=="1" -------------decide if to write Vals,PAttern, or both files types
                                                 then do 
                                                  (writeFile "exportDATA.txt" (show accesFuncTXT2)) -- normal vals
                                                 else if foyeMo=="2"
                                                 then do
                                                  (eYeMoDa e) --write/add pattern
                                                 else do -- normal val and  pattern 
                                                  (eYeMoDa e)
                                                  (writeFile "exportDATA.txt" (show accesFuncTXT2))
                                                 
                                                   
 
  
                                   putStrLn ("View the first 10 Randoms")    
                                   putStrLn (show (take 10 accesFuncTXT2))
                                  -- run yearMontDay to get ready to use  

                                   let writeOrAdd0 = if wriAdd0== "1" 
                                                     then do
                                                     putStrLn " Wrote exportDATA.txt the simulated Vals preset to have a 10% Abweichung Vals:" 
                                                     putStrLn          "Three mQ functions and the the real vals "
                                                     (deCid "1") -- decide which and write em 
                                                   
                                                               --  else if writAdd1=="1" && outFill == "2" then (writeFile "aaKA/Osilloskop.wxm" (unlines accesFuncWX))
                                                     else do add (["exportDATA.txt", (show accesFuncTXT2) ])  
                                                             (deCid "2")     --     add ([ "sot.txt", ((lines  yeMoDa)) ]) -- export "SetoStyle"

                                  -- let qwik = (ghijst 3)      
                                   writeOrAdd0
                                                                                
                                   putStrLn ("A a new Random Run has been generated ! Name:"++(nF)++ " Length : " ++ (xX)) -- set to only one random Run, same length as input file
                                   
                                 else if statiswa1=="2" ----------------------------------------------------------------------------------------MONATS AUSWAHL
                                               then do 
                                                 statisicalAnalysistext21 --welche zeile
                                                 input91 <- getLine
                                                 statisicalAnalysistext22  -- which year
                                                 input92 <- getLine
                                                              --  putStrLn$ (show lister2)
                                                 statisticalWarschtext2
                                                 putStrLn$ ("Which off these Functions , see above, to work with?")  -- SELECT FUNCTION FOR SIMPLE OVERVIEW
                                                 selLECTOR <- getLine
                                                
                                                 putStrLn$ ("Write or ADD (type: 1 for write")
                                                 writeAdd <- getLine
                                             --    simpleFacts2 -- x nF howMany crit -- choose write , add , or just view file in Menu 
                                              --   input93 <- getLine                                              
                                                 putStrLn$ ("Which file to read?")
                                                 openB <- getLine
                                                            --   forB <- readFile openB
                                                              --  let bebe = (show forB)
                                                 putStrLn$ ("Which file to write?")
                                                 openA <- getLine
                                                 let forA = (openA) --("afiction.txt")
                                                 putStrLn$ ("How many lines to read?")
                                                 forC <- getLine
                                                 let ceCE = ( forC)
                                                 putStrLn$ ("Which Criterium are you looking for?")
                                                 forD <- getLine
                                                 let deDE = (show forD)
                                                 let geGE = ( aCrunchList1 (openB) forA ceCE deDE) -- eigenstaendige Funktion liest openB ein
                                                                  -- Intro to see Hochtief punkte
                                                 putStrLn$ (show forD)
                                                 putStrLn$ (show forA)
                                                 putStrLn$ (show ceCE)
                                                 putStrLn $ ("Enter month for snbs function!")
                                                 foSnbs <- getLine

 --TAKES IN IO -> --zeile:input91, year:input92, write/Add:input93, file2read:openB, file2write:openA, criterium:forD->deDE, Month:foSnbs
                                           {-      let aWriterOne  = funcChooser foSnbs   
                                                        where
                                                          selcts = selLECTOR;
                                                          a1a =  (steVeapproach whichFunction input91 foSnbs )
                                                          a2a = (monatllist (read whichFunction) (read input91) (read foSnbs))
                                                        --  a3a = fst(concat  (filterYears ( input91)));
                                                          a5a  = (snbs (read input91) (read input92)); -- snbs monate jahr 
                                                          a6a = (setein whichFunction ( foSnbs) ( input92) );
                                                          funcChooser what = (selecFUNC selcts a1a a2a a2a (a5a) a6a)
                                           -}
-- ------------------------------------occurrance7
                                                 let aWriterOne  = lines ( funcChooser )-- foSnbs  ------------------------------------------Snd Function List of MONATSAUSWAHL
                                                        where
                                                          selcts = selLECTOR;
                                                        --  a0a =  (steVeapproach whichFunction input91 foSnbs )

                                                       --   a1a =   (monatllist (read whichFunction) (read input91) (read foSnbs)); -- --> fstlaengemonat snd(breaker1,breaker2 )
                                                       --  - foawe2a1 g =  (fst (a1a));
                                                       --   foawe2a2 g =  [(fst(snd a1a))];
                                                       --   foawe2a3 g = [(snd(snd a1a))];  
                                                       --   foawe2a4 g = (((concat [(foawe2a1 g),(foawe2a2 g),(foawe2a3 g)])) );
                                                       --   foawe2Min g = (head(drop 1 ( take 2 (foawe2a4 g))));
                                                       --   foawe2Max g = (last (foawe2a4 g))
                                                        --  selectv g = (verglMonate (read input92 ) (foawe2Min g) (foawe2Max g)) --, year ,  breaker1 , breaker2 (e.g daten des jahres 1 braucht year = 2...) 

                                                        --  a2a g = ((show (selectv g)) ) -- 

                                                        --  a2a =  (verglMonate (read intfoPlug) (foawe2a1 a1a) (foawe2a2 a1a));  
                                                          a3a  = ( ( gogos)); -- % Dies ist die Liste mit Formel 1 wahrscheinlichkeiten ")
                                                          a6a =( ( heet)); --"% gesamt  wahrscheinlichkeiten ")
                                                          a4a  =( ( zurWahr gogos) );  --"zurwahrliste schafft PICKER fuer % ")
                                                          a5a  =( (gorch )); -- "Ahenlichkeit wahrscheinlichkeit ")
                                                          a7a  = ( (gorchH ));
                                                          a8a =  ( ( humfrey )); --"BSp bereinigtes Format string aus monade ")
                                                          a9a =   ( (prego  1)) ;-- " BSP Prozentrechener als Int leider nur fuer ganze zahlen " )

                                                          a10a  =  ( (bro 1 ) ); -- " vergleich von Zaehler und Zufallsgenerator")

                                                          a11a  = ( (fert prego  )); -- " Die Additive Liste zum Abzaehlen und einfuegen von Wahrscheinlichkeitsaenderung"  )

                                                          a12a = ( (preAI 2 2 )); --  " vergleich fuer Zufallsgenerator einzelne zahl")
                                                          a13a = ( (eindelijkGo ) ); -- " vergleich fuer Zufallsgenerator liste gogos")
                                                          a14a =  ( (addIA 1729 prego )); --  " fuegt prozent der additiven liste zu")
                                                          a15a = ( (ghijst 1 ) ); --   " test fuer map additive Liste mit zufall ")
                                                          a16a = ((goghijst  )); --  " s.o. Liste ")
                                                      --    a17a = val4Pairs (read input91) (read input92) -- line, year  8
                                                      --
                                                           
                                                        --  a18a g = makeInfoPairs2  (read input91)(read input92) (foawe2Min g) (foawe2Max g)
                                                          funcChooser   = (selecFUNC3 selcts a3a a6a (a4a ) a5a (a7a  ) a8a (a9a ) (a10a)(a11a  ) a12a a13a a14a a15a a16a)
           
                                                          
 -- zeigt die Stellen n aus wostehenWerte\n"++ -- funkt nich


                                                 let writeOrAdd = if writeAdd=="1"
                                                                                                                                    
                                                                  then do 
                                                                    (writeFile openA (concat( aWriterOne)))
                                                                  else do 
                                                                    let metA = let ab12 = aWriterOne
                                                                               in let ab23 = (show ab12)
                                                                               in ab23 

                                                                    add ([openA, (( metA))]) -- (writeFile openA (myAdder aWriterOne))
-- add [(openA, aWriterOne)]
                                                 let writeAddIndi = if writeAdd=="1" then putStrLn$ ("WRITE MODE")--indicates if write or add mode
                                                                    else putStrLn$ ("ADD MODE")

                                                 let chart10Mo = if selLECTOR=="1"------------------- WRITE STATISCAL aNALYSIS NEW FILE OR ADD 
                                                               then do
                                                                  putStrLn$ ("Dies is die Lsite mit dn Formel 1 Wahrscheinlichkeiten :gogos "++ input92)
                                                                  putStrLn$ (show ( gogos))
                                                                 -- avanti [(("\n"++ (print writeAddIndi))++" "++openB++"\n"++
                                                                   --        ", all lines year: "++ input92++"\n" ) ]
                                                                  writeOrAdd
                                                                  --chart8
                                                                else if selLECTOR=="2"
                                                                then do 
                                                                  avanti [("% gesamt  wahrscheinlichkeiten: heet")]
                                                                  putStrLn$(show ( heet))                                                                 -- avanti [("\n"++writeAddIndi++" Mapcriti fst, line"++input91++" year: "++ input92)]
                                                                  writeOrAdd
                                                                 -- chartWa1 
                                                                else if selLECTOR=="3"
                                                                then do 
                                                                  avanti [("zurwahrliste schafft PICKER fuer \n"++
                                                                           " aus , line"++input91++" year: "++ input92)]
                                                                  putStrLn$ (show ( zurWahr gogos) )                                                              -- avanti [("\n"++writeAddIndi++" Wrote File\n"++ openB ++ "\n"++
                                                                 --          " aus , line"++input91++" year: "++ input92)]
                                                                  writeOrAdd
                                                               --   chartWa1  
                                                                else if selLECTOR=="4"
                                                                then do
                                                                  avanti [("Ahenlichkeit wahrscheinlichkeit \n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 

                                                                  putStrLn$ (show (gorch ))
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                               --   chartWa1  
                                                                else if selLECTOR=="5"
                                                                then do
                                                                  avanti [(" 'machWahr3' :1813\n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                                  putStrLn$(show (gorchH ))
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                               --   chartWa1  
                                                                else if selLECTOR=="6"
                                                                then do
                                                                  avanti [("BSp bereinigtes Format string aus monade SimultedVals: \n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                                  putStrLn$ (show ( humfrey ))
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                               --   chartWa1  
                                                                else if selLECTOR=="7"
                                                                then do
                                                                  avanti [(" BSP Prozentrechener als Int leider nur fuer ganze zahlen\n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                                  putStrLn$  (show (prego  1)) 
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                               --   chartWa1  
                                                                else if selLECTOR=="8"
                                                                then do
                                                                  avanti [(" vergleich von Zaehler und Zufallsgenerator: bro 1\n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                                  putStrLn$ (show (bro 1 ) )
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                               --   chartWa1  
                                                                 else if selLECTOR=="9"
                                                                then do
                                                                  avanti [("  vergleich fuer Zufallsgenerator liste gogos : fert prego\n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                                  putStrLn$ (show (fert prego  ))
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                               --   chartWa1  
                                                                else if selLECTOR=="10"
                                                                then do
                                                                  avanti [(" vergleich fuer Zufallsgenerator einzelne zahl : prAI\n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                                  putStrLn$ (show (preAI 2 2 ))
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                                 -- chartWa1 
 
                                                                else if selLECTOR=="11"
                                                                then do
                                                                  avanti [("vergleich fuer Zufallsgenerator liste gogos: eidelijkGo\n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                                  putStrLn$ (show (eindelijkGo ) )
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                               --   chartWa1  
                                                                else if selLECTOR=="12"
                                                                then do
                                                                  avanti [(" fuegt prozent der additiven liste zu\n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                                  putStrLn$ (show (addIA 1729 prego ))
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                               --   chartWa1 

 
                                                                else if selLECTOR=="13"
                                                                then do
                                                                  avanti [(" test fuer map additive Liste mit zufall: ghijst 1\n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                                  putStrLn$ (show (ghijst 1 ) )
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                                --  chartWa1
  
                                                                else if selLECTOR=="14"
                                                                then do
                                                                  avanti [("  test fuer map additive Liste mit zufall , Alle vals mapped\n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                                  putStrLn$  (show (goghijst  ))
                                                                  writeAddIndi   
                                                                  writeOrAdd
                                                                --  chartWa1  
                                                                        
                                                                else
                                                                  avanti [("  test fuer map additive Liste mit zufall , Alle vals mapped\n"++
                                                                           "  eintrag: nummer (zeile): "++ input91)] 
                                                 chart10Mo
                                 else if statiswa1=="4"
                                 then do
                                   avanti [("UNDER CONSTRUCTION")] 
                            
                                   (statisticalWahrsch)  
                                 else if statiswa1=="3"--------------------------------------------------------------------DISPLAY AND WRITE WXMAXIMA
                                 then do
                                   avanti [("Write or ADD; 1==write")] 
                                   wriAdd1 <- getLine
                                   avanti [("outifil vs custom fill  ; outfill==1")] -- outmaxima vs selectfunctions 
                                   outFill <- getLine
                                   
                                   let accesFuncTXT2 =  goghijst -- or (takenN n intoConsider)
 

                                   statisticalWarschtext3 -----------------------------------------------------------------------------Display All MQ-Functions
                                   -- manage writing of WX-Files  
                                  -- export various formats of of files
                                   let finA = if outFill == "1" -----------------------autofil
                                              then do
                                                avanti [("Write/Add to: WxmFile==1 , Vals==2,  Patterfile==3, Wx-Addable==4 or Both 5 (wxm,Vals)")]
                                                foyeMo <- getLine
                                              --  - e: e.g"1", aaKAfoWx : e.g. "4intoConsidere.g."autofil"(see below)
--  -------------decide if to write Vals,PAttern, or both files types

                                                let deCid e aaKAfoWx iintoConsider = if foyeMo=="1" -- write wxm file
                                                     then do 
                                                      (writeFile aaKAfoWx (iintoConsider))   
                                                     else if foyeMo=="2" --write/add vals to txt
                                                     then do
                                                      (eYeMoDa e) 
                                                     else if foyeMo=="3"
                                                     then do -- write/add patternfile style
                                                      (eYeMoDa e)
                                                     else if foyeMo=="4"
                                                     then do
                                                      (writeFile "exportDATA.txt" (show accesFuncTXT2)) --wxm-addable
                                                     else do 
                                                      (eYeMoDa e)           -- write vals AND
                                                      (writeFile aaKAfoWx (iintoConsider)) -- write wxm file


                                                let autofil = (writerOszillos 1) -- set to produce 3 simulated and the real Vals
                                                let autotxt = (takenN 13 intoConsider)  -- 13 takes the simulated vals
                                                
                                                (avanti (lines autofil))
                                                let meProg = if wriAdd1=="1" --write/add readymade autofil 
                                                             then do 
                                                                (deCid "1"  "aOutfill.wxm" autofil) -- backup is wxm data as txt
                                                             else if wriAdd1=="2"
                                                             then do
                                                                (deCid "2" "aOutfill.txt" (show autotxt)) -- these backups txt  will be overwritten every cycle!!!
                                                                       -- writes vals only
                                                             else do 
                                                                (deCid "3" "aOutfill.wxm" autofil) --exports txt as well see above (s.ab.)
                                                meProg

                                                                
                                              else do  -------------------------------custom Datafil - only exports 2 formats, 
                                                avanti [("Enter Funktionsliste e.g [7,6,2,8]")]
                                                ala <- getLine
                                              --  avanti [("Wieviele Spuren anzeigen? (kann momentan nur 2 , wenn 2 dann 7funktion")]
                                                let fospur = ( lines ala)
                                                let spurn = show (length fospur)     

                                         -- foIn:Int (welche liste aus intoConsider) 
                                         -- n: Int (waehle eine variable der liste ) 
                                                let foConsider foInn n = let ste1 foInn = takenN foInn intoConsider 
                                                                 in let ste2 n = takenN n (map show (ste1 foInn))
                                                                -- in ste2 
                                                                 in let ste3 n   = length (ste2 n) 
                                                                 in let ste4 n = [1..(ste3 n )]
                                                                 in let ste5 n = (break (=='.') (ste2 n))	 	
                                                                 in let ste6 n =  (take 5 (snd (ste5 n))) -- zhe digits smaller zero   
                                                                 in let ste7 n =  (fst (ste5 n)) ++ (ste6 n)
                                                                 in let ste8 n = map ste7 (ste4 n )
                                                                 in ((unlines (ste8 n))++"\n")

                                                let foConsider2  = let ste1 f = foConsider f 1 
                                                                   in let tUListe = ( read ala)
                                                                   in concat (map ste1 tUListe)
                                                                                 
                                                let accesFuncTXT =  let aw1 n = (takenN n intoConsider) -- exports intoConsider
                                                                    in let wielanGg  = [1..(read spurn)]
                                                                    in let tUListe = ( read ala)
                                                                    in let aw2 = map aw1 tUListe --- [8,4,6,3] nimm diese funktionen	
                                                                    in (concat aw2)      


                                                let progs = ( (ala))
                                                putStrLn (show progs) 
                                             -- settings WX maxima file 
                                                let accesFuncWX2 =  let aw1 n = (takenN n outPutMaxima3)
                                                                    in let wielanGg  = [1..(read spurn)]
                                                                    in let tUListe = ( read ala)
                                                                    in let aw2 = map aw1 tUListe --- [8,4,6,3] nimm diese funktionen
                                                                      
                                                                    in let enExp a b sqale1 sqale2 = (aCompleteWX2 a b xX sqale1 sqale2)     
                     --  in let aw3 =  ceiling (l/2)
                                                                    in let aw4 = ([wielanGg ,[((read spurn)+1)..((read spurn)*2) ]])
                                                                    in let aw5 = "-100.0" 
                                                                    in let aw6 = "100" 
                                                                  -- in let foaw2 = ""++aw2 --map read( aw2))             
                                                                    in enExp (aw2) aw4 aw5 aw6 --enExp   
                                               {- 
                                                let meProg = if wriAdd1=="1" --write/add readymade autofil 
                                                             then do
                                                                (deCid "1"  "aaKA/aOutfill.wxm" autofil) -- backup is wxm data as txt
                                                             else if wriAdd1=="2"
                                                             then do
                                                                (deCid "2" "aaKA/aOutfill.txt" (show autotxt)) -- these backups txt  will be overwritten every cycle!!!
                                                                       -- writes vals only
                                                             else do 
                                                                (deCid "3" "aaKA/aOutfill.wxm" autofil) --exports txt as well see above (s.ab.)
                                                meProg

                                                -}
                                              
                                                (avanti (lines accesFuncWX2))
                                                  
                                                (avanti (lines (show accesFuncTXT)))
                      -- tasK: eyemoda decides write or add
                      --       

                                                let writeOrAddd = if wriAdd1== "1" && outFill == "2"
                                                                  then do
                                                                      -- (deCid "1"  "aaKA/aOutfill.wxm" accesFuncWX2) --write custom real wxm-file
                                                                       (writeFile nF ( accesFuncWX2)) -- writes txt WX Files
                                                                       (writeFile "aOutfill.wxm" (accesFuncWX2))
                                                                       (writeFile "exportDATA.txt" (foConsider2))
                                                                     --  let nees = takenN 2 intoConsider 
                                                                       (eYeMoDa "1") -- ((snd (ghijst 1 ))) )--( nees) )
                                                                       putStrLn "Wat"
                                                                  else do 
                                                                       add ([nF, (accesFuncWX2) ]) -- add a complete wx file to nf file 
                                                                       add (["exportDATA.txt", (show accesFuncTXT) ]) -- add cust. val unprocessed yet!!
                                                                       add (["exportDATA.txt", ((foConsider2)) ])
                                                                       (eYeMoDa "2")
 
                                                writeOrAddd
                                                putStrLn ( "Wrote :  \""++nF++"\"  AND  \"exportDATA.txt\"")
                                           --   else do
                                            --    let custom = (accesFuncWX  (read spurn) )
                                              --  (avanti (lines custom))
 
                                                  
                                 --  let soViFunk =   [1..(read spurn)] 
                                   finA
                                   putStrLn "4" -- (getFun)
                  --                 -- add [(openA, aWriterOne)]
                                   let writeAddIndi = if wriAdd1 =="1" then putStrLn$ ("WRITE MODE")--indicates if write or add mode
                                                      else putStrLn$ ("ADD MODE")
                                   writeAddIndi
                        --           writeOrAdd 

                                   statisticalWahrsch 
                                 else if statiswa1=="4"
                                 then do 
                                   avanti [("under construction")]
                                   (statisticalWahrsch ) 

                                 else if statiswa1=="5"
                                 then do 
                                   putStrLn ("closing aOsZilloskop1 ..." ) --writeFile nF (show goghijst ) --avanti [("way out")]    --guiReturnfunction (statisticalWahrsch) (chartWa1) 
                                 else 
                                   (statisticalWahrsch ) 
                  chartWa1
     (statisticalWahrsch ) 


-- exports vals back into patternfile, that can be further prcessed :in 'DreiBandTEST'
-- will be used in function see below : aOSZilloskop1 and module DreiBandTest ' ...
-- Function is for EXPORT of my.seto.txt files
-- used for DreiBandTEST iterative 
-- p is for to insert  e.g. MQ functions 
-- s choose if write or add simulated Vals to Setofile
-- used at :ca. 6090
yeMoDa s bobo anyWa ghij nFF = do 
             yearMonthDay <- forM (bobo) (\ a -> do   -- variable needs bobbo
                     let auswahl      = let an =takerlein a (lines anyWa) -- needs 
                                        in an

                     let val = let an =  cvsZeilenBrea (unlines auswahl)
                               in let boss = filter (>40) an 
                               in map chr boss
                     let process = (unlines auswahl) \\ show val
 
                     let findYear =  let an =  (cvsZeilenBrea2 (process))
                                     in map chr an 

                     let year  = jahr findYear

                     let finddate =  let an =  (cvsZeilenBrea3 (process))
                                     in map chr an
     
                     let monat =  month finddate
                     let tag =day finddate
                     let mo3 = show(unlines(words finddate))
 

                     let punkte name a1 a2 a3 a4 a5  =  Punkt name a1 a2 a3 a4 a5
                     let apunkt = punkte val Nothing Nothing Nothing Nothing Nothing 
       --   seeit <- getLine
                     let apunkt2 = apunkt
                     let spuren = show (einTyp (getRid (val))) 
                     
                     let processP  = (snd (snd (ghij a))) -- insert the simulated vals Back into "seto-style"
                     let dataTyp = ""++"("++(concat processP)++","++year++","++monat++","++tag++")"++"\n"
                     return (dataTyp) ) --a also nur zahlen
             putStrLn " the data is (ohne die extra Leerzeile) : "
             let schuser s = if s =="1" 
                             then do 
                              writeFile "sot.txt" (concat yearMonthDay)
                            else if s=="2" 
                            then do
                              add ["sot.txt", (concat yearMonthDay)]
                            else do
                              writeFile nFF (concat yearMonthDay)

             (schuser s)


--------------------------------------------------------------------------------------------------------
-- aCrunchList ist Tool um einlese Strategien fuer Extrema zu finden
-- generiert : max, min 
-- durch das CrunchList display gewinnt man uebersicht -> 
-- die  Funktion: ((show zui)++ "  Ist das Vorkommen der Zahlen im Spektrum von min nach Max")
--                ((show zuu2)++"  Die Zahlen die Vorkmmen in Verbindung mit s.o.") 
--  gibt auskunft Welche Zahlen, Wieoft, Wo (in welcher Reihenfolge) stehen
--  so laesst sich eine Strategie festlegen um Sets , also Val listen
--  einzulesen  , schreibt keine Files und kann vollstaendig integriert werden              
    
--howMany String , wieviele Zeilen einlesen
--
exCrunch faden = aCrunchList1 "seto.txt" "aaKA/hubbort.txt" faden "0.56"
aCrunchList1  x nF howMany crit= do
  --   putStrLn$ "Enter starting Line:"
     let anfang = ("1")
     let xX = howMany
     
   --  database <- readFile "milch.txt"
     --let machAdd f = add [ nF, f]
     anyWalk <- readFile x -- z.b. "mageWalAllPath.txt"--durble.csv
     let outRead = length anyWalk
     putStrLn$ (""++ (show outRead))
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
   --  add [nF, (concat dipfade)]
     let muster = ("1")
     let criterium = crit 
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
                      else (val++tag)

          let dataTyp =  (modus)
          return (dataTyp) ) --a also nur zahlen
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
  


  --   putStrLn (show dipfade) 
       
     
     let more = ((dropWhile (\(val) -> val < criterium ) ( dipfade2)))
     let laengeSet = length (dropWhile (\(val) -> val < criterium ) ( dipfade2)) --wie lang ist set
     let max = (maximum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))
     let min = (minimum (dropWhile (\(val) -> val < criterium ) ( dipfade2)))
     let wievieleMx = length(snd (partition (>criterium) (dropWhile (\(val) -> val < criterium ) ( dipfade2))))
     let wostehenMx = criterium `elemIndices` (dropWhile (\(val) -> val < criterium ) ( dipfade2))
     
     let comparePosition = let hochundtiefPunkt = if ""++(show laengeSet) == (criterium) then putStrLn "Found Hoch/Tiefpunkt"
                                                  else putStrLn "\n\n\nkein hoch oder tiefPunkt"
                           in hochundtiefPunkt


     let lister2  = let a=  max
                    in let b = (read a)
                    in let a2 = (read min)
                    in let c = ((b)-0.01)
                    in let d = [(b),(c)..(a2)]
                  --  in let e = drop 4 (take 5 d)
                    in let f = map round (map (*100) d)
                    in f
   
     
     let wos =  group more
     let spuren g h= (einTyp3 (getRid (g h)))

     dipfade4 <- forM (bobbo2) (\ a -> do
          let auswahl      = let an = drop (a-1) (take a (wos))
                             in   ( an)
          let rosi =  ( take 1 (map length (concat auswahl)))
          let dataTyp3 =  (auswahl)
          return (dataTyp3) ) 


     let zui = map length (group (concat (sort (concat dipfade4))))-- Das Vorkommen der Vals von min nach max")

     let zui11 = (map head (concat dipfade4))
    
     let zui1 = map length (group (concat (concat dipfade4)))-- die val in Zeitgruppen eines Intervalls  
     let zuu2 =  sort (nub ((concat((concat dipfade4)))))
     let zuu22 d =  map round (map (*100) d)
     let zuu3 =  (lister2 \\ (zuu22 (map read zuu2)) )


    -- mapM print (dipfade2)
       
-- to use GUI CrunchList
-- --a = string name bzu bearbeitende file
--b = string Name der Quell datei
--b2 = string IO input, wieviele zeilen eingelesen 
--c = string name max
--d = string min 
-- e  = string val einzel
-- e2 = String val vorkommen 
-- f = string crit stellen

-- a = nF ; b = x ; b2 = anyWalk  , c = max , d = min, e = crit ; e2 = zui; f = wostehenMx
     let crunch = listCruncher a1 b1 b2 c1 d1 e1 e2 f1
          where
          a1 = nF;
          b1 = x;
          b2 = xX;
          c1 = max;
          d1 = min;
          e1 = crit;
          e2 = zui;
          f1 = wostehenMx

     let buildDisplay  t = do
                        header <- readFile "C:/Users/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/stream-crypt/test/TestGUIheader.txt"
                        tailer <- readFile "C:/Users/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/stream-crypt/test/TestGUItail.txt"

                        --color <- getLine -- e.g green
                      --  yum <- forM aw4 (\a -> do 
                        let dose k farb =   take k (repeat (map ord farb)) -- wird e.g ["green","green",..
                        let onG g = ((dose (length zui)) g)  -- dispaly solang wie spektrum    
                        let perG =  ( onG (a1 ("green")))--    maps button with dose monad
                        let hoeheDisplay = let ste1  = maximum zui 
                                           in let ste2 = take (head wostehenMx) zui 
                                           in ste2
                        let foinS h = (map chr (concat h))
                        let ahta = [(header)++ (map chr(concat perG))++tailer++abreak] -- ++ (concat perG) ++ tailer)]
                        let mapZ g = onG (a1 g)
         --               let checkerliste = 
                     --   let newZui =  in foaw0 = g `elemIndex` zui
                       --               in let aw1  = break (>=g) zui -- finde leerstellen in zui
                         --             in let aw2 = 0 : (snd aw1) -- fuelle leestellen in zui
                           --           in let aw3 = (fst aw1)
                             --         in let aw4 = (aw3) : [aw2]
                               --       in aw4
                        let neZui = let df1 = zui --if (read xX) <= 60 
                                             -- then zui
                                            --  else (concat vorlete) 
                                    in df1
                        let foexP g = (map chr(concat (onG (a1 g)))++abreak++"\n" )
                        let newOng g = map chr(concat (dose (36) g)) -- nimmt e.g maximimm zui statt 100 , wie hoch , wieviele zeilen
                        let fobreak    =  let getFonk = zui -- schliesst an foexP an
                                          in let zeiLaenge = [0..(read howMany)]------------------------------------------LAenge Zeile
                                          in let vieleZeilen = [1..(60)] ------------------------------------- Menge Zeilen 
                                          in let selev x = drop (x-1) (take x neZui)
                                          in let seleMult p o = drop (p-1) (take p o)
                                          in let mapv = (map selev [1..(length neZui)]) -- parst zui in die liste: mapv  
                                          in let chuuvs etwas = ((maximum zui) - etwas) -- berechnet abstand zu zui 
                                          in let miMonivd = map chuuvs (concat mapv) -- addapter liste zu zui
                                          in let prepZip zeile = let aw1  = (take (length miMonivd) (repeat zeile))
                                                                 in aw1 -- rechnet zb miMonivd - [5,5,5,5.. -> 5. zeile
                                          in let foOH zeile = (zipWith (-) miMonivd (prepZip zeile) ) -- zeile1 - 1 , zeile2 -2 ..von miMonivd
                                          in let acceS p zeile =  (seleMult p (foOH zeile)) -- eine zeilen funktion wandert runter , braucht:
                                          in let lineStep p =  (seleMult p neZui) -- zum vergleich dieses Spektrum
                                          in let ooH p zeile= if (acceS p zeile) <= (lineStep p) then "blue"  -- in let mapStep = 
                                                              else "white"
                                        --  in let sets zeile stelle = (seleMult stelle (ooH stelle zeile))
                                         -- in let foexp x p = (ooH x p) 
                                          in let expo lisX p = map (ooH p)  lisX  -- lisX: length zui  ; p :maximum zui
                                          in let moexp p zeiL =  (expo zeiL ) p 
                                          in let moexp2 = map foOH [0..(maximum zui)] -- die liste wo immer die jew. stelle abgezogen wird und mit zui stelle verglichen
                                          in let moexp3 p = concat (seleMult p moexp2) -- slect single list miMonivd -> fooH zeile -> e.g  0 enter [22,0,17,16,20... 
                                          in let moexp4 =   (map moexp3 zeiLaenge)
                                          in let moexp5 p zeil = map (moexp p ) zeil -- map  
                                        --  in (moexp5 p (  moexp4)) --(moexp 1) --(moexp3 1) --map foOH vieleZeilen --[moexp [1..10] --moexp [1..10] -- ( ooH 1 1 ) -- prepZip 5 --miMonivd -- (foOH  1 )--map sets [1..(length zui)] --[(map sets [1..10])]

                                          --in  miMonivd if 
                                          in let sosos =  (newOng "blue")
                                          in let mapfarliste  = ( newOng (foexP "blue")) -- max zui viele zeilen 
                                          in let farliste2 g   = let aw1 x p = drop ((x*(length neZui))-1)   (take (x*(length neZui)) ( (concat( moexp5 p (moexp4 )) )))--let aw1 g = aoexP  (head (head (take g fobreak)))
                                                                 in let aw2 p x = map (aw1 x) p
                                                                 in let foaw2 x = aw2 [1..(maximum zui)] x   
                                                                 in let newExp g =  ( (  (a1 g))++ abreak )
                                                                 in ( (foaw2 g))  -- map newExp (concat (foaw2 g)) --(concat(foaw2 g)

                      
                                          in let farliste3 g =  let aw1 x = ( ( farliste2 x )) -- alle felder mit farben gruen oder blau
                                                              in   map  aw1 (foOH g)
                        
                                          in let farliste4 p = let aw1 p =  (( farliste3 p))
                                                             in let aw2 g =   map a1 g -- verbinde mit der a1 funktion die den html box/button baut 
                                                             in  (concat (aw2  (take (length neZui) (concat (concat (aw1 p))))))++ abreak 
 
                                          in let farliste5  = map farliste4 [0..(maximum zui)] -- (maximum zui)]
                                          in concat farliste5 -- show (map foOH [0..30])--(concat farliste5) -- (show (lineStep 2))++(show miMonivd) ++(show (foOH 1)++show (foOH 0)) -- (concat farliste5) -- concat (concat(concat farliste3 )) -- (concat(concat (farliste2 1))) --farliste4
           --             let fohta2 g = if g<= g  then (mapZ g)
             --                          else (mapZ g)
                      --  let manyLines t g = take t (repeat (fohta2 g))
                        
                        let hta2 = [(header ++((foexP "green" ))++(((fobreak))) ++((foexP "green" ))++ tailer)] 
                        writeFile "C:/Users/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/stream-crypt/test/TestGUIVal.hta" (unlines hta2)
                        
                        avanti ahta
                      --  aw6
                        putStrLn (show zui)
                        putStrLn (show max)
                      --  putStrLn (show ( fobreak ))
                        putStrLn (show wostehenMx)
                        putStrLn (show wievieleMx)
                     --   putStrLn (show (lister2))  -- VollesSpektrum beinhalted die die fehlen e.g [76,75,74,73...,58] 
                        putStrLn (show zuu3) -- die die fehlen x100
                        putStrLn (show aw2)
                        putStrLn ( show (wostehenFehln 1))  
                        putStrLn (( show (doIter)))  
                     --   putStrLn (show vorlete)        
          --  putStrLn (show (fohta2 "green"))
           where
             soloang = [1..t];
             a1 foSt = ("<input style=\"width: 12px; height:1px; color: white; background-color: "++foSt++"; font-family:Book Antiqua;\" type=\"button\" Value=\"L\" onclick=\"Call Run('C:/Users/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/Logini17/Neuer Ordner/BackBoneOldFiles/HaskellSVG_XHTML2017/Pipes01-17/FunctionCloud/LoginiScript/TestClose1.hta')\">");
             a2 foSt = ("<input style=\"width: 12px; height:2px; color: white; background-color: "++foSt++"; font-family:Book Antiqua;\" type=\"button\" Value=\"L\" onclick=\"Call Run('C:/Users/Watson/Documents/DynamicSystems/Experimente/FractionalLearning/Logini17/Neuer Ordner/BackBoneOldFiles/HaskellSVG_XHTML2017/Pipes01-17/FunctionCloud/LoginiScript/TestClose1.hta')\">");
             abreak = "<br>"
             aw1 = zui; --spektrum MIT Fehlenden 
             aw2 = lister2; -- die Vals des gewaehlten ausschnitts
             fehlt x foiter = head (drop ( x-1) (take x foiter ));
             aw3 = max;
             aw4 = [1..(length aw2)];
            -- aw5 = wostehenMx;
             howMany = "300";
             aw6 = aCrunchList1  x nF howMany crit;  --howMAny lines to read
             wostehenFehln x = let prpA x =  (fehlt x zuu3)  -- diese und folgende built ZUI mit Luecken
                               in let st1 = ((prpA x) `elemIndices` ( lister2))
                               in let st2 = (take (head st1) zui)
                               in let st3 = 0 : (reverse st2)
                               in let st31 = zui \\ st2
                               in let st4 = ( take ((length zui)-1) zui)
                               in let st41 = let ak1 =  (head st1)
                                             in (drop ak1 st4)
                               in let st5 = [(reverse st3)] --, st31]
                               in (concat st5) ;
              doIter =  map wostehenFehln [1..(length zuu3)]; -- (head(length zuu3)) )
              vorlete = let gh1 x = (concat (drop (x-1) (take x (doIter))))-- target is die vorletzte Ziffer jeder zeile
                        in let gh2 x =    ((length((gh1 x)))-1)
                        in let gh4 x = let hj1 =  (gh1 x) 
                                       in  (drop ((gh2 x)-1) (take (gh2 x) (gh1 x)))
                        in let gh3 x = let bg1 = ((gh4 (x)))
                                       in  (head bg1) `elemIndices` (gh1 (x+1))  --  die Stellen wo nullen eingefuegt werden
                        in let gh5  =  let foxd1 xd = (head( (gh3 xd))+1)  -- take one more because of elemIndices
                                       in let foxd2 xd =  (head (gh3 xd))
                                       in let fotake = (concat (map gh3 [1..(length zuu3)])) 
                                          --                                          [1,6,0],[1,6,39,0],[1,6,39,21,14,0],[1,6,39,
                                       in let xd1 xd = (drop (foxd2 xd) (take (length (gh1 xd)) (gh1 xd))) -- nimm vorletzte ziffer aus jeder s.o.

                                       in let xd3 xd =  let foxd3 xd = (head ((gh3 xd))-1) 
                                                        in (drop (foxd3 xd) (take (length (gh1 xd)) (gh1 xd)))  -- die letzten drei aus s.o
                                       in let dake = let pe1 = ((length zuu3)-1)
                                                     in map xd1 [1..pe1]
                                       in let nulInsert = dake --intercalate [0 ]dake 
                                       --in fotake
                                       in let breaker = let fofo1 xd = head (xd1 xd) 
                                                        in let fob2 = ((length zuu3)-1)  -- letzte Nullstelle kann Grenze sein! e.g [1,6,0...3,0]
                                                        in let fob3 =  map fofo1 [2..fob2]
                                                        in let fob4 xv = (drop (xv-1) (take xv fob3)) -- acces fotake , nimm vorletze ziffer  
                                                        in let fob5 xv = (head (fob4 xv) ) `elemIndices`  (gh1 (xv+1))-- wo steht vorletzte in foIter
                                                        in let cleanlist = let bh1 =  ((length zuu3)-2)
                                                                           in let bh2 = map fob5 [1..bh1] -- hat noch gruppen
                                                                           in map last bh2 -- entfernt gruppen; exportiert nur einfache liste
                                                        in let listnorm xv =  let bh1 =  ((length zuu3)-2)
                                                                           in let bh2 xv= drop (xv-1) (take xv (gh1 xv))-- 2. take snd; 3. take trd, 4. take 4th ....
                                                                           in bh2 xv -- map bh2 [2..bh1]
                                                        in let esiApro = let foesi c = (length (gh1 c))
                                                                         in let  fsi2 c = drop ((foesi c)-3 )(take (foesi c) (gh1 c))
                                                                         in map fsi2 [1..(length zuu3)]
                                                        in  let thetype xd = concat [(gh1 xd ),(xd1 (xd+1) )]  --fob4 1  --[[cleanlist] ,listnorm] --   [cleanlist,[(fob1 1)],listnorm,dake] -- fob3 --fob4 4 -- cleanlist
                                                        in let thet2 xd =  [(thetype xd),(xd1 (xd+2))]
                                                        in let the4  = map xd1  [3..((length zuu3)-4)] 
                                                     --   in let the3 xd = [(thet2 1), (xd1 (xd+1))]
                                                        --in map xd3 [1..((length zuu3)-1)] -- die letzten drei
                                                        -- in fob3 -- alle vorletzten 
                                                        -- in dake -- die letzten beiden
                                                        in let tes1 xd =  (xd3 xd)  -- nimm letzte drei
                                                        in let tes2 xd = (head(tes1 xd)) -- head dieser liste
                                                        in let tes3 xd = if (tes2 xd) == (fofo1 (xd-1))
                                                                      then (xd1 xd)-- gebe zweier aus 
                                                                      else (tes1 xd) -- oder gebe letzten drei raus 
                                                    --     nimm letzten drei , wenn erste zifferne dieser listen == der vorletzten des vorherigen schritts
                                                     --    dann setze NUR letzten beiden ein sonst die letzten drei  
                                                        in let tres4 = map tes3 [2..((length zuu3)-1)]
                                                        in let tres5 = (concat [ [(gh1 1 )],tres4 ]) -- ((concat [[(thetype 1 )],[(listnorm 4)] , the4 ]))-- (map thet2  [1..((length zuu3)-3)]) -- , (thet2 4) ]
                                                        in let tes4 xd = if (length (tes3 xd)) > 3 then drop ((length (tes3 xd))-3)(take (length (tes3 xd)) (tes3 xd))
                                                                         else tes3 xd 
                                                        in let tres4 = map tes4 [2..((length zuu3)-1)]
                                                        in let tres5 = (concat [ [(gh1 1 )],tres4 ]) 
                                                        in let tres6 = let fo5 xd = concat (drop (xd-1) (take xd tres5))
                                                                       in let getMidle xd =  (drop 1 (take 2 (fo5 (xd-1)))) -- the second cijfer,vorherige liste
                                                                       in let getFst xd = head (fo5 xd) 
                                                                       in let outts xd =  if (length (fo5 xd)) > 2 &&  (head(getMidle xd)) == (getFst xd) --wenn dreier liste mittlere
                                                                                          then -- zahl von vorherige == erste von dieser lister dann mach zweier liste
                                                                                               (fo5 xd) \\ [(getFst xd)] --
                                                                                          else (fo5 xd)
                                                                       in map outts [2..((length zuu3)-1)]
                                                        in let getEnd =  drop (last cleanlist) (take (length zui) zui)   
                                                        in  [(gh1 1), (concat tres6),getEnd] 
                                                                                                              
                                                                                                  
             --  fotake: -- wo stehen vorletzte [ziffer-1] als liste -> alos +1 zur verarbeitung
             --  cleanlist: wo soll eingefuegt werden  
             --  xd1 =- [dake] =- fofo1  nimm vorletzte ziffer aus jeder s.o.               e.g [
             --  xd1 =- voreltte und die null
             --  
                                       in breaker --fotake  --breaker --xd1 2-- --breaker 
                                      --  in fotake -- let tatake = fotake-- waehl aus [1,6,0] , [1,6,28,]...
                           
                        in gh5 -- map gh3 [1..((length zuu3)-1)] 
     (buildDisplay  5)

     crunch

