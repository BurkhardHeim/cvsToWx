# csvToWx
###### Content: 
######      1.'writePatternfile_____2.'aMemoryRAW'________3.'aOsZilloskop1RAW'_____4.'UNDERDEVELOPMENT'
   ######      modes of Files: 1- WX 2- TXT
Setup: Three modi  
- 1. A GHC GUI
- 2. Automatic (the same as above avoiding GUI's UNDERCONSTRUCTION )
- 3. Easy Acces (quickly read a csv file and process...)

i. will open a simpel Graphical User Interface (GUI) in the Glasgow Haskell Compiler (GHC)
   the aim is to automate reading of csv data and of long number streams 
   converting them and changing them via predefined functions. The whole program
   is derived from four main functions.
   
ii. Runs all functions as i. but avoiding the GUI...(realisation under construction 23-09-19)
    of 4 main functions some can be completly controlled just via Global-Variables*.
    which are ...aMemoryRAW  !! so far

iii. Quick and easy access to read csv -> write patternfile.txt
##### Main
- 1. A GHC GUI
enter> main
  ![alt tag](https://github.com/CBroemse/cvsToWx/blob/master/Manual1.png)
  
    The advantage of using Global Variables, the stream-crypt program can be fully automated which helps
    overseing bigger longer files and keeping an eye on the real aim of the Wx-Maxima application.
    
    The disadvantage of using Global Variables is that we can lose track of which functions we just
    applied to our data, getting no visiual feedback, both sides can be avoided be directly calling
    the "PatternfileRAW' functions that need more variables but can be easily be called via your own batch/bat
    files or any other program. 

##### Four functions to use   
The whole stream-crypt revolves around four functions that do the 'dirty' work. 
The easiest and almost laziest way is by utiliesing commandline and directly typ in
the function plus variabes.
   #### 1. 'writePatternfile' 
   via 'Patternfile.hs' read a csv file from disc and export it as aPatternfile.txt
   e.g enter: 'writePatternfile x nF'
   
   ###### x="fileTorRead.csv" ; nF = "fileToWrite.txt"
       writePatternfile x nF => format [val,year,month,day]
   fire up your editor and jump to line :858
   below the mentioned code and the GHC GUI are pictured to compare
   Enable 'writePatternfile' to read other csv formats!
   ![alt tag](https://github.com/CBroemse/cvsToWx/blob/master/Manual2.png)

  #### 2. 'aMemoryRAW'  
  writes screen1.txt statistical data about the data set
          autoInputII:String; e.g "1" if ==1 -> hide output ?!
        => opens menu: Options : POINTCLOUD 1..4,6: Pointcloud
                             CLOSE 5
                              
        => maybe writes: "lala.wxm" filled with  1 or 2 ..or 4 or 6
                 content: if 1 or 2 or 3 or 4 or 6 == True -> do P.aMemoryRAW autoInputII file22writ file33writ
                 with: autoInputII = "1"; file22writ; file33writ;
                          else do P.aMemoryRAW "2" file22writ file33writ
                          
        => writes: "screenI.txt" --a crude statistical overview of the set 
        => proceed: start a second Computation from the same source
        => reads: fileToread.txt (same as step above)
        => order: applies predefiend function to the value 
        =>        [MQ3,MQ4,MQ5,MQ6] -- group of sin functions (periodic)
        => do Wx.WriteMQScreenI && Wx.WriteMQ6ScreenII
        => load: if  then 'WriteWXmaximafile.hs* -> do writeMQ6SCREENI
                 else 'WriteWXmaximafile.hs* -> do writeMQ6SCREENII
  select any String you to import as as line into Wx-Maxima, 'aMemory' will convert it
  to WX.maxima format. Set to a random pointcloud while a myriad other settings are out there.
  
  ![alt tag](https://github.com/CBroemse/cvsToWx/blob/master/Manual3.png)  
  
        => function: aMemoryRAW outputOrnot fileToread fileTowrite 
                                  String     String    String
        => :*Patternfile>aMemoryRAW "1" "seniot.txt" "seniot7.txt"             
   
  #### 3. 'aOsZilloskop1RAW'
  will prepare real vals and simu vals, 
  it. will exportDATA.txt in first run (simu vals for re-import)
  exportDATA2.txt cleaned just vals [String]
  
    => writes: exportDatat1,exportData2,HTML
    => function: aOsZilosko1 outputOrnot fileToread   fileTowrite   howMAny   crit   foHans   fun1 fun2
    => types:                  String     String        String      String   String   String   Int  Int
    => *Patternfile>aMemoryRAW "1" "seniot.txt" "seniot7.txt"
  ###### First and formost yields a Clean.txt file:[String]
  that by now has been sorted. This file can be reimported as aPatternfile again but thus the 
  ###### 1.snd Computation in aOsZilloskop1
  ###### 2.choose Simulation (not fully functional yet) will simulate but to much noise
  ###### 3. call every single statistical function used in Patternfile and write to file.txt
  ###### 4. calls aOszilloskop again and applies our Gobal Var selectFunction (Patternfile.hs)
  ###### 5.CLOSES
  ###### 6. Help UNDERCONSTRCTION
  
  Is set to use MQ functions, which are simple sin functions, nice periodic functions. 
  
           => in 'Patternfile.hs' see global variables 
           : set function: selectFuncL = ( show [1,2,3,4,5]) 
           : control which functions to seect of 
  


#### MQ-Functions
The name stems from 'Magisches Quadrat'
  magic square in English. The Oszilloskop uses [MQ3,MQ4,MQ5,MQ6]
###### The 3x3.
["There is just one 3x3 magic square although rotations and reflections produce eight variations. The 3x3 square cannot be pan-magic..."](https://www.grogono.com/magic/3x3.php).

        => in WriteWXmaximaFile.hs
           fofourier1 = [4,9,2]
           fofourier2 = [3,5,7]
           fofourier3 = [8,1,6]
           
           fourier1 (x) = (sin((head fofourier1)*x)+ sin ((last (take 2 fofourier1))*x) + sin ((last fofourier1)*x)) 
           fourier2 (x) = (sin((head fofourier2)*x)+ sin ((last (take 2 fofourier2))*x) + sin ((last fofourier2)*x))
           fourier3 (x) = (sin((head fofourier2)*x)+ sin ((last (take 2 fofourier3))*x) + sin ((last fofourier3)*x))

           fourier123 (x) = (fourier1 x + fourier2 x + fourier3 x)*(1/90)
Remarkably fourier123 (x) Reihen und spalten are the same fucntion that proves the thing behaves a little bit like a 
magic square as plottedn below MQ3Rows = MQ3Collums ,a periodic function to explore.


  
 ##### Example: Stream-Crypt at work
  The intention was to 'bend' Data sets improving upon my first crude statitical analysis- I could say let the whole Set end lower than the input set depending on
  the highest/lowest points and where I will prope the data, e.g
  ##### picMQ3:
   ![alt tag](https://github.com/CBroemse/cvsToWx/blob/master/MQ3.png)
  A simple sin would have done it as well :) but just for curiosity reasons I kept that one, Or did it ???.
  Depending on how an 'interval' is defined it may bring some differences.
  I wanted to be able to visiualize one MQ3 , one of the four pandiagonal 4x4 and  .... 5x5 , ..6x6
  mixed with a given set of vals, source and simulated  and just gaze into the abyss :) of data science.
  Hence if I take one interval of the MQ3 function and a let it loose on the data, naturally the data Set will
  end up lower than the original one. 
  In 'aOsZilloskop1' the vals are 'befriended' with a MQ function.
  changing the vals of a given data set:
  I can say:       
  
                => length (ofSet) = someNumber `:= Ls1
                => with: ls1 = (one Interval of MQ3)
  
  ![alt tag](https://github.com/CBroemse/csvToWx/blob/master/MQ3mixVal_Val_SimuVals.png)
   I as it turns out the input vals in MQ3 do resemble the original function picMQ3, here ploted with
   the original input data. but is not 'atuned' to the original vals by 'atuned' i mean
   that the y1 and y2       |y1. . . . . . .y2
                            |___________________  values of one of the functions 
                             x1            x2  
  Should be really close to the y1..y2 of another of these functions. 
  Such as the simuVals y1 y2  relate to the realVals y1 y2 as plotted.
  #### picMQ3:
  
   ![alt tag](https://github.com/CBroemse/cvsToWx/blob/master/allMQsRealVals.png)
  Aright MQ3 did not work but all the other and now the red simuVa problem reveals itself.
  So by fiddeling around at the input dat in OsZilloskop1 we must 'tune' into the data and do 
  the right one if we like the y values to be rlativly close.
  ##### What is good about this
  The functions now tuned in can be used in simulations about the data set
  if using the various MQ functions.
  ##### The drawback
  This is all but viziualisation there is no problem solved by making this a plot.
  If the generated data may contain any information what sort ever it must be carried
  out by another domain that is capable to 'weigh' or 'rate' each newly generated function
  ###### good about the drawback Using the dit rnd function we can make each generated
         function 'unique' and that possibly could help that other 'weigh' rate' domain.
   #### 4.'aCrunchlist1RAW'
  UNDERDEVELOPMENT
  most basic HTA/HTML plotter can be easily changed from hta to html by 
  just typing differet format afile.hta <=> afile.html, 
  u,
  Hello Microsoft ;), the hta runs 
  Haskell generated VB.script that cannot be displayed in (firefox,..)
  
  writes senio, writes Html, using global variables   
       => writes: TESTGuiVal.hta
       
Patternfile.hs
'writePatternFile "fileToRead.csv" "fileToWrite.txt"'
 Reads a CSV file e.g: "durble.csv"
 Writes a a file with line format (val,year,month,day) a file called 'Patternfile.txt'

'aMemory '
Reads 'Patternfile.txt'
Turns it into a String will plot some statistic about the given DATA Set.

Write a WX.Maxima file
## Nexus: Armchair-phillosohy ,
   ### What is good about the code?
   ##### The stuff is without any blows and whistles, like 'sophisticated Haskell'
   One of the most remarkable sentences abt. Haskell ,to me, is to keep it 
   rather simple more functional with less fancy code that will blow number theorisists minds
   but not be easy or handy for simple programming tasks. The main Menu helps to automate many tasks
   ### The drawback
   Especially Patternfile.txt has lengthy repetitive code passages that could  be easily shortend up 
   and compacted with purer code. The first aim of this programming venue was to setup a working program
   that can handle longer files with a new Main module and many adjustments from 'older' (three years) code.
   By just focusing on the overall program structure and less on the brliance of just one function. The code 
   even though the main part of aMemory, aOsZillosko1 and The main menu is       exclusivly developed for GitHub,
   #### What is good about the drawback
   repetitive and very similar code is begging for parrallel computing which is a task for the 
   future. https://github.com/simonmar/par-tutorial .




