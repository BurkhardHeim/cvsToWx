# csvToWx
Setup: Three modi  
- 1. A GHC GUI
- 2. Automatic (the same as above avoiding GUI's UNDERCONSTRUCTION )
- 3. Easy Acces (quickly read a csv file and process...)

i. will open a simpe Graphical User Interface (GUI) in the GLasgow Haskell Complier (GHC)
   the aim is to automated reading of csv data and of long number streams 
   comverting them and changing them via predefined functions. The whole program
   is derived from four main functions:
   -a. writePatternfile reading a csv file from disc and exporting it as aPatternfile.txt
       in format [val,year,month,day]
       
   -b. aMemory  writes screen1.txt statistical data about the data set
        => opens menu: Options : POINTCLOUD 1..4,6: Pointcloud
                             CLOSE 5
                              
        => maybe writes: "lala.wxm" filled with  1 or 2 ..or 4 or 6
                 content: if 1 or 2 or 3 or 4 or 6 == True -> do P.aMemoryRAW autoInputII file22writ file33writ
                 with: autoInputII = "1"; file22writ; file33writ;
                          else do 
ii. Will run all functions as i. but avoiding the GUI...(realisation under construction 23-09-19)
    of 4 main functions some can be completly controlled just via Global-Variables*.
    which are ...aMemoryRAW  !! so far 
    The advantage of using global Variables the stream-crypt program can be fully automated, which helps
    overseing bigger longer files and keeping an eye on the real aim of the Wx-Maxima application
    
    The disadvantage of using Global Variables is that we can lose track of which functions we just
    applied to our data, getting no visiual feedback, both sides can be avoided be directly calling
    the "PatternfileRAW' functions that need more variables but can be easily be called via your own batch/bat
    files. 
    any other program 

![alt tag](https://github.com/CBroemse/cvsToWx/blob/master/Manual1.png)

Patternfile.hs
'writePatternFile "fileToRead.csv" "fileToWrite.txt"'
 Reads a CSV file e.g: "durble.csv"
 Writes a a file with line format (val,year,month,day) a file called 'Patternfile.txt'

'aMemory '
Reads 'Patternfile.txt'
Turns it into a String will plot some statistic about the given DATA Set.

Write a WX.Maxima file

make a List of the Functions that shall be plotted in WX-Maxima
e.g: let thisList x = [x,x+x,x^2]
     ->               [1,2,3]
 
 aCompleteWX writes the WX-Maximafile
 
 aCompleteWX 
 
#### Step 1: Read csv, write patternfile.txt;
![alt tag](https://github.com/CBroemse/cvsToWx/blob/master/Manual2.png)

![alt tag](https://github.com/CBroemse/cvsToWx/blob/master/Manual3.png)


