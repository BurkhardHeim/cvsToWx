# csvToWx
Setup: Three modi  1. A GHC GUI \2. Automatic (the same as above avoiding GUI's)
                   3. Easy Acces (quickly read a csv file and process...)

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


