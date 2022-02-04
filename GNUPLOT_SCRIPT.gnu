#GNUPLOT SCRIPT 
#for my one the triangle was rotated pi radians (3.14/180 degrees)


#Type gnuplot in terminal to get it up and running
#When in gnuplot you should see the prompt "gnuplot>"
#Type these commands line by line into the prompt to view your .png file for the triangle


set terminal png
set output "6513114-final.png"
set xrange [0:3]
set xlabel "x"
set yrange [0:3]
set ylabel "y"
set zeroaxis
set termoption enhanced
plot "A4.dat" using 1:2 with lines title "new triangle", "A4.dat" using 4:5 with lines title "initial triangle"

#To see the plot of both triangles type "eog 6513114-final.png" in terminal 
#This should show you a file of both triangles, where one has been rotated 180 degress about the centre of mass

#Alternatively open gnuplot and type load "6513114-GNUPLOT_SCRIPT.gnu"
#This will create a .png file in your current folder






