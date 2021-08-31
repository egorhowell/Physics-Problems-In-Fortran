
!This program finds the taylor expansion of the funtion sin(x)*e^(-x/2) about x=0
!Writes to a file, which can be plotted on gnuplot

PROGRAM taylor
	IMPLICIT NONE

	REAL :: x, f_x, tf_x, approx
	REAL, PARAMETER :: pi = asin(1.0)*2.0 !accurate pi value
	INTEGER :: n, i, j
	
	WRITE(6,*) 'This program finds the taylor expansion of sin(x)*e^(-x/2) about x=0 for n number of terms'
	WRITE(6,*) ''
	WRITE(6,*) 'It then outputs values for f(x) for given x values, which can then be plotted on gnuplot' 
	WRITE(6,*) ''
	WRITE(6,*) 'How many terms do you want the expansion to be'	
	READ(5,*) n
	WRITE(6,*) ''

	OPEN(unit = 20, file = 'taylor.dat')

	DO j=-50,50
		
		x=(2.0*pi/50.0)*j  !defining x in pi and j to make sure do loop has integers

		f_x=sin(x)*exp(-x/2.0) !actual values of the function at x
		
		approx = 0
		
			DO i=1,n
			
				tf_x = ((-(5.0**0.5)/2.0)**i) * (sin(-i*atan(2.0)))   
				approx =  approx + tf_x * (x**i)/(GAMMA(REAL(i+1.0))) !use of gamma function for factorial
					    
			END DO
		
		WRITE(20,*) x, f_x, approx !outputting the actual values with the approximation value
	END DO
	
	!To plot this graph on gnuplot open gnuplot by typing 'gnuplot' in terminal
	!Write 'plot 'taylor.dat' using 1:2 with lines, 'taylor.dat' using 1:3 with lines'
	!This should give you two lines of the actual graph and the approximation

END PROGRAM

	
