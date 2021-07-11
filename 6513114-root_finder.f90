!------------------------------------------------
!PHY1038 Assingment 1 - Newton-Raphson root finder
!URN 6513114 - CPTL Lab Group 2F
!February 5th 2018
!------------------------------------------------

!This program finds the real roots of a cubic function, using the Newton-Raphson method

PROGRAM root_finder

	IMPLICIT NONE

	REAL :: x_0, a, b, c, d, poly, x, dpoly, x_new
	INTEGER :: i

	WRITE(6,*) ' '
	WRITE(6,*) 'This program finds the roots of a cubic function'
	WRITE(6,*) ' '
	WRITE(6,*) 'ax^3+bx^2+cx+d'
	WRITE(6,*) ' '
	WRITE(6,*) 'The solution is worked out through the Newton-Raphson method'
	WRITE(6,*) ' '   

	WRITE(6,*) 'Input value for a'  			!inputting values for the coefficients of the function
	READ(5,*)a

	WRITE(6,*) 'Input value for b'
	READ(5,*)b	

	WRITE(6,*) 'Input value for c'
	READ(5,*)c

	WRITE(6,*) 'Input value for d'
	READ(5,*)d

	WRITE(6,*) 'Make an initial guess for the root'
	READ(5,*)x_0

	x=x_0
	i=0


	DO							!do loop to do the Newton-Raphson iteration to find the root

		poly=a*x**3+b*x**2+c*x+d	
		dpoly=3*a*x**2+2*b*x+c
		x_new=x-(poly/dpoly)
		
		IF (abs(x-x_new) < 0.000001) THEN		!if statement to find the root to a suitable accuracy
			EXIT
		ENDIF

		i=i+1						!this equation counts the number of iteration we do of the do loop
		x=x_new					
		
	END DO	


	WRITE(6,*) ' '
	WRITE(6,'(a22, f10.2)') 'Your initial guess was',x_0 	!outputs are formatted to 2 decimal places
	WRITE(6,*) ' '
	WRITE(6,'(a12, f10.2)') 'The root is',x
	WRITE(6,*) ' '
	WRITE(6,*) 'Using a tolerance of 10^-6'
	WRITE(6,*) ' '
	WRITE(6,*) 'Which took',i,'iterations to find the root'

END PROGRAM root_finder
 
