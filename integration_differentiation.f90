
!this program works out derivatives and integrals for certain functions
!stating the functions which will be used in a module for the program

MODULE methods

	CONTAINS		!this module contains all the functions which will be used in the program


	!functions for the first derivative
	FUNCTION f(x)
		IMPLICIT NONE
		
		INTEGER, PARAMETER :: rk = SELECTED_REAL_KIND(P=12, R=40)
		REAL(KIND=rk) :: f
		REAL :: x
		f = 1.0/(x**2+3+sin(x))
	
	END FUNCTION f

	FUNCTION dfor(x,h)
		IMPLICIT NONE
		
		INTEGER, PARAMETER :: rk = SELECTED_REAL_KIND(P=12, R=40)
		REAL(KIND=rk) :: dfor
		REAL :: x, h
		dfor = (f(x+h)-f(x))/h
	
	END FUNCTION dfor

	FUNCTION dcen(x,h)
		IMPLICIT NONE
		
		INTEGER, PARAMETER :: rk = SELECTED_REAL_KIND(P=12, R=40)
		REAL(KIND=rk) :: dcen
		REAL :: x, h
		dcen = (f(x+h/2.0)-f(x-h/2.0))/h
	
	END FUNCTION dcen

	FUNCTION df(x)
		IMPLICIT NONE
		
		INTEGER, PARAMETER :: rk = SELECTED_REAL_KIND(P=12, R=40)
		REAL(KIND=rk) :: df
		REAL :: x
		df = (-2.0*x-cos(x))/((x**2+3+sin(x))**2)
	
	END FUNCTION df

	!functions for the second derivative
	FUNCTION d2cen(x,h)
		IMPLICIT NONE
		
		INTEGER, PARAMETER :: rk = SELECTED_REAL_KIND(P=12, R=40)
		REAL(KIND=rk) :: d2cen
		REAL :: x, h
		d2cen = (f(x+h) - 2.0*f(x) + f(x-h))/h**2
	
	END FUNCTION d2cen

	FUNCTION d2f(x)
		IMPLICIT NONE
		
		INTEGER, PARAMETER :: rk = SELECTED_REAL_KIND(P=12, R=40)
		REAL(KIND=rk) :: d2f
		REAL :: x
		d2f = 2.0*(2.0*x+cos(x))**2/(x**2+sin(x)+3)**3 - (2.0-sin(x))/(x**2.0+sin(x)+3.0)**2
	
	END FUNCTION d2f

	!functions for integration
	FUNCTION Integral(x)
		IMPLICIT NONE

		REAL :: Integral,x
		Integral = 1/(x**2 + 4.0*x +3)

	END FUNCTION Integral

	FUNCTION Iacctual(a,b)
		IMPLICIT NONE
		
		REAL :: a, b, Iacctual
		Iacctual = (0.5*log((b+1)/(b+3))) - (0.5*log((a+1)/(a+3)))

	END FUNCTION Iacctual


END MODULE methods


PROGRAM finite
	
	USE methods	!opening of the module we stated above

	IMPLICIT NONE

	REAL :: x,h,a,b,counter,Trapezium,Isimp,number_1,number_2
	INTEGER :: i,N,j,k

	WRITE(6,*) ''
	WRITE(6,*) 'This program finds the first and second derivative numerical approximations'	!stating what the program works to the user
	WRITE(6,*) ''	
	WRITE(6,*) 'Of the function 1/(x^2 + 3 + sin(x))'
	WRITE(6,*) ''
	WRITE(6,*) 'It also finds the integrated numerical approximation of 1/(x^2 + 4x +3)'
	WRITE(6,*) ''

	DO k=1,100
			
		WRITE(6,*) ''		
		WRITE(6,*) 'What would you like to workout?'
		WRITE(6,*) ''
		WRITE(6,*) ''
		WRITE(6,*) 'Enter 1 for first derivative calculations'	!giving the user a choice of what they want to workout in the program
		WRITE(6,*) 'Enter 2 for second derivative calculations'
		WRITE(6,*) 'Enter 3 for integration calculations'
		WRITE(6,*) 'Enter 4 to exit'
		READ(5,*)i

		!code for the first derivative
		IF (i==1) THEN
	
			WRITE(6,*) 'This program uses the Centre and Forward Difference Method'
			WRITE(6,*) ''
			WRITE(6,*) 'To find the numerical approximations to the first derivative' 
			WRITE(6,*) ''
			WRITE(6,*) 'For the function f(x) = 1/(x^2 + 3 + sin(x))'
			WRITE(6,*) ''
			WRITE(6,*) 'Input value for x'
			READ(5,*)x
			WRITE(6,*) 'Input value for h, which is the interval'	!asking the user to input values, which will evaluate the functions
			READ(5,*)h
			WRITE(6,*) ''
			WRITE(6,'(a42, f12.9)') 'Centre Difference Method = ', dcen(x,h)		!formatting the output of the values to a suitable degree
			WRITE(6,*) ''
			WRITE(6,'(a40, f12.9)') 'Forward Difference Method = ', dfor(x,h)
			WRITE(6,*) ''
			WRITE(6,'(a40, f12.9)') 'Exact Differential Value = ', df(x)
			WRITE(6,*) ''
			WRITE(6,'(a40, f12.9)') 'The error in the centre method is',abs(dcen(x,h) - df(x))
			WRITE(6,*) ''
			WRITE(6,'(a40, f12.9)') 'The error in the forward method is',abs(dfor(x,h) - df(x))	!output of the derivatives with their error
	
	

		 !code for the second derivative
		 ELSE IF (i==2) THEN

			WRITE(6,*) 'This program uses the Centre Difference Method'
			WRITE(6,*) ''
			WRITE(6,*) 'To find the numerical approximations to the second derivative'
			WRITE(6,*) ''
			WRITE(6,*) 'For the function f(x) = 1/(x^2 + 3 + sin(x))'
			WRITE(6,*) ''
			WRITE(6,*) 'Input value for x'
			READ(5,*)x
			WRITE(6,*) 'Input value for h, which is the interval'	!bounds for second derivative
			READ(5,*)h
			WRITE(6,*) ''
			WRITE(6,'(a40, f13.11)') 'Centre Difference Method = ',d2cen(x,h)
			WRITE(6,*) ''
			WRITE(6,'(a40, f13.11)') 'Exact Differential Value = ',d2f(x)
			WRITE(6,*) ''
			WRITE(6,'(a45, f13.11)') 'The error in the second derivative = ',abs(d2cen(x,h) - d2f(x)) !output of the derivative with its error
			WRITE(6,*) ''

	
		!code for integration
		ELSE IF (i==3) THEN
		
			WRITE(6,*) 'This program uses the Trapezium and Simpsons method'
			WRITE(6,*) ''
			WRITE(6,*) 'To find the approximation to numerical integrals'
			WRITE(6,*) ''
			WRITE(6,*) 'This part uses the Trapezium rule, to workout the Integral'
			WRITE(6,*) ''
			WRITE(6,*) 'For the function f(x) = 1/(x^2 + 4x + 3)'
			WRITE(6,*) ''
			WRITE(6,*) 'How many strips of the trapezium rule do you require?'
			READ(5,*)N
			WRITE(6,*) ''
			WRITE(6,*) 'Input value for a, which is the lower bound'
			READ(5,*)a
			WRITE(6,*) ''
			WRITE(6,*) 'Input value for b, which is the upper bound'	!inputting bounds for the integral, for which it will be evaluated about
			READ(5,*)b
			WRITE(6,*) ''

			h = (b-a)/N
			counter= 0

			DO j=1,(N-1)
			
				counter = counter + Integral(a+j*h)	!do loop to find the sum of the area of the strips, which is 'counter' in this case
			
			END DO
		
			Trapezium = ((h/2.0) * (Integral(a)+Integral(b))) + h*counter	!final value for the area using trapezium rule
	
		
			WRITE(6,'(a48, f12.7)') 'The approximation using the Trapezium method = ',Trapezium	!formatting the ouput
			WRITE(6,*) ''
			WRITE(6,'(a40, f12.7)') 'The actual value = ',Iacctual(a,b)	!error for the trapezium method and its approximation
			WRITE(6,*) ''
			WRITE(6,'(a40, f12.7)') 'The error in the Trapezium method = ',abs(Trapezium-Iacctual(a,b))
			WRITE(6,*) ''
			WRITE(6,*) ''
		
			WRITE(6,*) 'This part uses Simpsons method to workout the Integral'
			WRITE(6,*) ''
			WRITE(6,*) 'For the function f(x) = 1/(x^2 + 4x +3)'
			WRITE(6,*) ''
			WRITE(6,*) 'How many strips of Simpsons rule do you require?'
			READ(5,*)N
			WRITE(6,*) ''
			WRITE(6,*) 'Input value for a, which is the lower bound'
			READ(5,*)a
			WRITE(6,*) ''
			WRITE(6,*) 'Input value for b, which is the upper bound'	!entering bounds for the approximation
			READ(5,*)b
			WRITE(6,*) ''
			
			h = (b-a)/N
			number_1 = 0
			number_2 = 0

			DO j=1,(N-1),2
				number_1 = number_1 + Integral(a+j*h)
			END DO

			DO j=2,(N-2),2
				number_2 = number_2 + Integral(a+j*h)	!do loops to find the sums in the Simpson rule formula
			END DO

			Isimp = (h/3.0)*(Integral(a)+Integral(b)+4*number_1+2*number_2)


			WRITE(6,'(a50, f12.9)') 'The approximation using Simpsons method = ',Isimp	!formatting the output
			WRITE(6,*) ''
			WRITE(6,'(a40, f12.9)') 'The actual value = ',Iacctual(a,b)
			WRITE(6,*) ''
			WRITE(6,'(a40, f12.9)') 'The error in Simpsons method = ',abs(Isimp-Iacctual(a,b))		!output of the approximation with the error
			WRITE(6,*) ''

		ELSE IF (i==4) THEN 	!this statement gives the option to exit the whole program
			EXIT									
		
		
		ELSE 
			CYCLE	!constantly cycling back to the beginning of the do loop
		END IF
	
	END DO
	

END PROGRAM
