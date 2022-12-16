
!this program uses matrices to workout translation and rotation
!for given vectors and writes them to a file for gnuplot to visualise

PROGRAM matrix

	IMPLICIT NONE

		
	REAL, DIMENSION(1:3,1:3) :: R,T,G,H
	REAL, DIMENSION(1:3) :: A,B,C,D,E,F
	REAL :: o, x_A, y_A, x_B, y_B, x_C, y_C, x, y, x_com, y_com
	INTEGER :: k,i

	WRITE(6,*) ''
	WRITE(6,*) 'This program works out the rotation and translation of triangles'
	WRITE(6,*) 'From the user specified initial triangle'
	WRITE(6,*) ''
	WRITE(6,*) 'What are the co-ordinates for your triangle?'
	WRITE(6,*) ''
	
	
	!first vector co-ordinates
	WRITE(6,*) 'For the first point:'
	WRITE(6,*) ''
	WRITE(6,*) 'Enter the x co-ordinate for your vector'
	READ(5,*)x_A
	WRITE(6,*) 'Enter the y co-ordinate for your vector'
	READ(5,*)y_A
	WRITE(6,*) ''
	
	A(1) = (x_A) ; A(2) = (y_A) ; A(3) = 1.0

	
	!second vector co-ordinates
	WRITE(6,*) 'For the second point:'
	WRITE(6,*) ''
	WRITE(6,*) 'Enter the x co-ordinate for your vector'
	READ(5,*)x_B
	WRITE(6,*) 'Enter the y co-ordinate for your vector'
	READ(5,*)y_B
	WRITE(6,*) ''
	
	B(1) = (x_B) ; B(2) = (y_B) ; B(3) = 1.0
	
	
	!third vector co-ordinates
	WRITE(6,*) 'For the third point:'
	WRITE(6,*) ''
	WRITE(6,*) 'Enter the x co-ordinate for your vector'
	READ(5,*)x_C
	WRITE(6,*) 'Enter the y co-ordinate for your vector'
	READ(5,*)y_C
	WRITE(6,*) ''
	
	C(1) = (x_C) ; C(2) = (y_C) ; C(3) = 1.0

	

	

	DO k=1,100	!fail safe here just incase the code starts to constantly cylce infinitly

		
		!menu for the user to decide what they want to do to their matrix
		WRITE(6,*) ''
		WRITE(6,*) ''
		WRITE(6,*) ''
		WRITE(6,*) 'What would you like to do?'
		WRITE(6,*) ''
		WRITE(6,*) 'Enter 1 for rotation calculations (step 1)'
		WRITE(6,*) 'Enter 2 for translation calculations (step 2)'
		WRITE(6,*) 'Enter 3 for both translation and rotation calculations (step 3)'
		WRITE(6,*) 'Enter 4 to exit'
		READ(5,*)i

		IF (i==1) THEN	
			
			!code for the rotation
			WRITE(6,*) ''
			WRITE(6,*) 'This part of the program asks you to input the co-ordinates'
			WRITE(6,*) 'for a triangle and rotates it by your given angle'
			WRITE(6,*) ''
			WRITE(6,*) ''
	
			!for the angle of rotation
			WRITE(6,*) 'Enter the angle you want to rotate your triangle by in radians'
			READ(5,*)o
			WRITE(6,*)''

			R(1,1) = cos(o) ; R(1,2) = -sin(o) ; R(1,3) = 0.0
			R(2,1) = sin(o) ; R(2,2) = cos(o) ; R(2,3) = 0.0
			R(3,1) = 0.0 ; R(3,2) = 0.0 ; R(3,3) = 1.0 
	
			D = MATMUL(R,A)			!using the matmul function to multiply the matrices
			E = MATMUL(R,B)
			F = MATMUL(R,C)
	
			WRITE(6,*) 'The rotated co-ordinates are'
			WRITE(6,*) ''
			WRITE(6,*) '	x	  y   		z'
			WRITE(6,*) D
			WRITE(6,*) E
			WRITE(6,*) F
	
			OPEN(unit=20,file='rotate.dat')		!creating a file which can be used to show the rotated triangles in gnuplot
			WRITE(20,*) D, A			!writing the x and y co-ordinates of the rotated and initial triangles
			WRITE(20,*) E, B
			WRITE(20,*) F, C
			WRITE(20,*) D, A

			!instructions to the user about how to display the triangle on gnuplot
			WRITE(6,*) ''
			WRITE(6,*) 'You can see this rotation on gnuplot by typing:'
			WRITE(6,*) '"gnuplot" in termial'
			WRITE(6,*) 'plot "rotate.dat" using 4:5 with lines (initial triangle)'
			WRITE(6,*) '"rotate.dat" using 1:2 with lines (rotated triangle)' 
	
		
		ELSE IF (i==2) THEN

			!code for the translation
			WRITE(6,*) ''
			WRITE(6,*) 'This part of the program translates a specicied user triangle'
			WRITE(6,*) 'for a given triangle and translational vector'
			WRITE(6,*) ''

			!the translational vector values
			WRITE(6,*) 'How much do you want to translate on the x-axis'
			READ(5,*)x
			WRITE(6,*) ''
			WRITE(6,*) 'How much do you want to translate on the y-axis'	!the displacement of the vector in x and y direction
			READ(5,*)y
			WRITE(6,*) ''

			T(1,1) = 1.0 ; T(1,2) = 0.0 ; T(1,3) = x	!setting the matrix for translation
			T(2,1) = 0.0 ; T(2,2) = 1.0 ; T(2,3) = y
			T(3,1) = 0.0 ; T(3,2) = 0.0 ; T(3,3) = 1.0

			D = MATMUL(T,A)			!using the matmul function to multiply the matrices
			E = MATMUL(T,B)
			F = MATMUL(T,C)

			WRITE(6,*) 'The translated co-ordinates are'
			WRITE(6,*) ''
			WRITE(6,*) '	x	  y   		z'
			WRITE(6,*) D
			WRITE(6,*) E
			WRITE(6,*) F
	
			OPEN(unit=20,file='translate.dat')
			WRITE(20,*) D, A	!writing the x and y co-ordinates of the translated and initial triangles to a file for gnuplot
			WRITE(20,*) E, B
			WRITE(20,*) F, C
			WRITE(20,*) D, A

			!instructions to the user about how to display the triangles on gnuplot
			WRITE(6,*) ''
			WRITE(6,*) 'You can see this translation on gnuplot by typing:'
			WRITE(6,*) '"gnuplot" in termial'
			WRITE(6,*) 'plot "translate.dat" using 4:5 with lines (initial triangle)'
			WRITE(6,*) '"translate.dat" using 1:2 with lines (translated triangle)' 

		
		ELSE IF (i==3) THEN

			!code for rotation and translation about the centre of mass
			WRITE(6,*) ''
			WRITE(6,*) 'This part of the program translates, rotates and then translates a triangle'
			WRITE(6,*) 'about its centre of mass'
			WRITE(6,*) ''

			x_com = (x_A+x_B+x_C)/3.0	!working out the centre of mass for the triangle from the given points
			y_com = (y_A+y_B+y_C)/3.0

			WRITE(6,'(a60, f6.3)') 'The centre of mass for your triangle is   x =',x_com, 'y =',y_com

			G(1,1) = 1.0 ; G(1,2) = 0.0 ; G(1,3) = (-5.0/3.0)	!setting the matrix for translation to the origin for the centre of mass
			G(2,1) = 0.0 ; G(2,2) = 1.0 ; G(2,3) = (-4.0/3.0)
			G(3,1) = 0.0 ; G(3,2) = 0.0 ; G(3,3) = 1.0

			D = MATMUL(G,A)			!using the matmul function to multiply the matrices
			E = MATMUL(G,B)
			F = MATMUL(G,C)

			WRITE(6,*) ''
			WRITE(6,*) 'The translated co-ordinates for the centre of mass to be on the origin are'
			WRITE(6,*) ''
			WRITE(6,*) '	x	  y   		z'
			WRITE(6,*) D
			WRITE(6,*) E
			WRITE(6,*) F
			
			!code for the rotation of the triangle about the origin
			WRITE(6,*) ''
			WRITE(6,*) 'Enter the angle you want to rotate this triangle by on the origin in radians'
			READ(5,*)o
			WRITE(6,*)''

			R(1,1) = cos(o) ; R(1,2) = -sin(o) ; R(1,3) = 0.0	!matrix for rotation
			R(2,1) = sin(o) ; R(2,2) = cos(o) ; R(2,3) = 0.0
			R(3,1) = 0.0 ; R(3,2) = 0.0 ; R(3,3) = 1.0 
	
			D = MATMUL(R,D)			!using the matmul function to multiply the matrices
			E = MATMUL(R,E)
			F = MATMUL(R,F)
	
			WRITE(6,*) 'The rotated co-ordinates are'
			WRITE(6,*) ''
			WRITE(6,*) '	x	  y   		z'
			WRITE(6,*) D
			WRITE(6,*) E
			WRITE(6,*) F

			WRITE(6,*) ''			
			WRITE(6,*) ''
			WRITE(6,*) 'Translating the rotated triangle back to its original centre of mass co-ordinates'
			WRITE(6,*) ''

			H(1,1) = 1.0 ; H(1,2) = 0.0 ; H(1,3) = (5.0/3.0)	!setting the matrix for translation to the back to the centre of mass
			H(2,1) = 0.0 ; H(2,2) = 1.0 ; H(2,3) = (4.0/3.0)
			H(3,1) = 0.0 ; H(3,2) = 0.0 ; H(3,3) = 1.0

			D = MATMUL(H,D)			!using the matmul function to multiply the matrices
			E = MATMUL(H,E)
			F = MATMUL(H,F)
			
			WRITE(6,*) ''
			WRITE(6,*) 'The final co-ordinates after the rotation and translation transformations are'
			WRITE(6,*) ''
			WRITE(6,*) '	x	  y   		z'
			WRITE(6,*) D
			WRITE(6,*) E
			WRITE(6,*) F

			OPEN(unit=20,file='A4.dat')
			WRITE(20,*) D, A			!writing the file for the png for a gnuplot script
			WRITE(20,*) E, B
			WRITE(20,*) F, C
			WRITE(20,*) D, A

			WRITE(6,*) ''
			WRITE(6,*) 'You can view the initial and new triangle on gnuplot'
			WRITE(6,*) 'Ive attached a separate gedit document on instructions on how to do so'	!instructions for the png file access
			WRITE(6,*) 'The document is titled "6513114-GNUPLOT_SCRIPT"'


		ELSE IF (i==4) THEN	!option to exit the whole program 
			EXIT

	
		ELSE
			CYCLE	!this forms a cycle and sends the user back to the menu to decide what they want to do
		END IF
	
	
	END DO



END PROGRAM
