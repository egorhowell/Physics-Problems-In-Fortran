!------------------------------------------------
!PHY1038 Assignment 5 - Linear Algebra
!URN 6513114 - CPTL Lab Group 2F
!April 23th 2018
!------------------------------------------------


!this program finds the determinant, cofactors and inverse of a 3x3 matrix
!and solves linear simultaneous equations



!STEP 2
!function for the determinant of a 3x3 matrix
FUNCTION dmt(A)
	IMPLICIT NONE

	REAL :: dmt, dmt_1, dmt_2, dmt_3
	REAL,DIMENSION(1:3,1:3),INTENT(IN) :: A

	dmt_1 = A(1,1)*((A(2,2)*A(3,3))-(A(2,3)*A(3,2)))
	dmt_2 = -A(1,2)*((A(2,1)*A(3,3))-(A(3,1)*A(2,3)))
	dmt_3 = A(1,3)*((A(2,1)*A(3,2))-(A(3,1)*A(2,2)))
	
	dmt = dmt_1 + dmt_2 + dmt_3

END FUNCTION dmt


!STEP 3
!subroutine for the cofactor matrix
SUBROUTINE C(A,cofactor)
	IMPLICIT NONE

	REAL,DIMENSION(1:3,1:3),INTENT(IN) :: A
	REAL,DIMENSION(1:3,1:3),INTENT(OUT) :: cofactor
	REAL :: C11,C12,C13,C21,C22,C23,C31,C32,C33
	
	C11 = ((A(2,2)*A(3,3))-(A(2,3)*A(3,2)))		!working out the points for the cofactor matrix
	C12 = -((A(2,1)*A(3,3))-(A(3,1)*A(2,3)))
	C13 = ((A(2,1)*A(3,2))-(A(3,1)*A(2,2)))
	C21 = -((A(1,2)*A(3,3))-(A(3,2)*A(1,3)))
	C22 = ((A(1,1)*A(3,3))-(A(1,3)*A(3,1)))
	C23 = -((A(1,1)*A(3,2))-(A(3,1)*A(1,2)))
	C31 = ((A(1,2)*A(2,3))-(A(2,2)*A(1,3)))
	C32 = -((A(1,1)*A(2,3))-(A(1,3)*A(2,1)))
	C33 = ((A(1,1)*A(2,2))-(A(2,1)*A(1,2)))

	cofactor(1,1) = C11 ; cofactor(1,2) = C12 ; cofactor(1,3) = C13		!listing the new cofactor matrix
	cofactor(2,1) = C21 ; cofactor(2,2) = C22 ; cofactor(2,3) = C23
	cofactor(3,1) = C31 ; cofactor(3,2) = C32 ; cofactor(3,3) = C33

END SUBROUTINE C




PROGRAM algebra
	IMPLICIT NONE

	
	!implementing the determinant function
	INTERFACE
		FUNCTION dmt(A)
			REAL,DIMENSION(1:3,1:3),INTENT(IN) :: A
		END FUNCTION dmt
	END INTERFACE

	
	!implementing the subroutine for the cofactor matrix
	INTERFACE
		SUBROUTINE C(A,cofactor)
			REAL,DIMENSION(1:3,1:3),INTENT(IN) :: A
			REAL,DIMENSION(1:3,1:3),INTENT(OUT) :: cofactor
		END SUBROUTINE C
	END INTERFACE
			

	REAL,DIMENSION(1:3,1:3) :: A, cofactor, inverse_A, I
	REAL,DIMENSION(1:3) :: V, S

	
	A(1,1) = 2.0 ; A(1,2) = 1.0 ; A(1,3) = -1.0	!setting up the matrix for the program
	A(2,1) = 1.0 ; A(2,2) = 2.0 ; A(2,3) = 1.0
	A(3,1) = -1.0 ; A(3,2) = 3.0 ; A(3,3) = 2.0

	
	!calling the subroutine for the cofactor matrix to use later in the program
	CALL C(A,cofactor)

	
	!STEP 1
	!writing the matrix A to a file and transposing it to avoid the column major preference
	OPEN(unit=20, file='matrix.dat')

	WRITE(20,'(3f6.1)') TRANSPOSE(A) 	

	
	!stating what the program does to the user
	WRITE(6,*) ''
	WRITE(6,*) 'This program is about linear algebra and uses matrices to solve equations'
	WRITE(6,*) ''
	WRITE(6,*) 'We will be doing calculations with the following matrix'
	WRITE(6,*) ''
	WRITE(6,'(3f6.1)') TRANSPOSE(A)		!transposing the matrix to avoid column major preference
	

	!STEP 2
	!writing the determinant to the screen
	WRITE(6,*) ''
	WRITE(6,*) ''
	WRITE(6,*) 'The determinant for the 3x3 matrix is:'
	WRITE(6,*) ''	
	WRITE(6,'(f6.2)') dmt(A)


	!STEP 3
	!writing out the cofactor matrix
	WRITE(6,*) ''
	WRITE(6,*) ''
	WRITE(6,*) 'The cofactor matrix for the matrix is:'
	WRITE(6,*) ''
	WRITE(6,'(3f6.1)') TRANSPOSE(cofactor)		!transposing the matrix to avoid column major preference


	!STEP 4
	!working out the inverse of the matrix A
	WRITE(6,*) ''
	WRITE(6,*) ''
	WRITE(6,*) 'Using the cofactor matrix and the determinant'
	WRITE(6,*) 'we can work out the inverse of the original matrix'
	WRITE(6,*) ''
	WRITE(6,*) 'The inverse is:'
	WRITE(6,*) ''
	
	inverse_A = (1.0/dmt(A)) * cofactor

	WRITE(6,'(3f6.1)') inverse_A	!this formatting allows a 3x3 matrix to be shown

	
	!proving its the inverse
	WRITE(6,*) ''
	WRITE(6,*) ''
	WRITE(6,*) 'We can check that its the inverse by multiplying the original'
	WRITE(6,*) 'matrix with the inverse, this should give us the identity matrix:'
	WRITE(6,*) ''
	
	I = MATMUL(inverse_A,TRANSPOSE(A))	!the matmul function here multiplies matrices together
	
	WRITE(6,'(3f6.1)') I	!this gives the identity matrix, shows we have correctly found the inverse


	!STEP 5
	!solving the 3 simultaneous equations
	WRITE(6,*) ''
	WRITE(6,*) ''
	WRITE(6,*) 'We can solve simultaneous equations by applying the inverse to both'
	WRITE(6,*) 'sides, if we put the equations in matrix form'
	WRITE(6,*) ''
	WRITE(6,*) 'The equations we are trying to solve are:'	!telling the user which equations we are going to solve
	WRITE(6,*) ''
	WRITE(6,*) '	2x_1 + x_2 -x_3 = 0'
	WRITE(6,*) '	x_1 + 2x_2 + x_3 = 3'
	WRITE(6,*) '	-x_1 + 3x_2 + 2x_3 =1'
	WRITE(6,*) ''
	WRITE(6,*) 'Setting 0,3 and 1 above into a 1x3 matrix and'
	WRITE(6,*) 'multiplying it by the inverse, we get the solutions:'
	WRITE(6,*) ''

	V(1) = 0.0 ; V(2) = 3.0 ; V(3) = 1.0	!setting the matrix for the solutions

	S = MATMUL(V,inverse_A)		!mulitplying the inverse by the newly formed matrix to obtain the solutions of the equations
	
	WRITE(6,*) '  x_1   x_2    x_3'
	WRITE(6,'(3f6.1)') S
	WRITE(6,*) ''



END PROGRAM algebra






	
