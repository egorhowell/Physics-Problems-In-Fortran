
!This program evaluates two ODE's and writes the results to a file
!It solves the equations dx/dt = v and dv/dt = -5x + 0.5cos(2t)
!It uses the modified euler method with a time step of h = 0.005 



PROGRAM Assignment_1 
	IMPLICIT NONE

	REAL, DIMENSION(1:10001):: t, x, v					!10,001 points to be printed to the file
	REAL :: h, dxdt, dvdt, v_mid, dxdt_mid, dvdt_mid, x_mid, t_mid
	INTEGER :: npts, i

	!Setting the Boundary Conditions of the ODE's
	v(1) = 2.0
	x(1) = 0.0
	npts = 10000
	h = 0.005
	t(1)=0.0
	
	OPEN(unit=20, file='assignment.dat')

	WRITE(20, *) "#   TIME     POSITION   VELOCITY"
	
	!Do loop for the iterative modified euler method
	DO i=1,npts

		!Working out the midpoints for the modified euler method
		t_mid = t(i) + 0.5*h
		
		dxdt = v(i)
		dvdt = -5.0*x(i) + 0.5*cos(2.0*(t(i)))
		x_mid = x(i) + 0.5*h*dxdt
                v_mid = v(i) + 0.5*h*dvdt
		
                dxdt_mid = v_mid
		dvdt_mid = -5.0*x_mid + 0.5*cos(2.0*(t_mid))
		
		!Outputting the values with the new approximated mid values
		t(i+1) = t(i) + h
		x(i+1) = x(i) + h*dxdt_mid			
		v(i+1) = v(i) + h*dvdt_mid

		!Outputting to a file, with the values formatted
		WRITE(20, '(3f10.3)') t(i), x(i), v(i)

	END DO
	
	!printing to the screen what the program has done
	WRITE(6,*) ''
	WRITE(6,*) 'The program has written to the file "assignment.dat"'
	WRITE(6,*) 'with three columns t, x and v which can be plotted on gnuplot'
	WRITE(6,*) ''

END PROGRAM
		
