PROGRAM diffusion

  IMPLICIT NONE

  !setting boundaries and constants
  INTEGER, PARAMETER :: n = 300, t_start = 0, t_end = 10000
  REAL :: temp_old(-n:n), h, dtempdt(-n:n), diffc, time(t_start:t_end), dt, FWHM(t_start:t_end)
  REAL :: temp_new(-n:n), x_position(-n:n), HM
  INTEGER :: i, t, i_pos, i_neg

  h = 0.1
  temp_old = 0.0
  dtempdt = 0.0
  diffc = 1.1
  dt = 10E-4
 
  !opening the files for the numerous plots this code produces
  OPEN(20, file='3D.dat')
  OPEN(19, file='diffusion.dat')
  OPEN(18, file='FWHM.dat')
 
 !setting the initial conditions for the temperature profile
  DO i = -10,10
    temp_old(i) = 20.0
  END DO

 !inputting the position values from -300 to 300
  DO i = -n,n
    x_position(i) = i
  END DO
  
  !looping for time from 0 to 10 seconds in 10-4 s intervals
  DO t = t_start, t_end
    time(t) = real(t) * dt
    
    
  !--------loops to calculate the temperature profile over time——————!
  DO i = -n, n
    dtempdt(i) = diffc * (temp_old(i+1) - 2.0 * temp_old(i) +
    temp_old(i-1))/(h**2)
  END DO
  
  DO i = -n,n
    temp_new(i) = temp_old(i) + dt * dtempdt(i)
  END DO
 
!-------loops to calculate the FWHM as a function of time——————————!

  HM = temp_new(0)/2.0
  DO i = 1, n
  
  IF(temp_new(i) > HM) THEN i_pos = i
    END IF
    
  END DO
  
  DO i = -n,-1
  
  IF(temp_new(i) < HM) THEN i_neg = i
    END IF
    
  WRITE(20,*) x_position(i), time(t), temp_new(i)
  
  END DO
  
   !——————loop for temperature profile changes with time, 3D graph———-——!
   
  DO i = -n,n
 
    WRITE(20,*) '' !line break to avoid gnuplot re-graphing
    FWHM(t) = real(i_pos + abs(i_neg) ) * h
    temp_old = temp_new
 
  END DO
 
!writing the final temperature profile to a file

  DO i = -n,n
  
    WRITE(19,'(2f20.10)') x_position(i), temp_new(i)
    WRITE(18,*) time(t), FWHM(t) 
    
  END DO
  
  
END PROGRAM
