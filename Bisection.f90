MODULE precision
	IMPLICIT NONE
	
	 ! Precision of at least 15 decimal digits
	 ! and exponent range of 10^-30 to 10^30 for floating_point numbers.
	 ! Range of 10^-30 to 10^30 for integer numbers.
	 
	 INTEGER, PARAMETER :: pr = SELECTED_REAL_KIND(P=15, R=30), &
	 											 int= SELECTED_INT_KIND(10)
END MODULE precision
	 	
PROGRAM find_root
	USE precision ; IMPLICIT NONE 
	
	! This program finds a root of an equation f(x)=0 with a specified
	! tolerance of the true root value using bisection method.
	
	! Input variables
	REAL(KIND=pr), EXTERNAL :: f
	REAL(KIND=pr) left, right, tolerance
	INTEGER(KIND=int) maximum_iterations
	
	! Other variables
	REAL(pr) :: Answer, delta
	INTEGER (int) :: number_of_bisections, err, ios
	
	! Data Entery
	Data_Entery: DO 
		WRITE(*,*) "Give two values for the root interval: "
		READ(*,*) left, right
		PRINT*, "Give the tolerance: "
		READ*, tolerance
		PRINT*, "Give the maximum number of iterations allowed: "
		READ*, maximum_iterations
	
		! Start calculations
		CALL bisection_method (f, left, right, tolerance, 		  &
													 maximum_iterations, Answer, delta, &
													 number_of_bisections, err)		
	
		! Results
		SELECT CASE (err)
		CASE (0)
			PRINT*, "The root is: ", Answer, "+- ", delta
			PRINT*, "obtained after ", number_of_bisections-1, " iterations"
			EXIT 
		CASE (-1)
			PRINT*, "The input data is bad, renter data"
		CASE (-2)
			PRINT*, "The maximum number of iterations has been exceeded"
			PRINT*, "The root value being considered in last iteration is: ", &
							 Answer
		END SELECT
	END DO Data_Entery
	
	OPEN(UNIT=2, FILE='Results.txt', STATUS='UNKNOWN', ACTION='WRITE', &
			 POSITION='APPEND', IOSTAT=ios)
	IF (ios .NE. 0_int) THEN
		PRINT*, "Output file for results could not be opened !!!"
	END IF
	WRITE(UNIT=2, FMT=100) "The root is: ", Answer, delta, "obtained after ", number_of_bisections-1, " iterations"

100 FORMAT (A, F12.6, " +-", E14.7 / A, I3, A//)

END PROGRAM find_root	

SUBROUTINE bisection_method	(f, xl_start, xr_start, tolerance, &
                             max_iterations, Answer, delta, num_bisecs, error)	
                             
	USE precision ; IMPLICIT NONE
  
  ! This subroutine is used for bisection method calculations     
  
  ! Dummy arguments
  REAL(pr), EXTERNAL :: f
  REAL(pr), INTENT(IN) :: xl_start, xr_start, tolerance
  INTEGER(int), INTENT(IN) :: max_iterations
  REAL(pr), INTENT(OUT):: Answer, delta
  INTEGER(int), INTENT(OUT) :: num_bisecs, error
         			
	! Interanl variables
	REAL(pr) x_left, x_mid, x_right, y_left, y_mid, y_right
	
	! Adjusting input values to be used for interval	
	IF (xl_start .LT. xr_start) THEN
		x_left  = xl_start
		x_right = xr_start
	ELSE IF (xl_start .GT. xr_start) THEN
		x_left  = xr_start
		x_right = xl_start	
	ELSE
		error = -1_int
		RETURN
	END IF
	
	y_left  = f(x_left)
	y_right = f(x_right)
	
	! Testing validity
	IF (y_left * y_right .GT.	0._pr .OR. tolerance .LT. 0._pr &
			.OR. max_iterations .LT. 1_pr) THEN	 
			error = -1_int
			RETURN
	END IF
	
	DO num_bisecs=1_int, max_iterations, 1_int
		delta = 0.5_pr*(x_right-x_left)
		x_mid = x_left + delta
		! Conversion check
		IF (delta < tolerance) THEN
			error = 0_int
			Answer = x_mid
			RETURN
		END IF
		y_mid = f(x_mid)
		
		PRINT 200, num_bisecs, x_left, x_mid, x_right, y_mid
		
		IF (y_left*y_mid .LT. 0._pr) THEN
			x_right = x_mid
		ELSE
			x_left = x_mid
			y_left = y_mid
		END IF
	END DO
	
	! Exceeding maximum iterations
	error = -2
	Answer = x_mid
	RETURN

200 FORMAT ("Iteration", I3, TR4, 3F12.6, "     (", F10.6, ")" )

END SUBROUTINE bisection_method	

REAL(pr) FUNCTION f (x)
	USE precision ; IMPLICIT NONE
	
	! Dummy variables
	REAL(pr), INTENT(IN) :: x
	
	f = x**2_int - 2_pr
END FUNCTION f
