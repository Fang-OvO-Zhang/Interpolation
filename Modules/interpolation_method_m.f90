MODULE interpolation_method
  IMPLICIT NONE  
  CONTAINS
  

  SUBROUTINE polynomial_first_barycentric( data_co, nvals_co, x, y )
  ! First kind of barycentric polynomial interpolation
  ! L(i) = [x - x(1)]*...*[x - x(i-1)]*[x - x(i+1)]*...*[x - x(N)] 
  !      / [x(i) - x(1)]*...*[x(i) - x(i-1)]*[x(i) - x(i+1)]*...*[x(i) - x(N)]
  ! y = sum{L(i)y(i)}
  ! x(i) = data_co(i,1); y(i)=data_co(i,2)
    USE parameters
    USE data_and_time
    IMPLICIT NONE
    
    INTEGER(DP),INTENT(in) :: nvals_co
    REAL(DP),DIMENSION(1:nvals_co,1:2),INTENT(in) :: data_co
    REAL(DP),INTENT(in) :: x
    REAL(DP),INTENT(out) :: y
    
    !local variables
    INTEGER(DP) :: i,j
    REAL(DP),DIMENSION(:),ALLOCATABLE :: L
    
!    ALLOCATE(data_co(nvals_co,2))
    ALLOCATE(L(nvals_co))
        
    DO i = 1, nvals_co ! We need to calcualte L(i)
        L(i) = 1.0        
        DO j = 1, nvals_co
          IF (j == i) CYCLE
          L(i) = ( (x - data_co(j,1)) / (data_co(i,1) - data_co(j,1)) )*L(i)
        END DO
    END DO

    y = 0.0   
    DO i = 1, nvals_co 
      y = y + data_co(i,2)*L(i)
    END DO
    
!    WRITE(*,*) "x = ",x,"; y = ",y
    
    DEALLOCATE(L)
    
  END SUBROUTINE polynomial_first_barycentric
  
END MODULE interpolation_method