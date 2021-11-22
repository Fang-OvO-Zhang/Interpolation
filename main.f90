PROGRAM main

  use read_input
  use parameters
  use data_and_time
  use interpolation_method
  
  IMPLICIT NONE
  
  CHARACTER(len=128) :: file_co_name, file_fi_name ! The files contains data_co and data_fi
  REAL(KIND=DP),DIMENSION(:,:),ALLOCATABLE :: data_co, data_fi
  REAL(KIND=DP) :: var_begin, var_end, var_step ! Output data_fi format
  INTEGER(KIND=DP)::nvals_co,nvals_fi ! the lines contaiend in data_co and data_fi files
  
  INTEGER::i
  
  ! Read the interpolation settings from "inter.inp" file
  CALL read_settings(file_co_name,file_fi_name,var_begin,var_end,var_step)

  ! Read the lines in data_co file
  CALL read_data_co_lines(file_co_name,nvals_co)  

  ! Read the lines for data_fi file
  CALL read_data_fi_lines(var_begin, var_end, var_step, nvals_fi)

  ! Initialing data_co that contains data waiting for interpolating.
  ALLOCATE(data_co(nvals_co,2))  
  CALL read_data_co(file_co_name, nvals_co, data_co) ! Output is data_co 
  

     
  ALLOCATE(data_fi(nvals_fi,2))
!  data_fi(1:nvals_fi,1:2) = 0.0 ! initialazing the data_fi 
  
  DO i=1,nvals_fi
    data_fi(i,1) = var_begin + var_step*(i-1)
!    WRITE(*,*) data_fi(i,1)
  END DO
  
  CALL time()
  WRITE(*,*) "start interpolation: using polynomial_first_barycentric"
  WRITE(*,*)
  DO i=1, nvals_fi
  !  Currently onlu support polynomial_first_barycentric (^_^)
  !  IF ( method == 1) THEN
      CALL polynomial_first_barycentric( data_co, nvals_co, data_fi(i,1), data_fi(i,2) )
  !  ELSE IF ( method == 2) THEN
  !    CALL
  !  ELSE
  !  END IF
     CALL time()
     WRITE(*,"(1X,A8,2X,I5.5,2X,A1,2X,I5.5)") "finished:",i,"/",nvals_fi
  END DO
  
  WRITE(*,*)
  CALL time()
  WRITE(*,"(1X,A29,1X,A20)") "writing interpolated data to ",file_fi_name
  OPEN(unit=102,file=file_fi_name,STATUS="replace",ACTION="write")
    DO i=1, nvals_fi
      WRITE(102,"(F20.5,4X,F15.5)") data_fi(i,1),data_fi(i,2)
    END DO
  CLOSE(unit=102)
  
  CALL time()
  WRITE(*,*) "all finished, have a good day !"
  
END PROGRAM main