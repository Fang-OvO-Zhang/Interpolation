MODULE read_input
  
  ! read_settings: read the input settings.
  ! read_data_co_lines: figure out how many lines in the input coarse data.
  ! read_data_co : read the input coarse data
  ! read_data_fi_lines: figure out how many lines in the output fine data.
  
  IMPLICIT NONE
  
  CONTAINS
  
  !--------------------------First subroutine----------------------
  SUBROUTINE read_settings(file_co_name,file_fi_name,var_begin,var_end,var_step)

    use parameters ! the global parameters
    use data_and_time ! print the system time

    IMPLICIT NONE  
    
    !files name contains the coarse data and fine/interpolated data
    CHARACTER(len=128),INTENT(out) :: file_co_name, file_fi_name
    !interpolated data settings
    REAL(KIND=DP),INTENT(out) :: var_begin, var_end, var_step
    
    !Here are some local variables
    INTEGER(KIND=SP) :: err_num
    CHARACTER(len=256) :: err_msg
    
    NAMELIST /control/file_co_name, file_fi_name,var_begin,var_end,var_step

    OPEN(unit=101,file="inter.inp",STATUS="old",ACTION="read",IOSTAT=err_num,IOMSG=err_msg)
      CALL time()
      WRITE(*,*) 'reading from "inter.inp"......'
!      IF (err_num /= 0) STOP err_msg ! open the input coarse data
      READ(unit=101,NML=control)
    CLOSE(unit=101)

      CALL time()
      WRITE(*,*) 'finshed reading from "inter.inp"'
      WRITE(*,201) file_co_name, file_fi_name,var_begin,var_end,var_step
      201 FORMAT(/,T12,"file_co_name",10X,"=",8X,A20,/, &
               & T12,"file_fi_name",10X,"=",8X,A20,//, &
               & T12,"var_begin",13X,"=",F20.10,/, &
               & T12,"var_end",15X,"=",F20.10,/,   &
               & T12,"var_step",14X,"=",F20.10,/)      
      
  END SUBROUTINE read_settings
  !--------------------------End of first subroutine----------------------
  
  
  !------------------------Begin of Second subroutine---------------------
  SUBROUTINE read_data_co_lines(file_co_name,nvals_co)
    
    use parameters
    use data_and_time
    
    IMPLICIT NONE
    
    CHARACTER(len=128),INTENT(in) :: file_co_name
    INTEGER(KIND=DP),INTENT(out) :: nvals_co
    
    ! Local variables
    INTEGER(KIND=SP) :: err_num
    CHARACTER(len=256) :: err_msg
    
!    CALL time()
!    WRITE(*,"(1X,A13,A15)") "reading from ", file_co_name

    OPEN(unit=101,file=file_co_name,STATUS="old",ACTION="read",IOSTAT=err_num,IOMSG=err_msg)
!      IF (err_num /= 0) STOP err_msg ! open the input coarse data
      nvals_co = 0
      DO
        READ(101,*,IOSTAT=err_num)
        IF (err_num /= 0) EXIT
        nvals_co = nvals_co + 1
      END DO

!      IF (err_num < 0) THEN
!        CALL time()
!        WRITE(*,201) nvals_co
!        201 FORMAT(1X,"There are ",I0," lines.")
!      ELSE
!        CALL time()
!        STOP "ERROR!"
!      END IF
    CLOSE(unit=101)
    
  END SUBROUTINE read_data_co_lines
  !------------------------end of Second subroutine---------------------
  
  
  
  !------------------------Begin of third subroutine---------------------
  
  SUBROUTINE read_data_fi_lines(var_begin, var_end, var_step, nvals_fi)
    use parameters
    use data_and_time
    
    IMPLICIT NONE
    
    REAL(KIND=DP),intent(in) :: var_begin, var_end, var_step
    INTEGER(KIND=DP),INTENT(out) :: nvals_fi
    REAL(KIND=DP) :: var_begin_temp 
    
    var_begin_temp = var_begin
    nvals_fi = 0
    
    IF (var_step > 0.0 .AND. var_begin <= var_end) THEN
      DO
        nvals_fi = nvals_fi + 1
        var_begin_temp = var_begin_temp + var_step
        IF (var_begin_temp - var_end > 1E-12_DP) EXIT
      END DO

    ELSE IF (var_step < 0.0 .AND. var_begin >= var_end) THEN
      DO
        nvals_fi = nvals_fi + 1
        var_begin_temp = var_begin_temp + var_step
        IF (var_begin_temp - var_end < 1E-12_DP) EXIT
      END DO

    ELSE IF (var_step > 0.0 .AND. var_begin > var_end) THEN
      CALL time()
      STOP " you choose a positive step, but var_begin > var_end, please check!"

    ELSE IF (var_step < 0.0 .AND. var_begin < var_end) THEN
      CALL time()
      STOP " you choose a negative step, but var_begin < var_end, please check!"
    ELSE
      CALL time()
      STOP " you choose a zero step, is this really you want? please check!"
    END IF
!    CALL time()
!    WRITE(*,"(1X,A10,I0,A7)") "there are ",nvals_fi, " values to be calculated"
  END SUBROUTINE read_data_fi_lines
  !------------------------end of third subroutine---------------------
  
  
  !------------------------Begin of fourth subroutine---------------------
  SUBROUTINE read_data_co(file_co_name,nvals_co,data_co)
    use parameters
    use data_and_time
    IMPLICIT NONE
    
    CHARACTER(len=128),INTENT(in) :: file_co_name
    INTEGER(KIND=DP),INTENT(in) :: nvals_co
    REAL(KIND=DP),DIMENSION(1:nvals_co,1:2),INTENT(out) :: data_co
    
    ! Local variables
    INTEGER(KIND=SP) :: err_num
    CHARACTER(len=256) :: err_msg
    INTEGER::i,j
    
 !   ALLOCATE(data_co(nvals,2))
    
    OPEN(unit=101,file=file_co_name,STATUS="old",ACTION="read",IOSTAT=err_num,IOMSG=err_msg)
      !IF (err_num /= 0) STOP err_msg ! open the input coarse data     
      READ(101,*,IOMSG=err_msg) ((data_co(i,j),j=1,2),i=1,nvals_co)
    CLOSE(unit=101)

    CALL time()
    WRITE(*,*) "the input coarse data are:"
    WRITE(*,"(T25,A3,T45,A3)") "X =","Y ="
    DO i=1,nvals_co
      WRITE(*,"(T12,2F20.10)") (data_co(i,j),j=1,2)
    END DO
    
  END SUBROUTINE read_data_co
  !------------------------End of fourth subroutine---------------------
  
END MODULE read_input