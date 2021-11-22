MODULE data_and_time
  IMPLICIT NONE
  
  CONTAINS
  !--------------------------First subroutine----------------------
  !To print the data and time
  SUBROUTINE time()
    IMPLICIT NONE
    
    INTEGER,DIMENSION(8)::data_time
    CHARACTER(len=10),DIMENSION(3) ::b
  
    CALL DATE_AND_TIME(b(1),b(2),b(3),data_time)
    !data_time: year, month, day,time_difference in minutes,
    !           hour, minuts, seconds, m-seconds
  
    WRITE(*,101) data_time(5),data_time(6),data_time(7)
    101 FORMAT("[",I2.2,":",I2.2,":",I2.2,"]",5X,\)

  END SUBROUTINE time
  !-----------------------------END--------------------------------
  
END MODULE data_and_time