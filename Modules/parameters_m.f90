MODULE parameters
  IMPLICIT NONE
  
  INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
  INTEGER,PARAMETER :: SP = SELECTED_REAL_KIND(p=6)
  REAL(KIND=DP),PARAMETER :: PI = 3.14159265358979323846_DP
  
END MODULE parameters