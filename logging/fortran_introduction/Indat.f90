SUBROUTINE Indat
!-----------------------------------------------------------------------
! Subroutine to read input data
!-----------------------------------------------------------------------
  USE GlobalVarbls
  IMPLICIT NONE
!
  OPEN(UNIT, FILE='INDATA.WGL', STATUS='OLD')
     READ(UNIT, *) RS
     READ(UNIT, *) IS
!
  CLOSE(UNIT)
!
END SUBROUTINE Indat
