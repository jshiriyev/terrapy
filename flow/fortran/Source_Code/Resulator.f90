PROGRAM Resulator
!-----------------------------------------------------------------------
! This is the simple 2 phase reservoir simulator
!-----------------------------------------------------------------------
  USE Global
  IMPLICIT NONE
!  INTEGER :: I
  REAL*8 :: T1, T2
!
  CALL CPU_TIME(T1)
!
  CALL Indat
  CALL Boundary
  CALL Staticprop
  CALL Wellprop
  CALL Relperm
  CALL Postpro
!
  CALL CPU_TIME(T2)
!
  WRITE(*,*) T2-T1
!
END PROGRAM Resulator
