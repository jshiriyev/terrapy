PROGRAM AxialNMM_25DEM
!-----------------------------------------------------------------------
! This is the code to learn Fortran
!-----------------------------------------------------------------------
! USE GlobalVarbls
!  IMPLICIT NONE
!
!  RS = 1.0D0
!  IS = 3
!
!  CALL Indat
!  CALL PostProc
!
!  OPEN(2, FILE='RESULT.DAT')
!  WRITE(2, '(1X, F15.4)') RS
!  WRITE(2, '(1X, I15)') IS
!  CLOSE(2)
!
real :: x = 2.18
integer :: i = 5
complex :: z = (2.3, 1.14)
print *, aimag(z)
END PROGRAM AxialNMM_25DEM
