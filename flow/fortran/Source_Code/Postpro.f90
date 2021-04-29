SUBROUTINE Postpro
!-----------------------------------------------------------------------
! Subroutine to read input data
!-----------------------------------------------------------------------
  USE Global
  IMPLICIT NONE
  INTEGER :: I
!
  OPEN(2, FILE='RESULT.DAT')
!
    WRITE(2,*) ' Pres, psi       Sw, ratio'
!
    DO I = 1, Nwell
	WRITE(2,*) P(I), Sw(I)
    END DO

!    WRITE(2,*) xwell
!    WRITE(2,*) ywell
!    WRITE(2,*) rwell
!    WRITE(2,*) qo
!    WRITE(2,*) qw
!    WRITE(2,*) qp
!    WRITE(2,*) bhp
!  WRITE(2,*) ' Run Time   # of Grids'
!  WRITE(2, '(2X, F8.6, 2X, I11)') T2 - T1, N
!
!  WRITE(2,*) 'West and East Boundaries'
!
!    DO I = 1, N-Ny
!	WRITE(2, '(1X, I5, 1X, I5)') Bx(I,:)
!    END DO
!
!  WRITE(2,*) ''
!  WRITE(2,*) 'South and North Boundaries'
!
!    DO I = 1, N-Nx
!	WRITE(2, '(1X, I5, 1X, I5)') By(I,:)
!    END DO
!
  CLOSE(2)
!
END SUBROUTINE Postpro
