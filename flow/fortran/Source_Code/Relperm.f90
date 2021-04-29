SUBROUTINE Relperm
!-----------------------------------------------------------------------
! Subroutine to create vector of relative permeability for the reservoir
!-----------------------------------------------------------------------
  USE Global
  IMPLICIT NONE
  INTEGER :: I
!
  DO I = 1, N
     IF(Sw(I) > (1-Soirw)) THEN
	Sw(I) = 1-Soirw
     END IF
  END DO
!
  ALLOCATE(kro(N))
  ALLOCATE(krw(N))
!
  DO I = 1, N
     kro(I) = koro*((1-Sw(I)-Sorw)/(1-Swcon-Sorw))**No
     krw(I) = korw*((Sw(I)-Swcrit)/(1-Swcrit-Soirw))**Nw
  END DO
!
END SUBROUTINE Relperm
