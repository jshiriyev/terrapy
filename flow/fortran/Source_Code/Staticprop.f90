SUBROUTINE Staticprop
!-----------------------------------------------------------------------
! Subroutine to create static part of transmissibility matrix
!-----------------------------------------------------------------------
  USE Global
  IMPLICIT NONE
  REAL*8, DIMENSION(N) :: xyz, yxz
  INTEGER :: I, v, u
!
  DO I = 1, N
     xyz(I) = kx(I)*dy(I)*dz(I)
     yxz(I) = ky(I)*dx(I)*dz(I)
  END DO
!
  ALLOCATE(Tx(N-Ny))
  ALLOCATE(Ty(N-Nx))
!
  DO I = 1, N-Ny
     v = Bx(I,1)
     u = Bx(I,2)
     Tx(I) = 2*xyz(v)*xyz(u)*conv/(xyz(v)*dx(u)+xyz(u)*dx(v))
  END DO
!
  DO I = 1, N-Nx
     v = By(I,1)
     u = By(I,2)
     Ty(I) = 2*yxz(v)*yxz(u)*conv/(yxz(v)*dy(u)+yxz(u)*dy(v))
  END DO
!
END SUBROUTINE Staticprop
