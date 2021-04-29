SUBROUTINE Wellprop
!-----------------------------------------------------------------------
! Subroutine to create well constants
!-----------------------------------------------------------------------
  USE Global
  IMPLICIT NONE
  REAL*8 :: yx, xy, rq
  INTEGER :: I
!
  ALLOCATE(We(Nwell))
!
  DO I = 1, Nwell
     yx = ky(gwell(I))/kx(gwell(I))
     xy = kx(gwell(I))/ky(gwell(I))
     rq = 0.28*(yx**0.5*dx(gwell(I))**2+xy**0.5*dy(gwell(I))**2)**0.5
     rq = 2*pi*dz(gwell(I))*conv/log(rq/rwell(I)/(yx**0.25+xy**0.25))
     We(I) = (kx(gwell(I))*ky(gwell(I)))**0.5*rq
  END DO
!
END SUBROUTINE Wellprop
