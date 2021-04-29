SUBROUTINE Indat
!-----------------------------------------------------------------------
! Subroutine to read input data
!-----------------------------------------------------------------------
  USE Global
  IMPLICIT NONE
!
  OPEN(1, FILE='GRID.WGL', STATUS='OLD')
!
     READ(1,*) Nx
     READ(1,*) Ny
     READ(1,*) Nz
!
     N = Nx*Ny*Nz
!
     READ(1,*) t
     READ(1,*) dt
!
     ALLOCATE(dx(N))
     ALLOCATE(dy(N))
     ALLOCATE(dz(N))
     ALLOCATE(dp(N))
     ALLOCATE(pr(N))
     ALLOCATE(kx(N))
     ALLOCATE(ky(N))
     ALLOCATE(P(N))
     ALLOCATE(Sw(N))
!
     CALL Cvector(1,dx)
     CALL Cvector(1,dy)
     CALL Cvector(1,dz)
     CALL Cvector(1,dp)
     CALL Cvector(1,pr)
     CALL Cvector(1,kx)
     CALL Cvector(1,ky)
!
  CLOSE(1)
!
  OPEN(2, FILE='COMPONENTS.WGL', STATUS='OLD')
!
     READ(2,*) Swcrit
     READ(2,*) Soirw
     READ(2,*) korw
     READ(2,*) Nw
     READ(2,*) Swcon
     READ(2,*) Sorw
     READ(2,*) koro
     READ(2,*) No
!
  CLOSE(2)
!
  OPEN(3, FILE='FLUID.WGL', STATUS='OLD')
!
     READ(3,*) rho_o
     READ(3,*) rho_w
     READ(3,*) mu_o
     READ(3,*) mu_w
     READ(3,*) Bo
     READ(3,*) Bw
     READ(3,*) Co
     READ(3,*) Cw
     READ(3,*) Cr
!
  CLOSE(3)
!
  OPEN(4, FILE='INITIAL.WGL', STATUS='OLD')
!
     CALL Cvector(4,P)
     CALL Cvector(4,Sw)
!
  CLOSE(4)
!
  OPEN(5, FILE='WELLS.WGL', STATUS='OLD')
!
     READ(5,*) Nwell
!
	ALLOCATE(xwell(Nwell))
	ALLOCATE(ywell(Nwell))
	ALLOCATE(rwell(Nwell))
	ALLOCATE(gwell(Nwell))
	ALLOCATE(qo(Nwell))
	ALLOCATE(qw(Nwell))
	ALLOCATE(qp(Nwell))
	ALLOCATE(bhp(Nwell))
!
     gwell(1) = 1
     gwell(2) = 5
     gwell(3) = 9
!
     READ(5,*) xwell(1:Nwell)
     READ(5,*) ywell(1:Nwell)
     READ(5,*) rwell(1:Nwell)
     READ(5,*) qo(1:Nwell)
     READ(5,*) qw(1:Nwell)
     READ(5,*) qp(1:Nwell)
     READ(5,*) bhp(1:Nwell)
!
  CLOSE(5)
!
END SUBROUTINE Indat
!
!
SUBROUTINE Cvector(a,Vn)
  USE Global
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: a
  REAL*8, DIMENSION(N), INTENT(OUT) :: Vn
  REAL*8, SAVE :: V
  INTEGER :: I
!
  READ(a,*) V
!
     DO I = 1, N
	Vn(I) = V
     END DO
!
END SUBROUTINE Cvector
