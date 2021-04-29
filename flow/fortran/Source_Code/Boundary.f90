SUBROUTINE Boundary
!-----------------------------------------------------------------------
! Subroutine to create boundary numbers
!-----------------------------------------------------------------------
  USE Global
  IMPLICIT NONE
  INTEGER :: I, J, K, Q = 1
!
     ALLOCATE(Bx(N,2))
     ALLOCATE(By(N,2))
!
  DO J = 1, Ny
     DO I = 1, Nx-1
        K = Nx*(J-1)+I
        Bx(Q,1) = K+1
        Bx(Q,2) = K
        Q = Q+1
     END DO
  END DO
!
  DO I = 1, N-Nx
	By(I,1) = I+Nx
	By(I,2) = I
  END DO
!
END SUBROUTINE Boundary
