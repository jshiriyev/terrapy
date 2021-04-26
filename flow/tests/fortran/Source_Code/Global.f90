MODULE Global
!-----------------------------------------------------------------------
! conv: bbl to ft3, smt like that????????
! pi: pi number
!-----------------------------------------------------------------------
  REAL*8, PARAMETER :: conv = 6.33d-3, pi = 3.141592653589793
!-----------------------------------------------------------------------
! NUMERICAL INPUT
!  t: simulation time, days
! dt: time step, day
! Nx: number of grids in x direction
! Ny: number of grids in y direction
! Nz: number of grids in z direction
!  N: total number of grids
!-----------------------------------------------------------------------
  REAL*8, SAVE :: t, dt
  INTEGER, SAVE :: Nx, Ny, Nz, N
!-----------------------------------------------------------------------
! RESERVOIR KNOWLEDGE
! Bx: defines grid numbers not overlapping east and west boundaries
! By: defines grid numbers not overlapping south and north boundaries
! Tx: static part of transmissibility factor in x direction
! Ty: static part of transmissibility factor in y direction
! Lx: lenght of reservoir in x direction
! Ly: length of reservoir in y direction
! Lz: length of reservoir in z direction
! dx: size of grid in x direction, ft
! dy: size of grid in y direction, ft
! dz: size of grid in z direction, ft
! dp: depth of grid from the surface, ft
! pr: porosity of grid
! kx: permeability in x direction, md
! ky: permeability in y direction, md
!-----------------------------------------------------------------------
  INTEGER, DIMENSION(:,:), ALLOCATABLE, SAVE :: Bx, By
  REAL*8, DIMENSION(:), ALLOCATABLE, SAVE :: Tx, Ty
  REAL*8, SAVE :: Lx, Ly, Lz
  REAL*8, DIMENSION(:), ALLOCATABLE, SAVE :: dx, dy, dz, dp
  REAL*8, DIMENSION(:), ALLOCATABLE, SAVE :: pr, kx, ky
!-----------------------------------------------------------------------
! FLOW PROPERTIES OF COMPONENTS
! kro: oil relative permeability
! krw: water relative permeability
! Swcrit: critical water saturation
! Soirw: 
! korw: 
! Nw: 
! Swcon: 
! Sorw: 
! koro: 
! No: 
!-----------------------------------------------------------------------
  REAL*8, DIMENSION(:), ALLOCATABLE, SAVE :: kro, krw
  REAL*8, SAVE :: Swcrit, Soirw, korw, Nw, Swcon, Sorw, koro, No
!-----------------------------------------------------------------------
! RESERVOIR ROCK AND FLUID PROPERTIES
! Bo: formation volume factor oil, bbl/STB
! Bw: formation volume factor water, bbl/STB
! rho_o: density oil, lb/ft3
! rho_w: density water, lb/ft3
! mu_o: viscosity oil, cp
! mu_w: viscosity water, cp
! Co: compressibility oil, psi-1
! Cw: compressibility water, psi-1
! Cr: compressibility rock, psi-1
!-----------------------------------------------------------------------
  REAL*8, SAVE ::  Bo, Bw, rho_o, rho_w, mu_o, mu_w, Co, Cw, Cr
!-----------------------------------------------------------------------
! INITIALIZATION OF PRESSURE AND WATER SATURATION
! P: pressure, psi
! Sw: water saturation, fraction
!-----------------------------------------------------------------------
  REAL*8, DIMENSION(:), ALLOCATABLE, SAVE :: P, Sw
!-----------------------------------------------------------------------
! WELL LOCATION AND PRODUCTION DATA
! Nwell: number of wells
! xwell: x coordinates of the wells
! ywell: y coordinates of the wells
! rwell: radius of each well, ft
! gwell: grid number of wells
! qo: oil injection, ft3/day, '+' injector
! qw: water injection, ft3/day, '+' injector
! qp: constant flow rate producer, ft3/day, '-' producer
! bhp: constant bottom hole pressure well, psi
!-----------------------------------------------------------------------
  INTEGER, SAVE :: Nwell
  REAL*8, DIMENSION(:), ALLOCATABLE, SAVE :: xwell, ywell, rwell, We
  INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: gwell
  REAL*8, DIMENSION(:), ALLOCATABLE, SAVE :: qo, qw, qp, bhp
!-----------------------------------------------------------------------
END MODULE Global

