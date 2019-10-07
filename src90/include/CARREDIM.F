!  version : 07.07.97 20:52
!
      integer, parameter :: nxmax=1025,  nymax=1025, npstmx=1200, &
     &          npnimx=5000, &
     &          npxmx=4   , strumx=60  ,  nivmx=8  , nbdmx=8 , &
     &          gradmx=100  , npmamx=2000 , nrmamx=200, nregmx=6, &
     &          nsepsegmx = 10

!     AUTOCONTINUE=.true. will skip questions when failing to converge
#ifdef EUITM
!     for ITM use, always skip
      logical, parameter :: AUTOCONTINUE = .true.
#else
      logical, parameter :: AUTOCONTINUE = .true.      
#endif

!
!======================================================================
!***  gradmx      : max. number of points whith zero gradients (x/o)
!***  nxmax, nymax: max. grid dimensions for equilibrium data (psi)
!***  npstmx      : max. number of points in a structure
!***  strumx      : max. number of structures
!***  npxmx       : max. number of the X-points to be treated
!***  nivmx       : max. number of the level lines
!***  npmamx      : max. number of the poloidal mesh points
!***  nrmamx      : max. number of the radial mesh points
!***  nregmx      : max. number of regions
!***  npnimx      : max. number of points in tracing curves
!***                (for curves tracing contour lines)
!***  nbdmx       : max. number of divertor plate structures
!***  nsepsegmx   : max. number of separatrix segments
!***
!======================================================================
