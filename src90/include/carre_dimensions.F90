      module carre_dimensions

      integer, parameter :: nxmax=1025
      integer, parameter :: nymax=1025
      integer, parameter :: npstmx=1200
      integer, parameter :: npnimx=5000
      integer, parameter :: npxmx=4
      integer, parameter :: strumx=60
      integer, parameter :: nivmx=8
      integer, parameter :: nbdmx=8
      integer, parameter :: gradmx=100
      integer, parameter :: npmamx=2000
      integer, parameter :: nrmamx=200
      integer, parameter :: nregmx=6
      integer, parameter :: nsepsegmx = 10

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
      end module carre_dimensions
