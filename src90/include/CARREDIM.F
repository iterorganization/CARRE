!  version : 07.07.97 20:52
!
      integer nxmax, nymax , npstmx, npnimx, npxmx , strumx, nivmx, &
     &        nbdmx, gradmx, npmamx, nrmamx, nregmx 
      parameter (nxmax=1025,  nymax=1025, npstmx=1200, npnimx=3000, &
     &           npxmx=4   , strumx=60  ,  nivmx=8  , nbdmx=8  , &
     &          gradmx=100  , npmamx=500 , nrmamx=200, nregmx=6)
      logical, parameter :: AUTOCONTINUE = .true.

!
!======================================================================
!***  gradmx      : max. number of points whith zero gradients (x/o)
!***  nxmax, nymax: max. grid dimensions for equilibrium data (psi)
!***  npstmx      : max. number of points in a structure
!***  strumx      : max. number of structures
!***  npxmx       : max. number of the x-points to be treated
!***  nivmx       : max. number of the level lines
!***  npmamx      : max. number of the poloidal mesh points
!***  nrmamx      : max. number of the radial mesh points
!***  nregmx      : max. number of regions
!***  npnimx      : max. number of points in the tracing curve (?)
!***  nbdmx       : max. number of divertor plates (?)
!***
!.. npnimx: nombre maximum de points dans une courbe quelconque a
!           tracer.
!.. nbdmx: nombre maximum de structures etant une plaque de deflecteur.
!======================================================================
