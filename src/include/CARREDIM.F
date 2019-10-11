c  version : 07.07.97 20:52
c
      integer nxmax, nymax , npstmx, npnimx, npxmx , strumx, nivmx,
     .        nbdmx, gradmx, npmamx, nrmamx, nregmx
      parameter (nxmax=1025,  nymax=1025, npstmx=360, npnimx=5000,
     .           npxmx=4   , strumx=60  ,  nivmx=8  , nbdmx=8  ,
     .           gradmx=100, npmamx=500 , nrmamx=200, nregmx=6)
c
c======================================================================
c***  gradmx      : max. number of points with zero gradients (x/o)
c***  nxmax, nymax: max. grid dimensions for equilibrium data (psi)
c***  npstmx      : max. number of points in a structure
c***  strumx      : max. number of structures
c***  npxmx       : max. number of the x-points to be treated
c***  nivmx       : max. number of the level lines
c***  npmamx      : max. number of the poloidal mesh points
c***  nrmamx      : max. number of the radial mesh points
c***  nregmx      : max. number of regions
c***  npnimx      : max. number of points in tracing curves
c***  nbdmx       : max. number of divertor plate structures
c***
c======================================================================
