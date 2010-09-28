      subroutine virtuallimiters(nivx,nivy,nivtot,
     .      nbniv)
c
c  version : 21.07.2000 13:38
c
c=======================================================================
c*** This routine creates virtual limiters.

c*** One virtual limiter is created for every limiting curve.
c*** The virtual limiters are triangles, the tip of which is placed 
c*** in the middle of the limiting curve (measured
c*** in physical length along the curve) and the triangle is oriented
c*** away from the O-point
c=======================================================================

      implicit none
#include <CARREDIM.F>

*  arguments

*  variables locales
      
*  procedures

c.. nbniv : number of the limiting level lines
c.. nivx,nivy: coordinates of the points of the parametrised
c              limiting level lines (point index, curve index)
c.. nivtot: number of points for each parametrised limiting level line


c=======================================================================



      do iniv = 1, nbniv




      enddo
