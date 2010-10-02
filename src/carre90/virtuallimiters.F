      subroutine virtuallimiters(nivx,nivy,nivtot,nbniv,npx,ptx,pty, & 
     &     nstruc,npstru,xstruc,ystruc)

!=======================================================================
!*** This routine creates virtual limiters.

!*** One virtual limiter is created for every limiting curve.
!*** The virtual limiters are triangles, the tip of which is placed
!*** in the middle of the limiting curve (measured
!*** in physical length along the curve) and the triangle is oriented
!*** away from the O-point
!=======================================================================

      implicit none
#include <CARREDIM.F>

!  arguments
      real*8 :: nivx(npnimx,nivmx),nivy(npnimx,nivmx)
      real*8 :: ptx(npxmx),pty(npxmx)
      real*8 :: xstruc(npstmx,strumx),ystruc(npstmx,strumx)
      integer :: nbniv, nivtot(nbniv), npx, nstruc, npstru(strumx)

!  variables locales
      real*8 :: l, x, y, ox, oy, pi
      integer :: iniv
      parameter(pi=3.141592654)

!  procedures
      real*8 :: long
      external long

!.. nbniv : number of the limiting level lines
!.. nivx,nivy: coordinates of the points of the parametrised
!              limiting level lines (point index, curve index)
!.. nivtot: number of points for each parametrised limiting level line

!=======================================================================

      do iniv = 1, nbniv

         l = long( nivx(:,iniv), nivy(:,iniv), nivtot(iniv) )
         call coord( nivx(:,iniv), nivy(:,iniv), nivtot(iniv), & 
     &        l/2.0, x, y )

         ox = ( x - ptx(npx) ) * 0.1
         oy = ( y - pty(npx) ) * 0.1

         nstruc = nstruc + 1
         npstru(nstruc) = 4
         xstruc(1,nstruc) = x
         ystruc(1,nstruc) = y

         call rotate( ox, oy, pi / 6.0 )
         xstruc(2,nstruc) = x + ox
         ystruc(2,nstruc) = y + oy

         call rotate( ox, oy, - pi / 3.0 )
         xstruc(3,nstruc) = x + ox
         ystruc(3,nstruc) = y + oy

         xstruc(4,nstruc) = x
         ystruc(4,nstruc) = y

      enddo

      end
