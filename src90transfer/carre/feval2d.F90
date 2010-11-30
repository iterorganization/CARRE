      REAL*8 function feval2d(nx,ny,x,y,a00,a10,a01,a11,px,py)
!
!  version : 21.07.2000 13:38
!
!=======================================================================
!*** Evaluate a function given on a 2d grid at point px,py
!=======================================================================

      implicit none
!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

!  arguments
      integer nx,ny
      real*8 x(nxmax),y(nymax),px,py, & 
     &     a00(nxmax,nymax),a10(nxmax,nymax), & 
     &     a01(nxmax,nymax),a11(nxmax,nymax)
      real*8 f

!  variables locales

      integer ix, jx

!  procedures
      integer ifind
!=======================================================================


      ix = ifind( px, x, nx, 1 )
      jx = ifind( py, y, ny, 1 )

      feval2d = a00(ix,jx) + a10(ix,jx)*px & 
     &     + a01(ix,jx)*py & 
     &     + a11(ix,jx)*px*py

      end
