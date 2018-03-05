      subroutine coord(vx,vy,n,d,x,y)

c  version : 21.07.2000 16:18
c=======================================================================
      implicit none
#include <CARRETYPES.F>

c*** Calculate the x and y coordinates of a point located on a curve v 
c*** at a distance d from the origin of the curve

*  arguments
      integer n
      real(kind=R8) vx(n),vy(n),d,x,y

*  variables locales
      integer i
      real(kind=R8) dist,distot,eps
      parameter (eps=1.0e-06_R8)

*  procedures
      intrinsic sqrt

c========================
c.. n   : number of points in the curve
c.. vx,vy: coordinates of the points in the curve
c.. d   : required distance along the curve
c.. x,y : coordinates of the found point
c.. dist: distance between 2 consecutive points on the curve
c.. distot: total distance covered
c========================

*..Initialisation.

      distot = 0.0_R8

*..Loop over the curve segments

      do i=1, n-1 !{

         dist = sqrt((vx(i+1)-vx(i))**2 + (vy(i+1)-vy(i))**2)
         distot = distot + dist

*..Check whether the distance d is passed

         if (distot .ge. d) then !{
            x = vx(i+1) - ((distot-d)/dist)*(vx(i+1)-vx(i))
            y = vy(i+1) - ((distot-d)/dist)*(vy(i+1)-vy(i))

            return

         end if !}

      end do  !}

c*** Check whether the required distance is equal - within tolerance -
c*** to the total curve length

      if (abs(distot - d) .lt. eps) then !{
         x = vx(n)
         y = vy(n)

         return

      end if !}

*..Error: required distance greater than the total curve length.

      print *, 'coord: required distance greater than the total length'
      print*,'distot=',distot
      print*,'d=',d
      call pltend
      stop  'Check the target specification'

      end
