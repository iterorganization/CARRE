      subroutine coord(vx,vy,n,d,x,y)

c  version : 21.07.2000 16:18
c=======================================================================
      implicit none

c*** Calculate the x and y co-ordinates of a point located on a curve at
c*** a distance d from the origin of the curve

*  arguments
      integer n
      real*8 vx(n),vy(n),d,x,y

*  variables locales
      integer i
      real*8 dist,distot,eps
      parameter (eps=1.e-06)

*  procedures
      intrinsic sqrt

c========================
c.. n   : number of the points in the curve.
c.. vx,vy: co-ordinates of the points in the curve.
c.. d   : required distance along the curve
c.. x,y : co-ordinates of the found point
c========================

*..Initialisation.

      distot = 0.0

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
