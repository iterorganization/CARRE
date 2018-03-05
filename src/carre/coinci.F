      integer function coinci(vx,vy,n,rx,ry)

c  version : 20.04.2001 23:04

c=======================================================================
c*** This function checks the points along a curve to find the point
c*** which coincides - within the tolerance parameter - with the
c*** specified reference point. 
c*** If such a point is not found, then the closest point is marked after
c*** the confirmation from user.

c*** Input:
c***  vx, vy  coordinates of points along the curve
c***  n       number of these points
c***  rx, ry  coordinates of the reference points

c*** Internel parameters:
c***  const   the tolerance

c*** Output:
c***  The function value gives the index of the point found

      implicit none
#include <CARRETYPES.F>

*  arguments
      integer n
      real(kind=R8) vx(n), vy(n), rx, ry

*  variables locales
      integer i, imn, irp
      real(kind=R8) const, rmn, tol, r
      parameter (const=0.001_R8, tol=const*const)
      character c

      data irp /0/
c=======================================================================

      rmn=1.e16
      imn=0
      do i=1, n
        r=(vx(i)-rx)**2+(vy(i)-ry)**2
        if (r.le.tol) then
          coinci = i
          return
        else if(r.lt.rmn) then
          rmn=r
          imn=i
        endif
      end do
      if(irp.ge.2) then
        print *,'More than two occasions of the X-point mismatch in ',
     ,    'coinci'
        print *,'==> May be an internal error in Carre'
        call pltend
        stop
      end if
      irp=irp+1
      rmn=sqrt(rmn)
      if(irp.eq.1) then
        print *
        print *,'== The following message may appear twice:'
        print *
      end if
      print '(1x,1p,2a,2(e10.3,a))',
     ,  'A separatrix misses the X-point by more than ',
     ,  'the hard-wired tolerance (',rmn,' vs.',const,' )'
      print *,'Are you sure the X-points should be connected? (y/n)'
 10   read '(a1)',c
      if(c.eq.'y' .or. c.eq.'Y') then
        coinci = imn
        return
      else if(c.eq.'n' .or. c.eq.'N') then
        call pltend
        stop
      end if
      print *,'Please answer with Y or N'
      go to 10
c=======================================================================
      end
