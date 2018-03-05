      function aire(x,y,n)
c
c
c  VERSION: 03.08.99 12:12
c=======================================================================
c*** Calculate the area of a polygon with n-1 nodes (the n-th node 
c*** coincides with the 1st one).
c*** The area is negative if the nodes are listed clockwise.
c*** The area is calculated via decomposition of the polygon into a set
c*** of triangles having a common node at the first node of the polygon. 
c=======================================================================
      implicit none
#include <CARRETYPES.F>
      real(kind=R8) aire
      integer n
      real(kind=R8) x(n),y(n)
      integer i,k
      real(kind=R8) x1,y1,x2,y2
      logical hlp
      data hlp /.true./, k /0/
c=======================================================================

c<<<
c      k=k+1
c      hlp=k.le.4
c      if(hlp) then !{
c      	write(0,*) 'new aire',k,n
c	write(0,'(a4,1p,10e11.4/(4x,10e11.4))') ' x: ',x
c	write(0,'(a4,1p,10e11.4/(4x,10e11.4))') ' y: ',y
c	write(0,'(a4,7a7,4a10)') 'i','x(1)','y(1)','x(i)','y(i)',
c     ,	    'x(i+1)','y(i+1)','x1','y1','x2','y2','aire'
c      end if !}
c>>>
      aire=0._R8
      do i=2,n-2 !{
        x1=x(i)-x(1)
        y1=y(i)-y(1)
        x2=x(i+1)-x(1)
        y2=y(i+1)-y(1)
        aire=aire+(x1*y2-x2*y1)
c<<<
c      	if(hlp) then !{
c	  write(0,'(1x,i3,6f7.3,1p,5e10.3)') i,x(1),y(1),x(i),y(i),
c     ,	    x(i+1),y(i+1),x1,y1,x2,y2,aire
c      	end if !}
c>>>
      end do !}
      aire=0.5*aire
      hlp=.false.
c=======================================================================
      end
