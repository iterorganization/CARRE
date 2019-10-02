      function aire(x,y,n)
!
!
!  VERSION: 03.08.99 12:12
!=======================================================================
!*** Calculate the area of a polygon with n-1 nodes (the n-th node
!*** coincides with the 1st one).
!*** The area is negative if the nodes are listed clockwise.
!*** The area is calculated via decomposition of the polygon into a set
!*** of triangles having a common node at the first node of the polygon.
!=======================================================================
      use KindDefinitions
      implicit none
      real(rKind) :: aire
      integer n
      real(rKind) :: x(n),y(n)
      integer i,k
      real(rKind) :: x1,y1,x2,y2
      logical hlp
      data hlp /.true./, k /0/
!=======================================================================

!<<<
!      k=k+1
!      hlp=k.le.4
!      if(hlp) then !{
!      	write(0,*) 'new aire',k,n
!	write(0,'(4h x: ,1p,10e11.4/(4x,10e11.4))') x
!	write(0,'(4h y: ,1p,10e11.4/(4x,10e11.4))') y
!	write(0,'(a4,7a7,4a10)') 'i','x(1)','y(1)','x(i)','y(i)',
!     ,	    'x(i+1)','y(i+1)','x1','y1','x2','y2','aire'
!      end if !}
!>>>
      aire=0.
      do i=2,n-2 !{
        x1=x(i)-x(1)
        y1=y(i)-y(1)
        x2=x(i+1)-x(1)
        y2=y(i+1)-y(1)
        aire=aire+(x1*y2-x2*y1)
!<<<
!      	if(hlp) then !{
!	  write(0,'(1x,i3,6f7.3,1p,5e10.3)') i,x(1),y(1),x(i),y(i),
!     ,	    x(i+1),y(i+1),x1,y1,x2,y2,aire
!      	end if !}
!>>>
      end do !}
      aire=0.5*aire
      hlp=.false.
!=======================================================================
      end
