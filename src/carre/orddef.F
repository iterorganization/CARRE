      subroutine orddef(inddef,nbdef,xst,yst,ptx,pty,npx)
c
c  VERSION: 23.06.99 11:27
c
c=======================================================================
c*** This subroutine orders the indices in the vector inddef such that 
c*** the first element points to top right, second top left, third bottom
c*** right, fourth bottom left. If there is only one x-point, then 1 is 
c*** the right and 2 is the left.
c***  	NB: the o-point co-ordinates are in  ptx(npx+1), pty(npx+1).
c=======================================================================
      implicit none
*  arguments
      integer nbdef, inddef(nbdef),npx
      real*8 xst(*),yst(*),ptx(npx+1),pty(npx+1)

*  variables locales
      integer n
      real*8 tt,xx(2),xy(2),zero
      parameter (n=4, zero=0.)
      integer i, j, itt, ipx
      real*8 xt(n), yt(n),wx(4),wy(4)
      real*8 u_hlp

*  procedures
      real*8 aire
      external aire
c=======================================================================
c.. i,j: indices
c.. n  : maximum number of targets considered in this routine
c.. xt,yt: temporary arrays containing the x and y co-ordinates of the 
c     	   first points of a divertor plate
c.. ptx, pty: x-point co-ordinates
c.. xx,xy: x-point co-ordinates ordered from top to bottom
c.. npx: number of x-points
c=======================================================================
c<<<
      write(0,*) 'Entering orddef: npx,nbdef= ',npx,nbdef
      write(0,'(9h inddef: ,20i4/(9x,20i4))') inddef
      write(0,'(6h xst: ,1p,10e10.2/(6x,10e10.2))') (xst(i),i=1,nbdef)
      write(0,'(6h yst: ,1p,10e10.2/(6x,10e10.2))') (yst(i),i=1,nbdef)
      if(npx.gt.0) then !{
      	write(0,'(6h ptx: ,1p,10e10.2/(6x,10e10.2))') (ptx(i),i=1,npx)
      	write(0,'(6h pty: ,1p,10e10.2/(6x,10e10.2))') (pty(i),i=1,npx)
      end if !}
c>>>
c*** Order the x-points from top to bottom if necessary

      if(npx.eq.2) then !{
        if(pty(1).gt.pty(2)) then !{
          xx(1)=ptx(1)
          xy(1)=pty(1)
          xx(2)=ptx(2)
          xy(2)=pty(2)
        else !}{
          xx(1)=ptx(2)
          xy(1)=pty(2)
          xx(2)=ptx(1)
          xy(2)=pty(1)
        endif !}
      elseif(npx.eq.1) then !}{
          xx(1)=ptx(1)
          xy(1)=pty(1)
      elseif(npx.gt.2) then !}{
        write(6,*) 'npx=',npx,' too large in orddef'
        stop
      endif !}

c*** Copy inddef, xstruc and ystruc of the first point of the structure 
c*** on the temporary arrays

      do i=1, nbdef !{
        xt(i) = xst(i)
        yt(i) = yst(i)
      end do !}

c*** Order the structures according to y

      do i=1,nbdef-1 !{
        do j=i+1, nbdef !{
          if(yt(j).gt.yt(i)) then !{
            tt=yt(i)
            yt(i)=yt(j)
            yt(j)=tt
            tt=xt(i)
            xt(i)=xt(j)
            xt(j)=tt
            itt=inddef(i)
            inddef(i)=inddef(j)
            inddef(j)=itt
          endif !}
        end do !}
      end do !}
c<<<
      write(0,*) 'After ordering in y'
      write(0,'(9h inddef: ,20i4/(9x,20i4))') inddef
c>>>
c*** Order the plates 2 by 2 in x
c<<<
      write(0,'(1x,a3,a4,a6,2a9,2x,1h:,18x,a2,16x,1h:,18x,a2)') 'i',
     ,	'ipx','xy','pty','aire','wx','wy'
c>>>
      do i=1,nbdef-1,2 !{
        ipx=(i+1)/2
        wx(1)=xx(ipx)
        wy(1)=xy(ipx)
        wx(2)=xt(i)
        wy(2)=yt(i)
        wx(3)=xt(i+1)
        wy(3)=yt(i+1)
        wx(4)=wx(1)
        wy(4)=wy(1)
c<<<
	u_hlp=aire(wx,wy,4)
	write(0,'(1x,2i3,1p,1x,11e9.2)') i,ipx,
     ,	  xy(ipx),pty(npx+1),u_hlp,wx,wy
c>>>
        if((xy(ipx)-pty(npx+1))*aire(wx,wy,4).lt.zero) then !{
          itt=inddef(i)
          inddef(i)=inddef(i+1)
          inddef(i+1)=itt
        endif !}
      end do !}
c<<<
      write(0,*) 'After ordering in x'
      write(0,'(9h inddef: ,20i4/(9x,20i4))') inddef
c>>>
c=======================================================================
      end
