      subroutine orddef(inddef,nbdef,xst,yst,ptx,pty,npx)
!
!  VERSION: 23.06.99 11:27
!
!=======================================================================
!*** This subroutine orders the indices in the vector inddef such that
!*** the first element points to top right, second top left, third bottom
!*** right, fourth bottom left. If there is only one X-point, then 1 is
!*** the right and 2 is the left.
!***  	NB: the O-point coordinates are in ptx(npx+1), pty(npx+1).
!=======================================================================
      use KindDefinitions
      implicit none
!  arguments
      integer nbdef, inddef(nbdef),npx
      real(rKind) :: xst(*),yst(*),ptx(npx+1),pty(npx+1)

!  variables en common

#include <COMLAN.F>

!  variables locales
      integer n
      real(rKind) :: tt,xx(2),xy(2),zero
      parameter (n=4, zero=0.)
      integer i, j, itt, ipx
      real(rKind) :: xt(n), yt(n),wx(4),wy(4)
      real(rKind) :: u_hlp

!  procedures
      real(rKind) :: aire
      external aire
!=======================================================================
!.. i,j: indices
!.. n  : maximum number of targets considered in this routine
!.. xt,yt: temporary arrays containing the x and y coordinates of the
!     	   first points of a divertor plate
!.. ptx, pty: X-point coordinates
!.. xx,xy: X-point coordinates ordered from top to bottom
!.. npx: number of X-points
!=======================================================================
!<<<
      write(0,*) 'Entering orddef: npx,nbdef= ',npx,nbdef
      write(0,'(a9,20i4/(9x,20i4))') ' inddef: ',inddef
      write(0,'(a6,1p,10e12.4/(6x,10e12.4))') ' xst: ',(xst(i),i=1,nbdef)
      write(0,'(a6,1p,10e12.4/(6x,10e12.4))') ' yst: ',(yst(i),i=1,nbdef)
      if(npx.gt.0) then !{
       write(0,'(a6,1p,10e12.4/(6x,10e12.4))') ' ptx: ',(ptx(i),i=1,npx)
       write(0,'(a6,1p,10e12.4/(6x,10e12.4))') ' pty: ',(pty(i),i=1,npx)
      end if !}
!>>>
!*** Order the X-points from top to bottom if necessary

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
        if(sellan(1:7).eq.'english') then
          write(6,*) 'npx=',npx,' too large in orddef'
        elseif(sellan(1:8).eq.'francais') then
          write(6,*)'npx=',npx,' trop grand dans orddef'
        endif
        stop
      endif !}

!*** Copy inddef, xstruc and ystruc of the first point of the structure
!*** on the temporary arrays

      do i=1, nbdef !{
        xt(i) = xst(i)
        yt(i) = yst(i)
      end do !}

!*** Order the structures according to y

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
!<<<
      write(0,*) 'After ordering in y'
      write(0,'(a9,20i4/(9x,20i4))') ' inddef: ',inddef
!>>>
!*** Order the plates 2 by 2 in x
!<<<
      write(0,'(1x,a3,a4,a6,2a9,3x,a1,17x,a2,16x,a1,17x,a2)') 'i', &
     & 'ipx','xy','pty','aire',':','wx',':','wy'
!>>>
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
!<<<
      u_hlp=aire(wx,wy,4)
      write(0,'(1x,2i3,1p,1x,11e9.2)') i,ipx, &
     &   xy(ipx),pty(npx+1),u_hlp,wx,wy
!>>>
        if((xy(ipx)-pty(npx+1))*aire(wx,wy,4).lt.zero) then !{
          itt=inddef(i)
          inddef(i)=inddef(i+1)
          inddef(i+1)=itt
        endif !}
      end do !}
!<<<
      write(0,*) 'After ordering in x'
      write(0,'(a9,20i4/(9x,20i4))') ' inddef: ',inddef
!>>>
!=======================================================================
      end
