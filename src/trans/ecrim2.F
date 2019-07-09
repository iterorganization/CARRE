      subroutine ecrim2(nfin,nx,ny,crx,cry,bb,fpsi,ffbz,delpsi,
     .  nxmax,nymax)
c
c  version : 02.12.98 20:49
c
c======================================================================
c*** This routine writes the grid from carre directly in the B2.5 format
c by Heimo Buerbaumer
c======================================================================
      implicit none
#include <CARRETYPES.F>
c
c  arguments
      integer nfin,nx,ny,nxmax,nymax
      real(kind=R8)
     .  crx(-1:nxmax,-1:nymax,0:3),cry(-1:nxmax,-1:nymax,0:3),
     .  bb(-1:nxmax,-1:nymax,0:3),fpsi(-1:nxmax,-1:nymax,0:3),
     .  ffbz(-1:nxmax,-1:nymax,0:3),delpsi
c
c  local variables
      integer ix,iy
      real(kind=R8) x0,y0
c
c  procedures
c======================================================================
c  calculation

c* 2.   print mesh parameters

      write(nfin,*) nx,"  ",ny

      do iy=-1,ny
        do ix=-1,nx

c* 2.1  calculate coordinates of cell centre

          x0=0.25*(crx(ix,iy,0)+crx(ix,iy,1)+crx(ix,iy,2)+crx(ix,iy,3))
          y0=0.25*(cry(ix,iy,0)+cry(ix,iy,1)+cry(ix,iy,2)+cry(ix,iy,3))

c* print B2.5
          write (nfin,117) ix+1,iy+1,x0,y0,crx(ix,iy,0),cry(ix,iy,0),
     &      crx(ix,iy,1),cry(ix,iy,1),crx(ix,iy,2),cry(ix,iy,2),
     &         crx(ix,iy,3),cry(ix,iy,3),bb(ix,iy,0),bb(ix,iy,2)
         
        enddo
      enddo

      do iy=-1,ny
        do ix=-1,nx
          write (nfin,117) ix+1,iy+1,
     &     fpsi(ix,iy,0)-delpsi,fpsi(ix,iy,1)-delpsi,
     &     fpsi(ix,iy,2)-delpsi,fpsi(ix,iy,3)-delpsi,
     &     ffbz(ix,iy,0),ffbz(ix,iy,1),ffbz(ix,iy,2),ffbz(ix,iy,3)
        enddo
      enddo

 10   continue
 117  FORMAT(I4,1X,I4,1X,12(F13.8,1X))
      return
c======================================================================
      end
