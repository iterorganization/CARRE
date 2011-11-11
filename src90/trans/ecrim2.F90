      subroutine ecrim2(nfin,nx,ny,crx,cry,fpsi,bb,b2cflag,nxmax,nymax)
!
!  version : 02.12.98 20:49
!
!======================================================================
!*** This routine writes the grid from carre directly in the B2.5 format
! by Heimo Buerbaumer
!======================================================================
      implicit none
!
!  arguments
      integer nfin,nx,ny,nxmax,nymax
      real*8 crx(0:nxmax,0:nymax,0:3),cry(0:nxmax,0:nymax,0:3), & 
     &  bb(0:nxmax,0:nymax,0:3),fpsi(0:nxmax,0:nymax,0:3)
      integer b2cflag(0:nxmax,0:nymax,2)

!  local variables
      integer ix,iy
      real*8 x0,y0,fpsi0
!
!  procedures
!======================================================================
!  calculation

!* 2.   print mesh parameters

      write(nfin,'(a)') "VERSION01.001.028"
      write(nfin,*) nx,"  ",ny

      do iy=0,ny-1
        do ix=0,nx-1

!* 2.1  calculate coordinates of cell centre

          x0=0.25*(crx(ix,iy,0)+crx(ix,iy,1)+crx(ix,iy,2)+crx(ix,iy,3))
          y0=0.25*(cry(ix,iy,0)+cry(ix,iy,1)+cry(ix,iy,2)+cry(ix,iy,3))
          fpsi0=0.25*(fpsi(ix,iy,0)+fpsi(ix,iy,1)+fpsi(ix,iy,2)+fpsi(ix,iy,3))


!* print B2.5
          write (nfin,117) ix+1,iy+1,x0,y0,fpsi0, &
               & crx(ix,iy,0),cry(ix,iy,0),fpsi(ix,iy,0), & 
               & crx(ix,iy,1),cry(ix,iy,1),fpsi(ix,iy,1), &
               & crx(ix,iy,2),cry(ix,iy,2),fpsi(ix,iy,2), & 
               & crx(ix,iy,3),cry(ix,iy,3),fpsi(ix,iy,3), &
               & bb(ix,iy,0),bb(ix,iy,2), &
               & b2cflag(ix,iy,:)



        enddo
      enddo

 117  FORMAT(I4,1X,I4,1X,17(F12.8,1X),I4,2I4)
      return
!======================================================================
      end
