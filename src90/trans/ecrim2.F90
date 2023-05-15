      subroutine ecrim2(nfin,nx,ny,crx,cry,&
           & fpsi,psidx,psidy,bb,ffbz,b2cflag,&
           & nxmax,nymax,&
           & niso,nxiso,nisomx)
        use KindDefinitions
        use carre_constants
        use tradui_constants , only: GRID_N_CELLFLAGS
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
      integer nfin,nx,ny,nxmax,nymax,niso,nisomx,nxiso(nisomx+1)
      real(rKind) :: crx(0:nxmax,0:nymax,0:3),cry(0:nxmax,0:nymax,0:3), &
     & bb(0:nxmax,0:nymax,0:3),ffbz(0:nxmax,0:nymax,0:3),&
     & fpsi(0:nxmax,0:nymax,0:3),&
     & psidx(0:nxmax,0:nymax,0:3),psidy(0:nxmax,0:nymax,0:3)
      integer b2cflag(0:nxmax,0:nymax,CARREOUT_NCELLFLAGS)

!  local variables
      integer ix,iy,i
      real(rKind) :: x0,y0,fpsi0
      character(len=35) :: hlp_format
!
!  procedures
!======================================================================
!  calculation

!* 2.   print mesh parameters

      write(nfin,'(a)') "VERSION01.001.028"
      write(hlp_format,'(a,i2,a)') '(3(i4,a2),', nisomx, '(i4,1x))'
      if (niso == 0) then
          write(nfin,hlp_format) nx,"  ",ny,"  ",niso,"  ",(0,i=1,nisomx)
      else
          write(nfin,hlp_format) nx,"  ",ny,"  ",niso,"  ",nxiso(1:nisomx)
      end if

      write(hlp_format,'(a,i1,a)') '(I4,1X,I4,1X,29(F15.8,1X),', &
                                  &  CARREOUT_NCELLFLAGS,'(I4,1X))'
      do iy=0,ny-1
        do ix=0,nx-1

!* 2.1  calculate coordinates of cell centre

          x0=0.25*(crx(ix,iy,0)+crx(ix,iy,1)+crx(ix,iy,2)+crx(ix,iy,3))
          y0=0.25*(cry(ix,iy,0)+cry(ix,iy,1)+cry(ix,iy,2)+cry(ix,iy,3))
          fpsi0=0.25*(fpsi(ix,iy,0)+fpsi(ix,iy,1)+fpsi(ix,iy,2)+fpsi(ix,iy,3))
!* print B2.5
          write (nfin,hlp_format) &
               & ix+1,iy+1,&
               & x0,y0,fpsi0, &  ! cell center quantities
               & (crx(ix,iy,i),cry(ix,iy,i),&
               &  fpsi(ix,iy,i),psidx(ix,iy,i),psidy(ix,iy,i),ffbz(ix,iy,i),i=0,3), & ! corner quantities
               & bb(ix,iy,0),bb(ix,iy,2), & ! cell center mag. field
               & b2cflag(ix,iy,1:GRID_N_CELLFLAGS) ! cell flags

        enddo
      enddo

      return
!======================================================================
      end
