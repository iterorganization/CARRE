      subroutine wreqvr(lun,ngpr,iret,nr,nz,rgr,zgr,pfm)
!
!  version : 29.04.97 21:55
!
!======================================================================
!*** Wrighting the CARRE equilibrium file
!***
!*** Input:
!***  lun     the logical unit number for the output
!***  ngpr    the maximum number of points in R direction
!***  nr      the actual number of points in R direction
!***  nz      the actual number of points in Z direction
!***  rgr     the R values for the grid points
!***  zgr     the Z values for the grid points
!***  pfm     the values of the poloidal flux
!***
!*** Output:
!***  iret    return code (0 means OK)
!======================================================================
      implicit none
      integer lun,ngpr,iret,nr,nz
      real*8 pfm(ngpr,*),rgr(ngpr),zgr(*)
!
      integer i,j
!======================================================================
!
      write(lun,*,err=999)
      write(lun,'(i5,i6)',err=999) nr,nz
      write(lun,*,err=999) '$r'
      write(lun,710,err=999) 'nr',nr
      write(lun,720,err=999) (rgr(i),i=1,nr)
      write(lun,*,err=999) '$z'
      write(lun,710,err=999) 'nz',nz
      write(lun,720,err=999) (zgr(i),i=1,nz)
      write(lun,*,err=999)
      write(lun,*,err=999) '$psi'
      write(lun,720,err=999) ((pfm(i,j),i=1,nr),j=1,nz)
      iret=0
      return
!======================================================================
 999  iret=1
!======================================================================
  710 format(1x,a2,'=',i3)
  720 format(1p,1x,6e13.5)
!
      end
