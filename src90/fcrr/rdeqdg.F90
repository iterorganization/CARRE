      subroutine rdeqdg(lun,ngpr,ngpz,iret, nr,nz, & 
     &                                             btf,rtf,rgr,zgr,pfm)
!=====================================================
!*** Read the equilibrium data written in the dg-compatible format.
!***
!*** Input:
!***  lun     the logical unit number for the input
!***  ngpr    the maximum number of points in R direction
!***  ngpz    the maximum number of points in Z direction
!***
!*** Output:
!***  iret    return code (0 means OK)
!***  nr      the actual number of points in R direction
!***  nz      the actual number of points in Z direction
!***  btf     the toroidal magnetic field at the R=rtf
!***  rgr     the R values for the grid points
!***  zgr     the Z values for the grid points
!***  pfm     the values of the (poloidal flux - separatrix flux)
!***
!*** If the input file contains no data on the toroidal field (old
!*** version), then the field file must be pre-connected to LUN 3
!=====================================================
!
!  version : 25.02.98 20:54
!
      use KindDefinitions
      implicit none
      integer(Short), intent(in) :: lun, ngpr, ngpz
      integer(Short), intent(out) :: iret, nr, nz
      real(rKind) :: rgr(*), zgr(*), pfm(ngpr, *)
!... toroidal field in tesla, radius in m
      real(rKind) :: btf, rtf
      real ubtf, urtf
!... internal
      integer i, j
!-----------------------------------------------------
!
!*** Read the plasma equilibrium ...
!
      iret=0
      print *,'rdeqdg: before rdeqlh'
      call rdeqlh(lun,nr,nz,ubtf,urtf,*99)
      btf=ubtf
      rtf=urtf
      print *,'rdeqdg: after rdeqlh. nr,nz,btf,rtf= ',nr,nz,btf,rtf
      if(rtf.le.0.) then
!
!*** Read the toroidal field and the corresponding radius
!*** from a separate file (for compatibility with old versions)
!
          read(3,*,err=99)
          read(3,*,err=99) btf, rtf
          close(3)
      end if
      if(nr.gt.ngpr) then
          print *,'==== rdeqdg: nr > ngpr ',nr
          iret=2
      end if
      if(nz.gt.ngpz) then
          print *,'=== rdeqdg: nz > ngpz'
          iret=2
      end if
      if(nr.le.0) then
          print *,'=== rdeqdg: nr < 1'
          iret=4
      end if
      if(nz.le.0) then
          print *,'=== rdeqdg: nz < 1'
          iret=4
      end if
      if(iret.ne.0) return
!
!      read(lun,8000) (rgr(i),i=1,nr)
      print *,'reading rgr...'
      read(lun,*) (rgr(i),i=1,nr)
      read(lun,*)
      read(lun,*)
      print *,'reading zgr...'
!      read(lun,8000) (zgr(i),i=1,nz)
      read(lun,*) (zgr(i),i=1,nz)
      read(lun,*)
      read(lun,*)
      print *,'reading pfm...'
!      read(lun,8000) ((pfm(i,j),i=1,nr),j=1,nz)
      read(lun,*) ((pfm(i,j),i=1,nr),j=1,nz)
 8000 format(5(3x,e14.8))
      iret=0
      return
!-----------------------------------------------------
!
 99   print *,'==== rdeqdg: error in the input files'
      iret=8
!
      end
