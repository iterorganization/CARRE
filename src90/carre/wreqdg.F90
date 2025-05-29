      subroutine wreqdg(lun,ngpr,iret,nr,nz,rgr,zgr,pfm)

!=====================================================
!*** Write the equilibrium data in the dg compatible format.
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
!=====================================================

      use KindDefinitions
      implicit none
      integer(Short) lun, ngpr, iret, nr, nz
      real(rKind) :: rgr(ngpr), zgr(*), pfm(ngpr,*)
!... toroidal field in Tesla, radius in m
      real(rKind) :: btf, rtf
      integer i, j
      logical ex

!*** Obtain nominal magnetic field and major radius

      inquire(file='btor.dat',exist=ex)
      if(ex) then     ! {
        ex=.false.
        open(17,file='btor.dat',status='old',err=10)
        rewind(17)
        read(17,*,err=10) btf
        close(17)
        ex= btf .ne. 0.                                   ! DPC
        if(.not.ex) write(6,*) 'Zero value of Btor*R "in btor.dat"'    ! }
      else     ! {
        write(*,*) 'No file btor.dat found.'     ! }
      end if
 10   if(.not.ex) then     ! {
        write(6,*) 'Unable to get data from "btor.dat"'
        write(6,*) 'Please enter the value of Btor*R (0 to quit):'
        read *,btf
        if(btf.eq.0.) stop    ! }
      end if

!***  Write the plasma equilibrium ...

      iret=0
      write(lun,*,err=99) &
     &  '   jm   :=  no. of grid points in radial direction;'
      write(lun,*,err=99) &
     &  '   km   :=  no. of grid points in vertical direction;'
      write(lun,*,err=99) &
     &  '   r    :=  radial   coordinates of grid points  [m];'
      write(lun,*,err=99) &
     &  '   z    :=  vertical coordinates of grid points  [m];'
      write(lun,*,err=99) &
     &  '   psi  :=  flux per radian at grid points      [Wb/rad];'
      write(lun,*,err=99) &
     &  '   psib :=  psi at plasma boundary              [Wb/rad];'
      write(lun,*,err=99) &
     &  '   btf  :=  toroidal magnetic field                  [T];'
      write(lun,*,err=99) &
     &  '   rtf  :=  major radius at which btf is specified   [m];'
      write(lun,*,err=99)
      write(lun,*,err=99)
      write(lun,*,err=99) '   jm    = ', nr,';'
      write(lun,*,err=99) '   km    = ', nz,';'
      write(lun,*,err=99) '   psib  = ',0.,' Wb/rad;'
      write(lun,*,err=99) '   btf   = ',btf,' T;'
      write(lun,*,err=99) '   rtf   = ',1.,' m;'
      write(lun,*,err=99)
      write(lun,*,err=99) '   r(1:jm);'
      write(lun,8000) (rgr(i),i=1,nr)
      write(lun,*)
      write(lun,*,err=99) '   z(1:km);'
      write(lun,8000) (zgr(i),i=1,nz)
      write(lun,*)
      write(lun,*) '     ((psi(j,k)-psib,j=1,jm),k=1,km)'
      write(lun,8000) ((pfm(i,j),i=1,nr),j=1,nz)
 8000 format(5(3x,e15.8))
      iret=0
      return
!-----------------------------------------------------

 99   print *,'==== wreqdg: error in the output files'
      iret=8

      end
