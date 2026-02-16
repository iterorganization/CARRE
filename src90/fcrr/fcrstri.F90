      subroutine fcrstri(lun)
!
!  version : 27.04.97 16:36
!
!======================================================================
!*** Read the structure file from dg
!======================================================================
      use KindDefinitions
      use fcrcom
      implicit none
      integer lun
      integer(Short) :: i,j,k,l,n
      real(Single) x, y
!======================================================================
!
      rewind(lun)
      read(lun,*) n
      nstr=0
      l=0
      do i=1,n
        read(lun,*,err=900) k
        do j=1,k
          l=l+1_Short
          read(lun,*,err=900) x,y
          xstr(l)=1.e-3_rKind*real(x,rKind)
          ystr(l)=1.e-3_rKind*real(y,rKind)
        end do
        nstr=nstr+1_Short
        lstr(nstr)=k
      end do
      return
!======================================================================
 900  write(*,*) 'fcrstri: error in the structure',i
      stop
!======================================================================
      end
