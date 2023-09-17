      subroutine fcraxn(nam)
!
!  version : 25.08.97 16:30
!
!======================================================================
!*** Input of auxiliary data for Carre
!======================================================================
      use KindDefinitions
      implicit none
#include <FCRCOM.F>
      character*8 nam
!
      integer nnm
      parameter (nnm=7)
      real(Single) rdummy, rarray(ntrgx)
      character*8 name, nms(nnm)
      integer i
      logical carre_streql
      external locase
      data nms / & 
     &    'repart  ','nrelax  ','relax   ','pasmin  ','rlcept  ', & 
     &    'tgarde  ','xpttol  '/
      external carre_streql, resimi, resime, rearre, fcrtrn
!======================================================================
!
      call locase(nam,name,8)
      do i=1,nnm
        if(carre_streql(name,nms(i))) exit
      end do
      select case (i)
      case (1)
        call resimi(repart)
      case (2)
        call resimi(nrelax)
      case (3)
        call resime(rdummy)
        relax = real(rdummy,rKind)
      case (4)
        call resime(rdummy)
        pasmin = real(rdummy,rKind)
      case (5)
        call resime(rdummy)
        rlcept = real(rdummy,rKind)
      case (6)
        call rearre(rarray,ntrgx,ntrg)
        tgarde(1:ntrg) = real(rarray(1:ntrg),rKind)
      case (7)
        call resime(rdummy)
        xpttol = real(rdummy,rKind)
      case default
        call fcrtrn(name)
      end select
      return
!======================================================================
      end
