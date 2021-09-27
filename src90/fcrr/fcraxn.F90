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
      character nms(nnm)*8
      integer i
      data nms / & 
     &    'repart  ','nrelax  ','relax   ','pasmin  ','rlcept  ', & 
     &    'tgarde  ','xpttol  '/
      external resimi, resime, rearre, fcrtrn
!======================================================================
!
      do i=1,nnm
        if(nam.eq.nms(i)) exit
      end do
      select case (i)
      case (1)
        call resimi(repart)
      case (2)
        call resimi(nrelax)
      case (3)
        call resime(relax)
      case (4)
        call resime(pasmin)
      case (5)
        call resime(rlcept)
      case (6)
        call rearre(tgarde,ntrgx,ntrg)
      case (7)
        call resime(xpttol)
      case default
        call fcrtrn(nam)
      end select
      return
!======================================================================
      end
