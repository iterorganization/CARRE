      subroutine fcraxn(nam)
!
!  version : 25.08.97 16:30
!
!======================================================================
!*** Input of auxiliary data for Carre
!======================================================================
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
!======================================================================
!
      do i=1,nnm
        if(nam.eq.nms(i)) go to (10,20,30,40,50,60,70),i
      end do
      call fcrtrn(nam)
      return
!
 10   call resimi(repart)
      return
 20   call resimi(nrelax)
      return
 30   call resime(relax)
      return
 40   call resime(pasmin)
      return
 50   call resime(rlcept)
      return
 60   call rearre(tgarde,ntrgx,ntrg)
      return
 70   call resime(xpttol)
      return
!======================================================================
      end
