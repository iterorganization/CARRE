      subroutine fcraxn(nam)
c
c  version : 25.08.97 16:30
c
c======================================================================
c*** Input of auxiliary data for Carre
c======================================================================
      implicit none
#include <FCRCOM.F>
      character*8 nam
c
      integer nnm
      parameter (nnm=7)
      character*8 name, nms(nnm)
      integer i
      logical streql
      external streql
c
c*** List of the valid input keywords
      data nms /
     1    'repart  ','nrelax  ','relax   ','pasmin  ','rlcept  ',
     2    'tgarde  ','xpttol  '/
c======================================================================
c
      write(name,'(a8)') nam
      do i=1,nnm
        if(streql(name,nms(i))) go to (10,20,30,40,50,60,70),i
      end do
      call fcrtrn(name)
      return
c
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
c======================================================================
      end
