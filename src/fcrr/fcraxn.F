      subroutine fcraxn(nam)
c
c  version : 25.08.97 16:30
c
c======================================================================
c*** Input of auxiliary data for Carre
c======================================================================
      implicit none
#include <CARRETYPES.F>
#include <FCRCOM.F>
      character*8 nam
c
      integer nnm
      parameter (nnm=7)
      real(kind=R4) rdummy, rarray(ntrgx)
      character*8 name, nms(nnm)
      integer i
      logical streql
      external streql,resimi,resime,rearre,fcrtrn
c
c*** List of the valid input keywords
      data nms /
     1    'repart  ','nrelax  ','relax   ','pasmin  ','rlcept  ',
     2    'tgarde  ','xpttol  '/
c======================================================================
c
      call locase(nam,name,8)
      do i=1,nnm
        if(streql(name,nms(i))) exit
      end do
c
      select case (i)
      case (1)
        call resimi(repart)
      case (2)
        call resimi(nrelax)
      case (3)
        call resime(rdummy)
        relax = real(rdummy,R8)
      case (4)
        call resime(rdummy)
        pasmin = real(rdummy,R8)
      case (5)
        call resime(rdummy)
        rlcept = real(rdummy,R8)
      case (6)
        call rearre(rarray,ntrgx,ntrg)
        tgarde(1:ntrg) = real(rarray(1:ntrg),R8)
      case (7)
        call resime(rdummy)
        xpttol = real(rdummy,R8)
      case default
        call fcrtrn(name)
      end select
      return
c======================================================================
      end
