      block data fcrblkd
!
!  version : 27.07.99 23:28
!
!======================================================================
      use KindDefinitions
      implicit none
      integer n
#include <FCRCOM.F>
      parameter(n=nxptm*3)
      data repart , nrelax  / &
     &       2    ,  5000   /
      data   relax,      pasmin,      rlcept,  tgarde / &
     &   0.2_rKind, 0.001_rKind, 1.e-6_rKind, ntrgx*0._rKind /
      data xptcntr,     xlpcntr,    xpttol / &
     & n*-1._rKind, 3*-1._rKind, 0.1_rKind /
      data nclstr,  nrgn , nsgm , ntrg / &
     &       -1  ,    0  ,  0  ,   0   /
!======================================================================
      end
