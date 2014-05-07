      block data fcrblkd
!
!  version : 27.07.99 23:28
!
!======================================================================
      implicit none
      integer n
#include <FCRCOM.F>
      parameter(n=nxptm*3)
      data repart , nrelax  ,  relax, pasmin, rlcept,  tgarde / & 
     &       2    ,  5000   ,   0.2 ,  0.001,  1.e-6, ntrgx*0./
      data xptcntr,  xlpcntr, xpttol, nclstr,  nrgn , nsgm , ntrg / & 
     &      n*-1. ,   3*-1. ,   0.1 ,   -1  ,    0  ,  0  ,   0  /
!======================================================================
      end
