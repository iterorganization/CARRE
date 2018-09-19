      block data fcrblkd
c
c  version : 27.07.99 23:28
c
c======================================================================
      implicit none
      integer n
#include <CARRETYPES.F>
#include <FCRCOM.F>
      parameter(n=nxptm*3)
      data repart , nrelax  ,  relax, pasmin, rlcept,  tgarde /
     /       2    , 5000    , 0.2_R8,0.001_R8,1.e-6_R8, ntrgx*0._R8/
      data xptcntr, xlpcntr , xpttol, nclstr,  nrgn , nsgm , ntrg /
     /    n*-1._R8, 3*-1._R8, 0.1_R8,   -1  ,    0  , 	0  ,   0  /
      data lm_cnfg, lm_pntrt,lm_grcln/
     /       0    ,    0.1  ,   0.1  /
c======================================================================
      end
