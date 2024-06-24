      subroutine fcrblkd
c
c  version : 27.07.99 23:28
c
c======================================================================
      use carre_types
      use fcrcom
      implicit none

      repart = 2
      nrelax = 5000
      relax = 0.2_R8
      pasmin = 0.001_R8
      rlcept = 1.e-6_R8
      tgarde(1:ntrgx) = 0.0_R8
      xptcntr(1:3,1:nxptm) = -1.0_R8
      xlpcntr(1:3) = -1.0_R8
      xpttol = 0.1_R8
      nclstr = -1
      nrgn = 0
      nsgm = 0
      ntrg = 0
      lm_cnfg = 0
      lm_pntrt = 0.1_R8
      lm_grcln = 0.1_R8

c======================================================================
      return
      end
