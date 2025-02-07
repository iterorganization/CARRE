      subroutine fcrblkd
!
!  version : 27.07.99 23:28
!
!======================================================================
      use KindDefinitions
      use fcrcom
      implicit none

      repart = 2
      nrelax = 5000
      relax = 0.2_RKind
      pasmin = 0.001_RKind
      rlcept = 1.e-6_RKind
      tgarde(1:ntrgx) = 0.0_RKind
      xptcntr(1:3,1:nxptm) = -1.0_RKind
      xlpcntr(1:3) = -1.0_RKind
      xpttol = 0.1_RKind
      nclstr = -1
      nrgn = 0
      nsgm = 0
      ntrg = 0
!======================================================================
      return
      end
