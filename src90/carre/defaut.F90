      subroutine defaut
!
!  version : 22.06.98 14:45
!
!======================================================================
      use KindDefinitions
      use carre_dimensions
      use comlan
      use comrlx
      implicit none
!..  Definition de certaines variables par defaut
!
!  arguments: aucun
!

!======================================================================
!  calcul
!
!  1.   parametres de relaxation
      nrelax=200
      relax=0.2
      pasmin=0.001
      dpol1max=0.01
      dpol2max=0.01
      rlcept=1.e-6
!
!  2.   langue de travail
      sellan='english'
!     sellan='francais'
!
      return
!======================================================================
      end
