      subroutine defaut
!
!  version : 22.06.98 14:45
!
!======================================================================
      implicit none
!..  Definition de certaines variables par defaut
!
!  arguments: aucun
!
!  variables en common
!
!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

#include <COMLAN.F>
#include <COMRLX.F>
!======================================================================
!  calcul
!
!  1.   parametres de relaxation
      nrelax=200
      relax=0.2
      pasmin=0.001
      rlcept=1.e-6
!
!  2.   langue de travail
      sellan='english'
!     sellan='francais'
!
      return
!======================================================================
      end
