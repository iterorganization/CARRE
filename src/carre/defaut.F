      subroutine defaut
c
c  version : 22.06.98 14:45
c
c======================================================================
      implicit none
c..  Definition de certaines variables par defaut
c
c  arguments: aucun
c
c  variables en common
c
cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>

#include <COMLAN.F>
#include <COMRLX.F>
c======================================================================
c  calcul
c
c  1.   parametres de relaxation
      nrelax=200
      relax=0.2_R8
      pasmin=0.001_R8
      rlcept=1.0e-6_R8
c
c  2.   langue de travail
      sellan='english'
c     sellan='francais'
c
      return
c======================================================================
      end
