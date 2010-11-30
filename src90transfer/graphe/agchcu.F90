      SUBROUTINE AGCHCU (IFLG,KDSH)
!
!  version : 05.04.97 15:49
!
!======================================================================
!  variables en common
#include <COMQUA.F>
!======================================================================
! The routine AGCHCU is called by AGCURV just before and just after each
! curve is drawn.  The default version does nothing.  A user may supply
! a version to change the appearance of the curves.  The arguments are
! as follows:
!
! - IFLG is zero if a curve is about to be drawn, non-zero if a curve
!   has just been drawn.
!
! - KDSH is the last argument of AGCURV, as follows:
!
!      AGCURV called by   Value of KDSH
!      ----------------   ----------------------------------------
!      EZY                1
!      EZXY               1
!      EZMY               "n" or "-n", where n is the curve number
!      EZMXY              "n" or "-n", where n is the curve number
!      the user program   the user value
!
!   The sign of KDSH, when AGCURV is called by EZMY or EZMXY, indicates
!   whether the "user" dash patterns or the "alphabetic" dash patterns
!   were selected for use.
!
! Done.
!
!  L'appel a la routine GSLWSC permet de modifier l'epaisseur du trait
!  utilise pour tracer les courbes.
!======================================================================

      if(qualit.eq.1) then
        CALL GSLWSC(6.)
      else
        call gslwsc(1.)
      endif

!  Cette sous-routine permet de modifier tous les caracteres qui sont
!  produits par AUTOGRAPH. L'appel a la routine GKS avec ICAR=-13 donne
!  un jeu de caracteres plus epais que la valeur par defaut. [Voir le
!  manuel pour la definition des jeux de caracteres].

      if(qualit.eq.1) then
        ICAR = -13
      else
        icar=1
      endif
      CALL GSTXFP (ICAR,0)

      RETURN
!
      END
