      SUBROUTINE AGCHCU (IFLG,KDSH)
c
c  version : 05.04.97 15:49
c
c======================================================================
c  variables en common
#include <COMQUA.F>
c======================================================================
C The routine AGCHCU is called by AGCURV just before and just after each
C curve is drawn.  The default version does nothing.  A user may supply
C a version to change the appearance of the curves.  The arguments are
C as follows:
C
C - IFLG is zero if a curve is about to be drawn, nonzero if a curve
C   has just been drawn.
C
C - KDSH is the last argument of AGCURV, as follows:
C
C      AGCURV called by   Value of KDSH
C      ----------------   ----------------------------------------
C      EZY                1
C      EZXY               1
C      EZMY               "n" or "-n", where n is the curve number
C      EZMXY              "n" or "-n", where n is the curve number
C      the user program   the user value
C
C   The sign of KDSH, when AGCURV is called by EZMY or EZMXY, indicates
C   whether the "user" dash patterns or the "alphabetic" dash patterns
C   were selected for use.
C
C Done.
C
C  L'appel a la routine GSLWSC permet de modifier l'epaisseur du trait
C  utilise pour tracer les courbes.
c======================================================================

      if(qualit.eq.1) then
        CALL GSLWSC(6.)
      else
        call gslwsc(1.)
      endif

C  Cette sous-routine permet de modifier tous les caracteres qui sont
C  produits par AUTOGRAPH. L'appel a la routine GKS avec ICAR=-13 donne
C  un jeu de caracteres plus epais que la valeur par defaut. [Voir le
C  manuel pour la definition des jeux de caracteres].

      if(qualit.eq.1) then
        ICAR = -13
      else
        icar=1
      endif
      CALL GSTXFP (ICAR,0)

      RETURN
C
      END
