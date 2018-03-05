************************************************************************
      INTEGER FUNCTION ifind(x,t,n,inc)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette fonction retourne l'indice i tel que t(i)<=x<t(i+1) sur un
*  tableau t d'indice 1, ..., n par bonds de inc.
*  On suppose ici que mod(n-1,inc)=0.

*  arguments
      INTEGER n,inc
      REAL(kind=R8) x, t(n)

*  variables locales
      INTEGER j, jp

c=========================
c.. x: valeur pour laquelle on veut trouver l'indice de t
c.. t: tableau de coordonnees sur lequel on veut interpoler
c.. n: nombre de points dans le tableau
c.. inc: increment
c=========================

      IF (x.LE.t(1)) THEN
         ifind=1
         RETURN

      ELSE IF (x.GE.t(1+n-inc)) THEN
         ifind=n-(2*inc)+1
         RETURN

      ELSE
         ifind=0
         j=n/inc+1

    2    IF (j-ifind.EQ.1) THEN
            ifind=1+inc*(ifind-1)
            RETURN

         ENDIF

         jp=(j+ifind)/2
         IF (x.GT.t(1+inc*(jp-1))) THEN
            ifind=jp

         ELSE
            j=jp

         ENDIF

         GO TO 2
      ENDIF

      END
