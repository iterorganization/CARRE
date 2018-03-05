
************************************************************************
      FUNCTION long(vx,vy,n)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>
      real(kind=R8) long

*..  Cette fonction calcule la distance totale que parcourt une courbe v
*  de n points.

*  arguments
      INTEGER n
      REAL(kind=R8) vx(n),vy(n)

*  variables locales
      INTEGER i
      REAL(kind=R8) dist

*  procedures
      INTRINSIC SQRT

c=========================
c.. n   : nombre  de points de la courbe.
c.. vx,vy: vecteurs des coordonnees des points de la courbe.
c.. dist: distance entre 2 points consecutifs de la courbe.
c=========================

      long = 0.0_R8

      DO 10 i=1, n-1

         dist = SQRT((vx(i+1)-vx(i))**2 + (vy(i+1)-vy(i))**2)
         long = long + dist

   10 CONTINUE

      RETURN
      END
