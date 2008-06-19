
************************************************************************
      REAL*8 FUNCTION long(vx,vy,n)
************************************************************************
      IMPLICIT NONE

*..  Cette fonction calcule la distance totale que parcours une courbe v
*  de n points.

*  arguments
      INTEGER n
      REAL*8 vx(n),vy(n)

*  variables locales
      INTEGER i
      REAL*8 dist

*  procedures
      INTRINSIC SQRT

c=========================
c.. n   : nombre  de points de la courbe.
c.. vx,vy: vecteurs des coordonnees des points de la courbe.
c.. dist: distance entre 2 points consecutifs de la courbe.
c=========================

      long = 0.0

      DO 10 i=1, n-1

         dist = SQRT((vx(i+1)-vx(i))**2 + (vy(i+1)-vy(i))**2)
         long = long + dist

   10 CONTINUE

      RETURN
      END
