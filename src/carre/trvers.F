
************************************************************************
      LOGICAL FUNCTION trvers(x1,y1,x2,y2,vx,vy,n)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette fonction nous dit si le segment de 1 a 2 traverse la courbe v

*  arguments
      INTEGER n
      REAL(KIND=R8) x1,y1,x2,y2,vx(n),vy(n)

*  variables locales
      INTEGER i
      REAL(KIND=R8) determ, mult1,mult2,zero,un
      parameter(zero=0.0_R8,un=1.0_R8)

c=========================
c.. vx,vy: tableaux des coordonnees des points de la courbe.
c.. n  : nombre de points de la courbe.
c.. x1,y1,x2,y2: coordonnees des 2 points formant le segment.
c.. determ: determinant de la matrice des deux equations.
c.. mult1: facteur multiplicatif du segment.
c.. mult2: facteur multiplicatif du segment de la courbe v.
c=========================

*..Boucle sur tous les segments de la courbe.

      DO 10 i=1, n-1

*..Calcul du determinant de la matrice.

         determ = (-(x2 - x1)) * (vy(i+1) - vy(i)) +
     .               (y2 - y1) * (vx(i+1) - vx(i))

*..Si determinant non nul, alors il y a solution.

         IF (determ .NE. 0.) THEN

*..Facteur multiplicatif du segment avec la methode de Cramer.

            mult1 = (-(vx(i)-x1) * (vy(i+1)-vy(i)) +
     .                 (vy(i)-y1) * (vx(i+1)-vx(i)))/determ

*..Pour avoir intersection, il faut que mult1 soit entre 0 et 1

            IF ((mult1.GT.zero).AND.(mult1.LT.un)) THEN

*..Fact. mult. du segment de la courbe v.

               mult2= ((x2-x1) * (vy(i)-y1) -
     .                   (y2-y1) * (vx(i)-x1))/determ

*..Intersection si mult2 entre 0 et 1

               IF ((mult2.GT.zero).AND.(mult2.LT.un)) THEN

*..S'il y a intersection alors le segment traverse la courbe.

                  trvers = .TRUE.
                  RETURN

               ENDIF

            ENDIF

         ENDIF

   10 CONTINUE

      trvers = .FALSE.

      RETURN
      END
