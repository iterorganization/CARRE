
************************************************************************
      LOGICAL FUNCTION cross(ind,xx,yy,xst,yst,n)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette fonction verifie si un segment traverse un segment de
*  structure autre que celui sur lequel on est en train de marcher.

*  arguments
      INTEGER ind, n
      REAL(kind=R8) xx(*),yy(*),xst(n),yst(n)

*  variables locales
      INTEGER i
      REAL(kind=R8) mult1,mult2,determ

c=========================
c.. xst,yst: tableaux des coordonnees des points de la structure.
c.. n  : nombre de points de la structure.
c.. xx,yy: tableaux des coordonnees contenant les 2 points du segment.
c.. ind: indice du segment de structure.
c.. determ: determinant de la matrice des deux equations.
c.. mult1: facteur multiplicatif du segment de courbe.
c.. mult2: facteur multiplicatif du segment de structure.
c=========================

*..Boucle sur la structure.
      DO 10 i=1, n-1

*..Calcul du determinant de la matrice.

         determ = (-(xx(2) - xx(1))) * (yst(i+1) - yst(i)) +
     .                   (yy(2) - yy(1)) * (xst(i+1) - xst(i))

*..Si determinant non nul, alors il y a solution.

         IF (determ .NE. 0.) THEN

*..Facteur multiplicatif du segment de courbe avec la methode de Cramer.

            mult1 = ((-(xst(i)-xx(1))) * (yst(i+1)-yst(i)) +
     .              (yst(i)-yy(1)) * (xst(i+1)-xst(i)))/determ

*..Pour avoir intersection, il faut que mult1 soit entre 0 et 1

            IF ((mult1.GT.0.).AND.(mult1.LT.1.)) THEN

*..Fact. mult. du segment de structure.

               mult2= ((xx(2)-xx(1)) * (yst(i)-yy(1)) -
     .                (yy(2)-yy(1)) * (xst(i)-xx(1)))/determ

*..Intersection si mult2 entre 0 et 1

               IF ((mult2.GT.0.).AND.(mult2.LT.1.)) THEN

*..On verifie si le segment de structure traversee est different de
*  celui sur lequel on marche.

                  IF (i .NE. ind) THEN

                     cross = .TRUE.
                     RETURN

                  ENDIF

               ENDIF

            ENDIF

         ENDIF

   10 CONTINUE

      cross = .FALSE.

      RETURN
      END
