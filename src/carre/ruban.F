
************************************************************************
      FUNCTION ruban(xn,yn,n,x1,y1,d)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>
      REAL(KIND=R8) ruban

*..  Cette fonction retourne la distance entre le debut de la courbe et
*  le point x1,y1.

*  arguments
      INTEGER n
      REAL(KIND=R8) x1,y1,xn(n),yn(n),d

*  variables locales
      INTEGER ind
      REAL(KIND=R8) dist

*  procedures
      INTEGER indcrb
      REAL(KIND=R8) long
      INTRINSIC SQRT
      EXTERNAL indcrb, long

c=========================
c.. xn,yn: tableaux des coordonnees des points de la courbe.
c.. n  : nombre de points de la courbe.
c.. x1,y1: coordonnees du point.
c=========================

      ruban = 0.0_R8

      ind = indcrb(xn,yn,n,x1,y1,d)

      dist = long(xn,yn,ind)

      ruban = dist + SQRT((x1-xn(ind))**2 + (y1-yn(ind))**2)

      RETURN
      END
