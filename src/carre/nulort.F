      FUNCTION nulort(d)
c
c  version : 07.07.97 18:37
c
c======================================================================
      IMPLICIT NONE

*..  Cette fonction

cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>
      REAL(kind=R8) nulort

* arguments
      REAL(kind=R8) d

*  variables en common

#include <COMORT.F>

*  variables locales
      REAL(kind=R8) x2,y2,ux2,uy2,x12,y12,zlong,dd

*  procedures
      INTRINSIC SQRT,INT
      EXTERNAL COORD,UNTANG
c======================================================================
c.. d  : distance a partir de la courbe 2 a laquelle on calcule le point
c.. x1,y1: coordonnees du point sur la courbe 1.
c.. x2,y2: coordonnees du point sur la courbe 2.
c.. ux1,uy1: vecteur unite a partir du point 1.
c.. ux2,uy2: vecteur unite a partir du point 2.
c.. xn2,yn2: tableaux des coordonnees en x et y des points successifs
c             parametrises de la courbe 2.
c.. npni2: nombre de points dans la courbe 2.
c.. x12,y12: vecteur allant du point sur la courbe 1 vers le point sur
c            la courbe 2.
c.. zlong: longueur du vecteur x12,y12.
c======================================================================

*..Calculs.

*..Initialisation de dd

      IF (period .GT. 0.) THEN
         IF (d .GT. 0.) THEN

            dd = d - INT(d/period)*period

         ELSE

            dd = d + (INT(-d/period) + 1.)*period

         ENDIF

      ELSE

         dd = d

      ENDIF

*..On trouve les coordonnees du point de la courbe 2.

      CALL COORD(xn2,yn2,npni2,dd,x2,y2)

*..Calcul des composantes du vecteur unite tangent a la courbe 2.

      CALL UNTANG(xn2,yn2,npni2,x2,y2,ux2,uy2,dd,period)

*..Calcul de la difference des cosinus.

*..On force les 2 vecteurs unites a etre paralleles.

c     IF ((ux1*ux2 + uy1*uy2) .LT. 0.) THEN
c        ux2 = -ux2
c        uy2 = -uy2
c     ENDIF

*..Definition du vecteur allant du point de la courbe 1 au point de
*   la courbe 2

      x12 = x2 - x1
      y12 = y2 - y1

*..Normalisation.

      zlong = SQRT(x12*x12 + y12*y12)
      x12 = x12/zlong
      y12 = y12/zlong

      nulort = (ux1 + ux2)*x12 + (uy1 + uy2)*y12

      RETURN
      END
