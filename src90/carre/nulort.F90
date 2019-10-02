      FUNCTION nulort(d)
!
!  version : 07.07.97 18:37
!
!======================================================================
      use KindDefinitions
      IMPLICIT NONE
      REAl(rKind) :: nulort

!..  Cette fonction

!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

! arguments
      REAL(rKind) :: d

!  variables en common

#include <COMORT.F>

!  variables locales
      REAL(rKind) :: x2,y2,ux2,uy2,x12,y12,zlong,dd

!  procedures
      INTRINSIC SQRT,INT
      EXTERNAL COORD,UNTANG
!======================================================================
!.. d  : distance a partir de la courbe 2 a laquelle on calcule le point
!.. x1,y1: coordonnees du point sur la courbe 1.
!.. x2,y2: coordonnees du point sur la courbe 2.
!.. ux1,uy1: vecteur unite a partir du point 1.
!.. ux2,uy2: vecteur unite a partir du point 2.
!.. xn2,yn2: tableaux des coordonnees en x et y des points successifs
!             parametrises de la courbe 2.
!.. npni2: nombre de points dans la courbe 2.
!.. x12,y12: vecteur allant du point sur la courbe 1 vers le point sur
!            la courbe 2.
!.. zlong: longueur du vecteur x12,y12.
!======================================================================

!..Calculs.

!..Initialisation de dd

      IF (period .GT. 0.) THEN
         IF (d .GT. 0.) THEN

            dd = d - INT(d/period)*period

         ELSE

            dd = d + (INT(-d/period) + 1.)*period

         ENDIF

      ELSE

         dd = d

      ENDIF

!..On trouve les coordonnees du point de la courbe 2.

      CALL COORD(xn2,yn2,npni2,dd,x2,y2)

!..Calcul des composantes du vecteur unite tangent a la courbe 2.

      CALL UNTANG(xn2,yn2,npni2,x2,y2,ux2,uy2,dd,period)

!..Calcul de la difference des cosinus.

!..On force les 2 vecteurs unites a etre paralleles.

!     IF ((ux1*ux2 + uy1*uy2) .LT. 0.) THEN
!        ux2 = -ux2
!        uy2 = -uy2
!     ENDIF

!..Definition du vecteur allant du point de la courbe 1 au point de
!   la courbe 2

      x12 = x2 - x1
      y12 = y2 - y1

!..Normalisation.

      zlong = SQRT(x12*x12 + y12*y12)
      x12 = x12/zlong
      y12 = y12/zlong

      nulort = (ux1 + ux2)*x12 + (uy1 + uy2)*y12

      RETURN
      END
