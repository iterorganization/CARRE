!***********************************************************************
      LOGICAL FUNCTION in(x,y,xstr,ystr,n)
!***********************************************************************
      IMPLICIT NONE

!..  Cette fonction determine si un point est a l'interieur d'une
!  structure fermee.

!  arguments
      INTEGER n
      REAL*8 x, y, xstr(n), ystr(n)

!  variables locales
      INTEGER i
      REAL*8 angtot,theta,prdvec,a

!  procedures
      INTRINSIC ACOS,ABS

!=========================
!.. xst,yst: tableaux des coordonnees des points de la structure.
!.. n  : nombre de points de la structure.
!.. x,y: point ou il faut verifier.
!.. theta
!.. theta: angle entre les vecteurs allant du point au debut d'un
!          segment et du allant du point a la fin de ce segment.
!.. prdvec: produit vectoriel entre les vecteurs.
!.. angtot: somme des angles.
!=========================

!  Le produit scalaire est utilise pour trouver l'angle et le
!  produit vectoriel, pour voir si cet angle sera additionne
!  ou soustrait.

      angtot=0.
      DO 10 i=1, n-1
         prdvec = (xstr(i)-x)*(ystr(i+1)-y) - (ystr(i)-y)*(xstr(i+1)-x)

       if (prdvec.ne.0.) then
           theta = acos(((xstr(i)-x)*(xstr(i+1)-x) + & 
     &                   (ystr(i)-y)*(ystr(i+1)-y))/ & 
     &                   (sqrt(((xstr(i)-x)**2+(ystr(i)-y)**2)* & 
     &                   ((xstr(i+1)-x)**2+(ystr(i+1)-y)**2))))
         else
         theta = 3.14159
       endif

         IF (prdvec.lt.0.) THEN
            a = -1.
         ELSE
            a = 1.
         ENDIF

         angtot = angtot + a*theta

   10 CONTINUE

      IF (ABS((ABS(angtot)-6.28318)).LT.0.001) THEN
         in = .TRUE.
      ELSE
         in = .FALSE.
      ENDIF
      RETURN
      END
