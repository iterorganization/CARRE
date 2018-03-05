
************************************************************************
      SUBROUTINE COORD1(vx,vy,n,d,x,y,period)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..Cette sous-routine renvoit les coordonnees x et y d'un point situe
*  sur une courbe v, et a une distance d du debut de celle-ci.

*  arguments
      INTEGER n
      REAL(kind=R8) vx(n),vy(n),d,x,y,period

*  variables locales
      INTEGER i
      REAL(kind=R8) dist,distot,eps,zero,dd
      PARAMETER (eps=1.0E-06_R8,zero=0.0_R8)

*  procedures
      INTRINSIC SQRT

c========================
c.. n   : nombre de points de la courbe.
c.. vx,vy: vecteurs des points de la courbe.
c.. d   : distance a laquelle il faut trouver x et y
c.. x,y : coordonnees du point situe sur la courbe a une distance d du
c         debut de celle-ci.
c.. period: periode du circuit si non nul
c.. dist: distance entre 2 points consecutifs de la courbe.
c.. distot: distance totale parcourue.
c========================

*..Initialisation.

      dd=d
      if(period.gt.zero) then
        if(d.gt.period) then
          dd=d-int(d/period)*period
        elseif(d.lt.zero) then
          dd=d-(int(d/period)-1)*period
        endif
      endif
      distot=zero

*..Boucle sur tous les segments de courbe.

      DO 10 i=1, n-1

         dist=SQRT((vx(i+1)-vx(i))**2 + (vy(i+1)-vy(i))**2)
         distot=distot + dist

*..On regarde si on a depasse la distance d.

         IF (distot .GE. dd) THEN
            x=vx(i+1) - ((distot-dd)/dist)*(vx(i+1)-vx(i))
            y=vy(i+1) - ((distot-dd)/dist)*(vy(i+1)-vy(i))

            RETURN

         ENDIF

   10 CONTINUE

*..Si on a rien trouve alors on verifie si la distance est la meme que
*  la longueur de la courbe. Si oui on prend le dernier point.

      IF (ABS(distot - dd) .LT. eps) THEN
         x=vx(n)
         y=vy(n)

         RETURN

      ENDIF

*..Si la distance d est plus grande que la distance totale de la courbe,
*  alors il y a une erreur.

      PRINT *, 'distance plus grande que dist. totale de la courbe dans'
     .  ,' COORD1'
      print*,'distot=',distot
      print*,'dd=',dd
      call pltend
      STOP
      END
