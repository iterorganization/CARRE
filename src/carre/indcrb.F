
************************************************************************
      INTEGER FUNCTION indcrb(vx,vy,n,x,y,d)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette fonction renvoie l'indice du point du debut du segment entre
*  2 points parametrises sur lequel se trouve le point x,y. On effectue
*  un produit scalaire entre les vecteurs allant du point a chaque
*  extremite du segment. Sur le segment de contact ou se trouve le
*  point, l'angle sera le plus grand. L'indice trouve est le plus grand
*  tel que la distance parcourue jusqu'a cet indice est inferieure ou
*  egale a la distance d

*  arguments
      INTEGER n
      REAL(kind=R8) vx(n),vy(n),x,y,d

*  variables locales
      INTEGER i
      REAL(kind=R8) mumin, mu, ax, ay, bx, by, dist, eps,dd
      PARAMETER (eps=1.0E-6_R8)

*  procedures
      INTRINSIC SQRT

c=========================
c.. n   : nombre  de points de la courbe.
c.. vx,vy: vecteurs des coordonnees des points de la courbe.
c.. x,y : point sur la courbe dont on recherche l'indice.
c.. ax,ay,bx,by: composantes x et y des vecteurs a et b.
c.. mu: angle entre les vecteurs a et b.
c=========================

*..Initialisation.

      mumin = 1.0_R8
      indcrb = 1

*..Test pour savoir si le point x,y est egal au premier point parame-
*  trise.

      ax = vx(1) - x
      ay = vy(1) - y

      dist = SQRT(ax*ax + ay*ay)
      dd=sqrt((vx(2)-vx(1))**2+(vy(2)-vy(1))**2)

      IF (dist .LT. eps .and. dd.gt.d) THEN

         indcrb = 1
         RETURN

      ENDIF

*..Boucle sur tous les points parametrises.

      dd=0.
      DO 10 i=1, n-1
      dd=dd+sqrt((vx(i+1)-vx(i))**2+(vy(i+1)-vy(i))**2)
      if(dd.ge.d) then

*..Def. des 2 vecteurs a et b.

         ax = vx(i) - x
         ay = vy(i) - y
         bx = vx(i+1) - x
         by = vy(i+1) - y

*..Test pour voir si le point x,y tombe sur le point parametrise suivant

         dist = SQRT(bx*bx + by*by)
         IF (dist .LT. eps) THEN
            IF (i .NE. n-1) THEN
               indcrb = i + 1
               RETURN
            ELSE
               indcrb = i
               RETURN
            ENDIF
         ELSE

*..On trouve le cosinus de l'angle entre les 2 vecteurs

            mu = (((ax*bx) + (ay*by))
     .                /SQRT(((ax**2)+(ay**2)) * ((bx**2)+(by**2))))
c

*..L'indice de segment correspond au cosinus le plus petit

            IF (mu .lt. mumin) THEN
               indcrb = i
               mumin = mu
            ENDIF

         ENDIF

      endif
   10 CONTINUE

      RETURN
      END
