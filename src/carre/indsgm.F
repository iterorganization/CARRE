************************************************************************
      INTEGER FUNCTION indsgm(xst,yst,n,xx,yy)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette fonction trouve le segment d'une structure auquel est associe
*  un point de contact (xx,yy). Les erreurs numeriques font que ce point
*  n'est pas exactement sur le segment. Pour trouver le segment, on
*  effectue (pour chaque segment) un produit scalaire entre les 2
*  vecteurs suivants: 1) du point jusqu'a une des extremites du segment;
*  2) du point vers l'autre extremite. Ainsi l'angle qu'on mesurera pour le
*  segment de contact sera pres de 180 degrees et pour chaque autre on
*  aura un angle moindre. Le segment de contact sera celui qui
*  presentera l'angle le plus grand.

*  arguments
      INTEGER n
      REAL(kind=R8) xst(n), yst(n), xx, yy

*  variables locales
      INTEGER i
      REAL(kind=R8) mumin, mu, ax, ay, bx, by,eps,dist
      PARAMETER (eps=1.0E-6_R8)

*  procedures
      INTRINSIC sqrt

c=========================
c.. xst,yst: tableaux des coordonnees des points de la structure.
c.. n  : nombre de points de la structure.
c.. xx,yy: point de contact sur un segment de structure.
c.. ax,ay: vecteur allant du point de contact au debut d'un segment.
c.. bx,by: vecteur allant du point de contact au debut du segment
c          suivant.
c.. mu, mumin: cosinus de l'angle et de l'angle maximum
c=========================

*..Initialisation.

      mumin = 1.0_R8

*..Test pour savoir si le point x,y est egal au premier point parame-
*  trise.

      ax = xst(1) - xx
      ay = yst(1) - yy

      dist = SQRT(ax*ax + ay*ay)

      IF (dist .LT. eps) THEN

         indsgm = 1
         RETURN

      ENDIF

*..Boucle sur tous les segments

      DO 10 i=1, n-1

*..Def. des 2 vecteurs a et b.

         ax = xst(i) - xx
         ay = yst(i) - yy
         bx = xst(i+1) - xx
         by = yst(i+1) - yy

*..Test pour voir si le point x,y tombe sur le point parametrise suivant

         dist = SQRT(bx*bx + by*by)
         IF (dist .LT. eps) THEN
            IF (i .NE. n-1) THEN
               indsgm = i + 1
               RETURN
            ELSE
               indsgm = i
               RETURN
            ENDIF
         ELSE

*..On trouve le cosinus de l'angle entre les 2 par le produit scalaire.

           mu = ((ax*bx) + (ay*by))
     .                  /SQRT(((ax**2)+(ay**2)) * ((bx**2)+(by**2)))

*..L'indice de segment correspond au cosinus le plus petit

           IF (mu .lt. mumin) THEN
              indsgm = i
              mumin = mu
           ENDIF

      endif

   10 CONTINUE

      RETURN
      END
