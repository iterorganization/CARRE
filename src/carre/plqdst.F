
************************************************************************
      FUNCTION plqdst(x0,y0,x2,y2,xst,yst,npst,doug)
************************************************************************
c
c  version : 18.06.98 20:20
c
      IMPLICIT NONE
#include <CARRETYPES.F>
      real(kind=R8) plqdst

*..Cette sous-routine calcule la distance entre deux points sur une meme
*  structure, en longeant les segments de structure.

*  arguments
      INTEGER npst
      REAL(kind=R8) x0,y0,x2,y2,xst(npst),yst(npst)
      CHARACTER*(*) doug

*  variables locales
      INTEGER ind,ind1,ind2,sens
      REAL(kind=R8) x1,y1,x3,y3,dist

*  procedures
      INTEGER indsgm,drctio
      INTRINSIC MOD,SQRT
      EXTERNAL drctio,indsgm
     ,        ,trc_stk_in,trc_stk_out

c=========================
c.. xst,yst: tableaux des coordonnees des points de la structure.
c.. npst: nombre de points de la structure.
c.. x0,y0: point de depart sur la structure.
c.. x2,y2: point d'arrivee sur la structure.
c.. doug: droite ou gauche
c.. ind2: indice du segment sur lequel se trouve le point 2.
c.. sens: sens dans lequel il faut partir pour la recherche,
c         1 = sens des points de la structure, -1 = sens contraire.
c.. x1,y1,x3,y3: points intermediaires.
c=========================

*..Copie d'arguments en variables locales

      x1 = x0
      y1 = y0

*..Recherche du sens et indices de segment pour les deux points.

      call trc_stk_in('plqdst','*start')
      sens = drctio(xst,yst,npst,x1,y1,doug)
      call trc_stk_out

      ind1 = indsgm(xst,yst,npst,x1,y1)
      ind2 = indsgm(xst,yst,npst,x2,y2)

*..On verifie si les points sont sur le meme segment.

      IF (ind1 .EQ. ind2) THEN
         plqdst = SQRT((x1-x2)**2 + (y1-y2)**2)
         RETURN
      ELSE

*..Sinon on calcule la distance de coin en coin jusqu'a ce qu'on soit
*  sur le segment du point d'arrivee.

         dist= 0.

   10    CONTINUE

         IF (sens .EQ. 1) THEN
            ind = MOD(ind1,npst-1) + 1
         ELSE
            ind = ind1
         ENDIF

         x3 = xst(ind)
         y3 = yst(ind)

         dist = dist + SQRT((x1-x3)**2 + (y1-y3)**2)

         IF (sens .EQ. 1) THEN

            IF (ind .NE. ind2) THEN
               x1 = x3
               y1 = y3
               ind1 = ind
               GO TO 10
            ELSE
               plqdst = dist + SQRT((x2-x3)**2 + (y2-y3)**2)
               RETURN
            ENDIF

         ELSE

            IF (ind .NE. MOD(ind2,npst-1) + 1) THEN
               x1 = x3
               y1 = y3
               ind1 = ind1 - 1
               IF (ind1 .EQ. 0) ind1 = npst - 1
               GO TO 10
            ELSE
               plqdst = dist + SQRT((x2-x3)**2 + (y2-y3)**2)
               RETURN
            ENDIF

         ENDIF
      ENDIF

      END
