
************************************************************************
      LOGICAL FUNCTION tsplaq(vx,vy,k,npx,px,py)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette fonction verifie si l'accroissement d'une courbe est monotone

*  arguments
      INTEGER k, npx
      REAL(kind=R8) vx(k), vy(k), px,py

*  variables locales
      INTEGER i
      REAL(kind=R8) dist1,dist2,eps_Xpt
      common /geom/ eps_Xpt
cxpb      PARAMETER (eps=0.015)
c!!!  attention a ce facteur de 0.05. Xavier a du l'augmenter dans
c     certains cas.

*  procedures
      INTRINSIC SQRT

c=========================
c.. vx,vy: tableaux des coordonnees des points de la courbe.
c.. k  : nombre de points de la courbe.
c.. px,py: coordonnees du point X.
c.. dist1,dist2: distance entre un point de coordonnee et le point X.
c=========================
      dist1 = 0.
      DO 10 i=2, k
         dist2 = (vx(i)-vx(1))**2 + (vy(i)-vy(1))**2
         IF (dist2 .LT. dist1) THEN
            tsplaq = .FALSE.
            RETURN
         ENDIF
         IF (npx .GT. 1) THEN
           IF (SQRT((vx(i)-px)**2 + (vy(i)-py)**2) .LT. eps_Xpt) THEN
               tsplaq = .FALSE.
               RETURN
            ENDIF
         ENDIF
         dist1 = dist2
   10 CONTINUE
      tsplaq = .TRUE.
      RETURN
      END
