
!***********************************************************************
      LOGICAL FUNCTION tsplaq(vx,vy,k,npx,px,py,eps_Xpt)
!***********************************************************************
      IMPLICIT NONE

!..  Cette fonction verifie si l'accroissement d'une courbe est monotone

!  arguments
      INTEGER k, npx
      REAL*8 vx(k), vy(k), px,py,eps_Xpt

!  variables locales
      INTEGER i
      REAL*8 dist1,dist2
!xpb      PARAMETER (eps=0.015)
!!!!  attention a ce facteur de 0.05. Xavier a du l'augmenter dans
!     certains cas.

!  procedures
      INTRINSIC SQRT

!=========================
!.. vx,vy: tableaux des coordonnees des points de la courbe.
!.. k  : nombre de points de la courbe.
!.. px,py: coordonnees du point X.
!.. dist1,dist2: distance entre un point de coordonnee et le point X.
!=========================
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
