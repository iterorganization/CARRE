
!***********************************************************************
      FUNCTION leng(vx,vy,n)
!***********************************************************************
      use KindDefinitions, only : rKind
      IMPLICIT NONE
      REAL(rKind) :: leng

!..  Cette fonction calcule la distance totale que parcourt une courbe v
!  de n points.

!  arguments
      INTEGER n
      REAL(rKind) :: vx(n),vy(n)

!  variables locales
      INTEGER i
      REAL(rKind) :: dist

!  procedures
      INTRINSIC SQRT

!=========================
!.. n   : nombre  de points de la courbe.
!.. vx,vy: vecteurs des coordonnees des points de la courbe.
!.. dist: distance entre 2 points consecutifs de la courbe.
!=========================

      leng = 0.0

      DO 10 i=1, n-1

         dist = SQRT((vx(i+1)-vx(i))**2 + (vy(i+1)-vy(i))**2)
         leng = leng + dist

   10 CONTINUE

      RETURN
      END
