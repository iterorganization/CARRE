
!***********************************************************************
      FUNCTION ruban(xn,yn,n,x1,y1,d)
!***********************************************************************
      use KindDefinitions, only : rKind
      IMPLICIT NONE
      REAL(rKind) :: ruban

!..  Cette fonction retourne la distance entre le debut de la courbe et
!  le point x1,y1.

!  arguments
      INTEGER n
      REAL(rKind) :: x1,y1,xn(n),yn(n),d

!  variables locales
      INTEGER ind
      REAL(rKind) :: dist

!  procedures
      INTEGER indcrb
      REAL(rKind) :: long
      INTRINSIC SQRT
      EXTERNAL indcrb, long

!=========================
!.. xn,yn: tableaux des coordonnees des points de la courbe.
!.. n  : nombre de points de la courbe.
!.. x1,y1: coordonnees du point.
!=========================

      ruban = 0.

      ind = indcrb(xn,yn,n,x1,y1,d)

      dist = long(xn(1:ind),yn(1:ind),ind)

      ruban = dist + SQRT((x1-xn(ind))**2 + (y1-yn(ind))**2)

      RETURN
      END
