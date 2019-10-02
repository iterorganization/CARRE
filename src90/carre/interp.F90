
!***********************************************************************
      FUNCTION interp(r1, r2, f1, f2, f)
!***********************************************************************
      use KindDefinitions
      IMPLICIT NONE
      REAL(rKind) :: interp

!    Cette fonction renvoie la valeur de l'interpolation lineaire entre
!  les points r1 et r2 de la valeur de la fonction f, en connaissant la
!  valeur de la fonction en chaque point.

!  arguments
      REAL(rKind) :: r1, r2, f1, f2, f

!=========================
!.. r1,r2: points de references.
!.. f1,f2: valeur de la fonction en chacun de ces points.
!.. f  : valeur de la fonction pour laquelle on doit trouver la position
!=========================

      interp = r1 + (f - f1)*(r2 - r1)/(f2 - f1)

      RETURN
      END
