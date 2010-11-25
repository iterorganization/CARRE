
!***********************************************************************
      LOGICAL FUNCTION milieu(f1, f2, niv)
!***********************************************************************
      IMPLICIT NONE

!    Cette fonction verifie si la valeur d'une fonction niv est presente
!  entre deux valeurs de fonction: f1 et f2.

!  arguments
      REAL*8 f1, f2, niv

!=========================
!.. f1,f2: valeurs de la fonction aux points 1 et 2.
!.. niv: fonction pour laquelle on doit verifier si elle est entre
!         f1 et f2.
!=========================

      IF ((f1.LE.niv .AND. f2.GE.niv) .OR. & 
     &      (f1.GE.niv .AND. f2.LE.niv)) THEN
         milieu = .TRUE.
      ELSE
         milieu = .FALSE.
      ENDIF

      RETURN
      END
