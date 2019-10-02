!***********************************************************************
      LOGICAL FUNCTION chgdir(nivox,nivoy,nivxtm,nivytm)
!***********************************************************************
      use KindDefinitions
      IMPLICIT NONE

!..  Cette fonction verifie si 2 vecteurs sont de sens inverse ou non.

!  arguments
      REAL(rKind) :: nivox(*),nivoy(*),nivxtm(*),nivytm(*)

!  variables locales
      REAL(rKind) :: x1,x2,y1,y2,scal

!=========================
!.. nivox,nivoy: tableaux contenant les coordonnees des points du
!                premier vecteur.
!.. nivxtm,nivytm: tableaux contenant les coordonnees des points du
!                deuxieme vecteur.
!.. x1,y1,x2,y2: vecteurs 1 et 2
!.. scal: produit scalaire entre les vecteurs 1 et 2.
!=========================

      x1 = nivxtm(2) - nivxtm(1)
      x2 = nivox(2) - nivox(1)
      y1 = nivytm(2) - nivytm(1)
      y2 = nivoy(2) - nivoy(1)

      scal = x1*x2 + y1*y2

      IF (scal .LT. 0.) THEN
         chgdir = .TRUE.
      ELSE
         chgdir = .FALSE.
      ENDIF

      RETURN
      END
