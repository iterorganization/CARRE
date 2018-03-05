************************************************************************
      LOGICAL FUNCTION chgdir(nivox,nivoy,nivxtm,nivytm)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette fonction verifie si 2 vecteurs sont de sens inverse ou non.

*  arguments
      REAL(kind=R8) nivox(*),nivoy(*),nivxtm(*),nivytm(*)

*  variables locales
      REAL(kind=R8) x1,x2,y1,y2,scal

c=========================
c.. nivox,nivoy: tableaux contenant les coordonnees des points du
c                premier vecteur.
c.. nivxtm,nivytm: tableaux contenant les coordonnees des points du
c                deuxieme vecteur.
c.. x1,y1,x2,y2: vecteurs 1 et 2
c.. scal: produit scalaire entre les vecteurs 1 et 2.
c=========================

      x1 = nivxtm(2) - nivxtm(1)
      x2 = nivox(2) - nivox(1)
      y1 = nivytm(2) - nivytm(1)
      y2 = nivoy(2) - nivoy(1)

      scal = x1*x2 + y1*y2

      IF (scal .LT. 0.0_R8) THEN
         chgdir = .TRUE.
      ELSE
         chgdir = .FALSE.
      ENDIF

      RETURN
      END
