
************************************************************************
      LOGICAL FUNCTION milieu(f1, f2, niv)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*    Cette fonction verifie si la valeur d'une fonction niv est presente
*  entre deux valeurs de fonction: f1 et f2.

*  arguments
      REAL(kind=R8) f1, f2, niv

c=========================
c.. f1,f2: valeurs de la fonction aux points 1 et 2.
c.. niv: fonction pour laquelle on doit verifier si elle est entre
c         f1 et f2.
c=========================

      IF ((f1.LE.niv .AND. f2.GE.niv) .OR.
     .      (f1.GE.niv .AND. f2.LE.niv)) THEN
         milieu = .TRUE.
      ELSE
         milieu = .FALSE.
      ENDIF

      RETURN
      END
