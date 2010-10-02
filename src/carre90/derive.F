      SUBROUTINE DERIVE(nx, ny, x, y, psi, psidx, psidy)
!
!  version : 07.07.97 19:37
!
!======================================================================
!..  Cette sous-routine calcule en chaque point ou psi est defini, la
!  derivee selon x et la derivee selon y.
!======================================================================

      IMPLICIT NONE

!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

!  arguments
      INTEGER nx, ny
      REAL*8 psi(nxmax,nymax), psidx(nxmax,nymax), psidy(nxmax,nymax), & 
     &  x(nx),y(ny)

!  variables locales
      INTEGER i, j, nxymax
      parameter (nxymax=nymax)
      REAL*8 tmp(nxymax), tmpp(nxymax)

!  procedures
      EXTERNAL DERIV

!=========================
!.. x,y: tableaux des coordonnees des points.
!.. nx,ny: nombre de points dans chaque tableau.
!.. psi: tableau de la valeur de la fonction en chacun de ces points.
!.. psidx: tableau de la valeur de la fonction derivee par rapport a x
!        en chacun de ces points.
!.. psidy: tableau de la valeur de la fonction derivee par rapport a y
!        en chacun de ces points.
!.. i,j: indices
!.. tmp: tableau selon une ligne ou une colonne de psi.
!.. tmpp: tableau de la meme ligne ou colonne de derivee de psi par
!         rapport a x ou y.
!=========================

!
!.. 1  Calcul des derivees partielles par rapport a X.
!

!..Boucle sur toutes les lignes.

      DO 10 j=1, ny

!..Copie du vecteur (ligne) dans un vecteur temporaire.

         DO 15 i=1, nx
            tmp(i) = psi(i,j)
   15    CONTINUE

!..Calcul de la derivee de ce vecteur.

         CALL DERIV(nxmax, x, tmp, tmpp, nx)

!..Copie du vecteur de derivees par rapport a x temporaire dans le
!  vecteur de derivee par rapport a X: psidx

         DO 20 i=1, nx
            psidx(i,j) = tmpp(i)
   20    CONTINUE

   10 CONTINUE

!
!.. 2  Calcul des derivees partielles par rapport a Y.
!

!..Boucle sur toutes les colonnes.

      DO 30 i=1, nx

!..Copie du vecteur (colonne) dans un vecteur temporaire.

         DO 35 j=1, ny
            tmp(j) = psi(i,j)
   35    CONTINUE

!..Calcul de la derivee de ce vecteur.

         CALL DERIV(nymax, y, tmp, tmpp, ny)

!..Copie du vecteur de derivees par rapport a Y temporaire dans le
!  vecteur de derivee par rapport a Y: psidy

         DO 40 j=1, ny
            psidy(i,j) = tmpp(j)
   40    CONTINUE

   30 CONTINUE

      RETURN
      END
