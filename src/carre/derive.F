      SUBROUTINE DERIVE(nx, ny, x, y, psi, psidx, psidy)
c
c  version : 07.07.97 19:37
c
c======================================================================
*..  Cette sous-routine calcule en chaque point ou psi est defini, la
*  derivee selon x et la derivee selon y.
c======================================================================

      IMPLICIT NONE

cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>

*  arguments
      INTEGER nx, ny
      REAL(kind=R8)
     .  psi(nxmax,nymax), psidx(nxmax,nymax), psidy(nxmax,nymax),
     .  x(nx),y(ny)

*  variables locales
      INTEGER i, j, nxymax
      parameter (nxymax=nymax)
      REAL(kind=R8) tmp(nxymax), tmpp(nxymax)

*  procedures
      EXTERNAL DERIV

c=========================
c.. x,y: tableaux des coordonnees des points.
c.. nx,ny: nombre de points dans chaque tableau.
c.. psi: tableau de la valeur de la fonction en chacun de ces points.
c.. psidx: tableau de la valeur de la fonction derivee par rapport a x
c        en chacun de ces points.
c.. psidy: tableau de la valeur de la fonction derivee par rapport a y
c        en chacun de ces points.
c.. i,j: indices
c.. tmp: tableau selon une ligne ou une colonne de psi.
c.. tmpp: tableau de la meme ligne ou colonne de derivee de psi par
c         rapport a x ou y.
c=========================

*
*.. 1  Calcul des derivees partielles par rapport a X.
*

*..Boucle sur toutes les lignes.

      DO 10 j=1, ny

*..Copie du vecteur (ligne) dans un vecteur temporaire.

         DO 15 i=1, nx
            tmp(i) = psi(i,j)
   15    CONTINUE

*..Calcul de la derivee de ce vecteur.

         CALL DERIV(nxmax, x, tmp, tmpp, nx)

*..Copie du vecteur de derivees par rapport a x temporaire dans le
*  vecteur de derivee par rapport a X: psidx

         DO 20 i=1, nx
            psidx(i,j) = tmpp(i)
   20    CONTINUE

   10 CONTINUE

*
*.. 2  Calcul des derivees partielles par rapport a Y.
*

*..Boucle sur toutes les colonnes.

      DO 30 i=1, nx

*..Copie du vecteur (colonne) dans un vecteur temporaire.

         DO 35 j=1, ny
            tmp(j) = psi(i,j)
   35    CONTINUE

*..Calcul de la derivee de ce vecteur.

         CALL DERIV(nymax, y, tmp, tmpp, ny)

*..Copie du vecteur de derivees par rapport a Y temporaire dans le
*  vecteur de derivee par rapport a Y: psidy

         DO 40 j=1, ny
            psidy(i,j) = tmpp(j)
   40    CONTINUE

   30 CONTINUE

      RETURN
      END
