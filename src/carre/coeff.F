************************************************************************
      SUBROUTINE COEFF(n1max,n2max,nx,ny,x,y,f,a00,a10,a01,a11)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..   Cette sous-routine calcule les coefficients a00, a10, a01 et a11
*  pour chaque carre de coordonnees (i,j) de l'equation :
*         f(x,y) = a00 + a10*X + a01*Y + a11*XY.

*  arguments
      INTEGER n1max, n2max, nx, ny
      REAL(kind=R8)
     .     x(nx),y(ny),f(n1max,ny),a00(n1max,n2max),a10(n1max,n2max),
     .     a01(n1max,n2max), a11(n1max,n2max)

*  variables locales
      INTEGER i,j
      REAL(kind=R8) deltxy

c=========================
c.. i,j: indices
c.. f  : <=> psi
c.. a00,a10,a01,a11: coefficients.(ind. selon x, ind. selon y)
c.. deltxy: distance entre 2 points selon x, multiplie par la
c           distance entre 2 points selon y.
c=========================
*
*.. Debut du calcul.
*
      DO 20 j=1, ny-1
         DO 21 i=1, nx-1
            deltxy=(x(i+1)-x(i))*(y(j+1)-y(j))

            a00(i,j) = (f(i,j)*x(i+1)*y(j+1) - f(i+1,j)*x(i)*y(j+1)
     +         - f(i,j+1)*x(i+1)*y(j) + f(i+1,j+1)*x(i)*y(j))/deltxy

            a10(i,j) = (-f(i,j)*(y(j+1)) + f(i+1,j)*y(j+1) +
     +                f(i,j+1)*(y(j)) - f(i+1,j+1)*y(j))/deltxy

            a01(i,j) = (-f(i,j)*(x(i+1)) + f(i+1,j)*x(i) +
     +                f(i,j+1)*(x(i+1)) - f(i+1,j+1)*(x(i)))/deltxy

            a11(i,j) = (f(i,j) - f(i+1,j) - f(i,j+1) +
     +                f(i+1,j+1))/deltxy

   21    CONTINUE
   20 CONTINUE

      RETURN
      END
