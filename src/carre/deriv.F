
************************************************************************
      SUBROUTINE DERIV(nmax, r, f, fp, n)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..   Cette sous-routine calcule la derivee fp en chaque point r d'un
*  vecteur f.

*  arguments
      INTEGER nmax, n
      REAL(kind=R8) r(n), f(nmax), fp(nmax)

*  variables locales
      INTEGER i
      REAL(kind=R8) r2,r3,f2,f3

c=========================
c.. r  : tableau de la coordonnee des points.
c.. f  : tableau de la valeur de la fonction en chacun de ces points.
c.. n  : nombre de points dans chaque tableau.
c.. fp : tableau de la valeur de la fonction derivee par rapport a r
c        en chacun de ces points.
c.. r2 : distance entre les 2 prermiers (ou derniers) points.
c.. r3 : distance entre les 3 prermiers (ou derniers) points.
c.. f2 : difference de psi entre les 2 premiers (ou derniers) points.
c.. f3 : difference de psi entre les 3 premiers (ou derniers) points.
c=========================

*..Debut du calcul

*..Calcul des derivees a partir de la valeur de la fonction de chacun
*  des deux points adjacents.

      DO 10 i=2, n-1

         fp(i) = ((f(i+1)-f(i))*(r(i)-r(i-1))/(r(i+1)-r(i))
     .  + (f(i)-f(i-1))*(r(i+1)-r(i))/(r(i)-r(i-1))) /(r(i+1)-r(i-1))

   10 CONTINUE

*..Determination de la derivee du premier et du dernier point.

      r2=r(2)-r(1)
      r3=r(3)-r(1)
      f2=f(2)-f(1)
      f3=f(3)-f(1)
      fp(1)=(f2*r3*r3-f3*r2*r2)/(r2*r3*(r3-r2))
      r2=r(n-1)-r(n)
      r3=r(n-2)-r(n)
      f2=f(n-1)-f(n)
      f3=f(n-2)-f(n)
      fp(n)=(f2*r3*r3-f3*r2*r2)/(r2*r3*(r3-r2))

      RETURN
      END
