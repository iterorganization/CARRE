
************************************************************************
      subroutine intrsc(x1,y1,x2,y2,vx,vy,n,x,y,iint,nint,sens)
************************************************************************
      implicit none
#include <CARRETYPES.F>

*..  cette routine determine le point d'intersection entre un segment
c    1-2 et une courbe parametrisee de n points de coordonnees vx vy.

*  arguments
      integer n,iint(2),nint,sens
      real(kind=R8) x1,y1,x2,y2,vx(n),vy(n),x(2),y(2)

*  variables locales
      integer i,i1,i2,ii
      real(kind=R8) determ, mult1,mult2,zero,un,multmn,zx,zy
      parameter(zero=0.0_R8,un=1.0_R8)
c  procedures
      integer tourne
      external tourne

c=========================
c.. vx,vy: tableaux des coordonnees des points de la courbe.
c.. n  : nombre de point de la courbe.
c.. x1,y1,x2,y2: coordonnees des 2 points formant le segment.
c   x,y: tableaux de coordonnees des points d'intersection
c   iint: tableau des indices de segment de la courbe v pour lesquels il
c         y a intersection
c   nint: nombre d'intersections
c   sens: sens du balayage
c.. determ: determinant de la matrice des deux equations.
c.. mult1: facteur multiplicatif du segment 1-2
c.. mult2: facteur multiplicatif du segment de la courbe v.
c=========================
*..boucle sur tous les segments de la courbe.
      multmn=2.0_R8
      nint=0
      i2=1
      do 10 i=1, n-1
         i1=i2
         i2=tourne(i1,sens,n)
*..calcul du determinant de la matrice.
         determ = (-(x2 - x1)) * (vy(i2) - vy(i1)) +
     .               (y2 - y1) * (vx(i2) - vx(i1))
*..il y a solution seulement si le determinant n'est pas nul
         if (determ .ne. 0.) then
*..facteur multiplicatif du segment avec la methode de Cramer.
            mult1 = (-(vx(i1)-x1) * (vy(i2)-vy(i1)) +
     .                (vy(i1)-y1) * (vx(i2)-vx(i1)))/determ
*..pour avoir intersection, il faut que mult1 soit entre 0 et 1
            if ((mult1.ge.zero).and.(mult1.le.un)) then
*..fact. mult. du segment de la courbe v.
               mult2= ((x2-x1) * (vy(i1)-y1) -
     .                 (y2-y1) * (vx(i1)-x1))/determ
*..intersection si mult2 entre 0 et 1
               if ((mult2.ge.zero).and.(mult2.le.un)) then
*..s'il y a intersection alors le segment traverse la courbe.
                  nint=nint+1
                  iint(nint)=i1
                  x(nint)=x1+mult1*(x2-x1)
                  y(nint)=y1+mult1*(y2-y1)
                  if(mult1.lt.multmn) then
                    multmn=mult1
                    ii=iint(1)
                    zx=x(1)
                    zy=y(1)
                    iint(1)=iint(nint)
                    x(1)=x(nint)
                    y(1)=y(nint)
                    iint(nint)=ii
                    x(nint)=zx
                    y(nint)=zy
                 endif
               endif
            endif
         endif
   10 continue
      return
      end
