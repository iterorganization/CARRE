
************************************************************************
      subroutine clort(x1,y1,x2,y2,ort,nppol,pasmin,g1,g2,l0,l2,
     .        	ortpur,propo,varr,tot)
************************************************************************
      implicit none
#include <CARRETYPES.F>
c  calcul des fonctions qui s'annulent quand deux series de points sont
c  disposees de facon "orthogonale".
c
c  arguments
      integer nppol
      real(kind=R8) x1(nppol),y1(nppol),x2(nppol),y2(nppol),ort(nppol),
     .  pasmin,g1,g2,l0(nppol),l2(nppol),ortpur(nppol),propo(nppol),
     .  varr(nppol),
     .  tot(nppol)
c
c  x1, y1: coordonnees des points de maille de reference (sur la courbe
c          precedente
c  x2, y2: coordonnees des points de maille a ajuster
c  ort: tableau de la fonction dont il faut trouver le zero
c  pasmin: plus petit espacement entre deux points de maille
c  g1, g2: gardes gauche et droite
c  l0: tableau des positions initiales
c  l2: tableau des positions actuelles
c
c  variables locales
      integer i
      real(kind=R8) cs1,cs2,cs3,cs4,l1m,l1p,l2m,l2p,l12,zero,
     .  fac1,fac2,fac,l12t,un
      parameter(zero=0._R8,un=1._R8)
c
c  procedures
      intrinsic sqrt
c
c  calculs
      i=1
      l1p=sqrt((x1(i)-x1(i+1))**2+(y1(i)-y1(i+1))**2)
      l2p=sqrt((x2(i)-x2(i+1))**2+(y2(i)-y2(i+1))**2)
c  on trouve empiriquement que la convergence est meilleure avec une
c  valeur de l12 constante.
      l12t=sqrt((x2(i)-x1(i))**2+(y2(i)-y1(i))**2)
      if(g1.gt.zero .and. g2.gt.zero) then
        do i=2,nppol-1
          fac1=(g1/(g1+l0(i)))**2
          fac2=(g2/(g2+l0(nppol)-l0(i)))**2
          fac=fac1*(un-fac2)+fac2
          l12=fac*l0(nppol)+(1.-fac)*l12t
c
          l1m=l1p
          l2m=l2p
          l1p=sqrt((x1(i)-x1(i+1))**2+(y1(i)-y1(i+1))**2)
          l2p=sqrt((x2(i)-x2(i+1))**2+(y2(i)-y2(i+1))**2)
          cs1=(x1(i)-x2(i))*(x2(i-1)-x2(i))
     .       +(y1(i)-y2(i))*(y2(i-1)-y2(i))
          cs2=(x1(i)-x2(i))*(x2(i+1)-x2(i))
     .       +(y1(i)-y2(i))*(y2(i+1)-y2(i))
          cs3=(x2(i)-x1(i))*(x1(i-1)-x1(i))
     .       +(y2(i)-y1(i))*(y1(i-1)-y1(i))
          cs4=(x2(i)-x1(i))*(x1(i+1)-x1(i))
     .       +(y2(i)-y1(i))*(y1(i+1)-y1(i))

          cs1=cs1/(l2m*l12)
          cs2=cs2/(l2p*l12)
          cs3=cs3/(l1m*l12)
          cs4=cs4/(l1p*l12)
          ort(i)=
     .       cs2+cs3-cs1-cs4
     .      -((g1/l0(i))**2+(g2/(l0(nppol)-l0(i)))**2)
     .      *(l2(i)-l0(i)/l0(nppol)*l2(nppol))/(pasmin+g1+g2)
     .      +(pasmin/(l2(i)-l2(i-1)))**2-(pasmin/(l2(i+1)-l2(i)))**2
          ortpur=cs2+cs3-cs1-cs4
          propo(i)= - ((g1/l0(i))**2+(g2/(l0(nppol)-l0(i)))**2)
     .      *(l2(i)-l0(i)/l0(nppol)*l2(nppol))/(pasmin+g1+g2)
          varr(i)= (pasmin/(l2(i)-l2(i-1)))**2-
     .             (pasmin/(l2(i+1)-l2(i)))**2
          tot(i)= ortpur(i) + propo(i) + varr(i)
        enddo
      else
        do i=2,nppol-1
          l12=l12t
c
          l1m=l1p
          l2m=l2p
          l1p=sqrt((x1(i)-x1(i+1))**2+(y1(i)-y1(i+1))**2)
          l2p=sqrt((x2(i)-x2(i+1))**2+(y2(i)-y2(i+1))**2)
          cs1=(x1(i)-x2(i))*(x2(i-1)-x2(i))
     .       +(y1(i)-y2(i))*(y2(i-1)-y2(i))
          cs2=(x1(i)-x2(i))*(x2(i+1)-x2(i))
     .       +(y1(i)-y2(i))*(y2(i+1)-y2(i))
          cs3=(x2(i)-x1(i))*(x1(i-1)-x1(i))
     .       +(y2(i)-y1(i))*(y1(i-1)-y1(i))
          cs4=(x2(i)-x1(i))*(x1(i+1)-x1(i))
     .       +(y2(i)-y1(i))*(y1(i+1)-y1(i))
          
          cs1=cs1/(l2m*l12)
          cs2=cs2/(l2p*l12)
          cs3=cs3/(l1m*l12)
          cs4=cs4/(l1p*l12)
          ort(i)=
     .       cs2+cs3-cs1-cs4
     .      +(pasmin/(l2(i)-l2(i-1)))**2-(pasmin/(l2(i+1)-l2(i)))**2
          ortpur=cs2+cs3-cs1-cs4
          propo(i)=  ((g1/l0(i))**2+(g2/(l0(nppol)-l0(i)))**2)
     .      *(l2(i)-l0(i)/l0(nppol)*l2(nppol))/(pasmin+g1+g2)
          varr(i)= (pasmin/(l2(i)-l2(i-1)))**2-
     .             (pasmin/(l2(i+1)-l2(i)))**2
          tot(i)= ortpur(i)- propo(i) + varr(i)
        enddo
      endif
c
      return
      end
