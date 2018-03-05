
************************************************************************
      SUBROUTINE UNTANG(xn,yn,npni,x,y,ux,uy,d,period)
************************************************************************
      IMPLICIT NONE
#include <CARRETYPES.F>

*..  Cette sous-routine calcule le vecteur unite tangent a la courbe
*  parametrisee au point x,y. La direction de ce vecteur est obtenue par
*  interpolation lineaire avec les points parametrises adjacents.

* arguments
      INTEGER npni
      REAL(kind=R8) xn(npni), yn(npni), x, y, ux, uy,d,period

*  variables en common

#include <COMLAN.F>

*  variables locales
      INTEGER indp1, ind, jnd, jndp, ipp
      REAL(kind=R8) unx1,uny1,unx2,uny2,z,w1

*  procedures
      INTEGER indcrb
      EXTERNAL indcrb
      INTRINSIC SQRT

c=========================
c.. xn,yn: coordonnees des points de la courbe parametrisee.
c.. npni: nombre de points dans la courbe.
c.. x,y: coordonnees du point sur la courbe.
c.. ind : indice du point parametrise precedent le point de la courbe
c.. ux,uy: vecteur unite tangent a la courbe au point x,y.
c.. unx1,uny1: vecteur unite dependant de l'indice precedant.
c.. unx2,uny2: vecteur unite dependant de l'indice suivant.
c=========================

*..Calculs.

*..Initialisation.

      ind = indcrb(xn,yn,npni,x,y,d)

      indp1 = ind + 1
c
c  indice du point le plus proche
      if((xn(ind)-x)**2+(yn(ind)-y)**2 .lt.
     .  (xn(indp1)-x)**2+(yn(indp1)-y)**2) then
        ipp=ind
      else
        ipp=indp1
      endif
c
c  calcul du 1-er vecteur unite
      unx1=xn(indp1)-xn(ind)
      uny1=yn(indp1)-yn(ind)
      z=sqrt(unx1*unx1+uny1*uny1)
      unx1=unx1/z
      uny1=uny1/z
c
c  pas d'interpolation aux extremites
      if(ipp.eq.1 .or. ipp.eq.npni) then
        if(period.eq.0.) then
          ux=unx1
          uy=uny1
          return
        else
          if(ipp.eq.1) then
            jndp=npni
            jnd=npni-1
          else
            jndp=2
            jnd=1
          endif
        endif
      elseif(ipp.eq.ind) then
        jndp=ind
        jnd=ind-1
      elseif(ipp.eq.indp1) then
        jndp=ind+2
        jnd=indp1
      else
        if(sellan(1:8).eq.'francais') then
          write(6,*)'Cas impossible dans untang: arret force'
        elseif(sellan(1:7).eq.'english') then
          write(6,*)'Impossible case in untang: forced stop'
        endif
        call pltend
        stop
      endif
c
c  calcul du 2-e vecteur unite
      unx2=xn(jndp)-xn(jnd)
      uny2=yn(jndp)-yn(jnd)
      z=sqrt(unx2*unx2+uny2*uny2)
      unx2=unx2/z
      uny2=uny2/z

c
c  ponderation du 1-er vecteur et moyenne
      w1=0.5+sqrt(((xn(ipp)-x)**2+(yn(ipp)-y)**2)/
     .  ((xn(indp1)-xn(ind))**2+(yn(indp1)-xn(ind))**2))
      ux=w1*unx1+(1.-w1)*unx2
      uy=w1*uny1+(1.-w1)*uny2
      z=sqrt(ux*ux+uy*uy)
      ux=ux/z
      uy=uy/z
      return
      end
