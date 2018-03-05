
************************************************************************
      integer function horair(xpto,ypto,x0,y0,xstruc,ystruc,npst,sens)
************************************************************************
      implicit none
#include <CARRETYPES.F>
c  Determination du sens de parcours "sens" sur une structure donnee,
c  a partir d'un point de depart x0, y0, pour que le trajet initial
c  soit dans le sens horaire (sens=-1) ou anti-horaire (sens=1) tel que
c  vu du point de reference xpto, ypto.
c  N.B.: la structure est supposee fermee.
c
c  arguments
      integer npst,sens
      real(kind=R8) xpto,ypto,x0,y0,xstruc(npst),ystruc(npst)
c
c  variables locales
      integer ind,im,ip
      real(kind=R8) v0x,v0y,norm,vmx,vmy,vpx,vpy
c
c  procedures
      integer indsgm
      external indsgm
      intrinsic sqrt
c
c  calcul
c
c  1.   on trouve d'abord l'indice du segment sur lequel se trouve le
c       point de depart.
      ind=indsgm(xstruc,ystruc,npst,x0,y0)

      if(x0.eq.xstruc(ind) .and. y0.eq.ystruc(ind)) then
        if(ind.eq.1) then
          im=npst-1
          ip=ind+1
        elseif(ind.eq.npst-1) then
          im=ind-1
          ip=1
        else
          im=ind-1
          ip=ind+1
        endif
      elseif(x0.eq.xstruc(ind+1) .and. y0.eq.ystruc(ind+1)) then
        if(ind.eq.npst-1) then
          im=ind
          ip=2
        else
          im=ind
          ip=ind+2
        endif
      else
        im=ind
        ip=ind+1
      endif
      v0x=x0-xpto
      v0y=y0-ypto

      vmx=xstruc(im)-x0
      vmy=ystruc(im)-y0
      norm=sqrt(vmx*vmx+vmy*vmy)
      vmx=vmx/norm
      vmy=vmy/norm

      vpx=xstruc(ip)-x0
      vpy=ystruc(ip)-y0
      norm=sqrt(vpx*vpx+vpy*vpy)
      vpx=vpx/norm
      vpy=vpy/norm

      if((v0x*vmy-v0y*vmx).gt.(v0x*vpy-v0y*vpx)) then
        horair=-sens
      else
        horair=sens
      endif

      return
      end
