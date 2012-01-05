
!***********************************************************************
      integer function horair(xpto,ypto,x0,y0,xstruc,ystruc,npst,sens)
!***********************************************************************
      use carre_target

      implicit none
!  Determination du sens de parcours "sens" sur une structure donnee,
!  a partir d'un point de depart x0, y0, pour que le trajet initial
!  soit dans le sens horaire (sens=-1) ou anti-horaire (sens=1) tel que
!  vu du point de reference xpto, ypto.
!  N.B.: la structure est supposee fermee.
!
!  arguments
      integer npst,sens
      real*8 xpto,ypto,x0,y0,xstruc(npst),ystruc(npst)
!
!  variables locales
      integer ind,im,ip
      real*8 v0x,v0y,norm,vmx,vmy,vpx,vpy
!
!  procedures
      intrinsic sqrt
!
!  calcul
!
!  1.   on trouve d'abord l'indice du segment sur lequel se trouve le
!       point de depart.
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
