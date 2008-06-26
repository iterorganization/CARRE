      INTEGER FUNCTION drctio(xst,yst,npst,x1,y1,doug)
c
c  version : 30.06.98 17:18
c
c======================================================================
      IMPLICIT NONE

*..Determination de la direction anti-horaire (dir=+1) ou horaire (-1)
*  qui correspond a un deplacement vers la droite ou vers la gauche
*  le long d'une structure, a partir d'un point x,y

*  arguments
      INTEGER npst
      REAL*8 xst(npst),yst(npst),x1,y1
      CHARACTER*(*) doug

*  variables locales
      INTEGER ind,nrot,irot
      REAL*8 sg,delta,xin,yin,xin1,yin1,zero,norm,pi,cs,sn
      parameter(delta=1.e-4,zero=0.,nrot=10,pi=3.141592654)

*  procedures
      intrinsic sqrt,sin,cos
      INTEGER indsgm
      logical in
      EXTERNAL indsgm,in
     ,         ,trc_stk

c<<<
      integer n_call
      logical l_dbg
      data n_call /0/, l_dbg /.false./
c>>>

c=========================
c.. xst,yst: tableaux des coordonnees des points de la structure.
c.. npst: nombre de points de la structure.
c.. x1,y1: point de depart sur la structure.
c.. doug: drote ou gauche
c.. ind: indice du segment de structure.
c=========================
*  calculs

      n_call=n_call+1
cc<<<
c      l_dbg=n_call.le.4
c      if(l_dbg) then
c        write(0,*) '==== Entering drctio: ',n_call
c        write(0,*) 'doug,npst,x1,y1: ',doug,' ',npst,x1,y1
c        write(0,'(5h xst:,1p,7e12.4/(5x,7e12.4))') xst
c        write(0,'(5h yst:,1p,7e12.4/(5x,7e12.4))') yst
c      end if
cc>>>

cank-19980618
      cs=cos((2.*pi)/nrot)
      sn=sin((2.*pi)/nrot)
cank

      ind=indsgm(xst,yst,npst,x1,y1)

      IF (doug(1:1).EQ.'d' .OR. doug(1:1).EQ.'D') then
         sg=+1
      else
         sg=-1
      end if
      if(y1.gt.zero) sg=-sg
cc<<<
c      if(l_dbg) then
c        write(0,*) 'ind,sg =',ind,sg
c      end if
cc>>>
      IF (xst(ind).NE.x1 .or. yst(ind).ne.y1) then
c  on trouve d'abord les coordonnees d'un point interieur
        xin=y1-yst(ind)
        yin=xst(ind)-x1
        norm=sqrt(xin*xin+yin*yin)
        xin=delta*xin/norm
        yin=delta*yin/norm
cc<<<
c        if(l_dbg) then
c          write(0,*) 'xin,yin,norm=',xin,yin,norm
c          write(0,'(1x,a4,4(5x,a6,3x))') 'irot','x1+xin','y1+yin',
c     ,                                            'xin','yin'
c        end if
cc>>>
        do 2 irot=1,nrot
cc<<<
c          if(l_dbg) then
c            write(0,'(1x,i3,1x,1p,4e14.6)') irot,
c     ,                                      x1+xin,y1+yin,xin,yin
c          end if
cc>>>
          if(in(x1+xin,y1+yin,xst,yst,npst)) then
            go to 4
          else
cank-19980618
c            cs=cos((2.*pi*irot)/nrot)
c            sn=sin((2.*pi*irot)/nrot)
cank
            norm=xin
            xin=xin*cs-yin*sn
            yin=norm*sn+yin*cs
          end if
  2     continue
c        write(0,*)'on ne trouve pas de point interieur dans drctio'
        write(0,*) 'Interior point not found in drctio ',
     ,                                  '- an internal error in Carre?'
        write(0,*) '*2: n_call,x1,y1=',n_call,x1,y1
        write(0,'(5h xst:,1p,7e12.4/(5x,7e12.4))') xst
        write(0,'(5h yst:,1p,7e12.4/(5x,7e12.4))') yst
        call trc_stk
        write(0,*) '==> Check whether the structures representing ',
     ,                                         'the targets are closed'
        call pltend
        stop
  4     continue
c  on teste maintenant l'orientation
        if(sg*(xin*(y1-yst(ind))-yin*(x1-xst(ind))).gt.zero) then
          drctio=1
        else
          drctio=-1
        end if
      else if(xst(ind+1).NE.x1 .or. yst(ind+1).ne.y1) then
        xin1=xst(ind+1)-x1
        yin1=yst(ind+1)-y1
        norm=sqrt(xin1*xin1+yin1*yin1)
        xin1=xin1/norm
        yin1=yin1/norm
        if(ind.eq.1) then
          irot=npst-1
        else
          irot=ind-1
        end if
        xin=xst(irot)-x1
        yin=yst(irot)-y1
        norm=sqrt(xin*xin+yin*yin)
        xin=xin/norm+xin1
        yin=yin/norm+yin1
        norm=sqrt(xin*xin+yin*yin)
        xin=delta*xin/norm
        yin=delta*yin/norm

        do 6 irot=1,nrot
          if(in(x1+xin,y1+yin,xst,yst,npst)) then
            go to 8
          else
cank-19980618
c            cs=cos((2.*pi*irot)/nrot)
c            sn=sin((2.*pi*irot)/nrot)
cank
            norm=xin
            xin=xin*cs-yin*sn
            yin=norm*sn+yin*cs
          end if
  6     continue
c        write(0,*)'on ne trouve pas de point interieur dans drctio'
        write(0,*) 'Interior point not found in drctio ',
     ,                                  '- an internal error in Carre?'
        write(0,*) '*6: n_call,x1,y1=',n_call,x1,y1
        write(0,'(5h xst:,1p,7e12.4/(5x,7e12.4))') xst
        write(0,'(5h yst:,1p,7e12.4/(5x,7e12.4))') yst
        call trc_stk
        write(0,*) '==> Check whether the structures representing ',
     ,                                         'the targets are closed'
        call pltend
        stop
  8     continue
c  on teste maintenant l'orientation
        if(sg*(xin*(y1-yst(ind))-yin*(x1-xst(ind))).gt.zero) then
          drctio=1
        else
          drctio=-1
        end if
      else
c        PRINT*,'Segment degenere detecte dans drctio: fin du programme'
        call pltend
        STOP 'Degenerated segment detected in drctio: the program stops'
      end if
      RETURN
      END
