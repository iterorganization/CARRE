      SUBROUTINE FRTIER(nx,ny,x,y,psi,nstruc, & 
     &          npstru,xstruc,ystruc,inddef,nbdef,npx,separx, & 
     &          separy,nptot,ptsep,racord,nivx,nivy,nivtot, & 
     &          nbniv,stp0,stpmin, & 
     &          distnv,ptxint,a00,a10,a01,a11)
!
!  version : 17.11.99 20:55
!
!======================================================================
!ank -- The comments are translated from French, sorry for errors!
!
!*** This subroutine parametrises the outermost level lines which go
!*** from one target to another not touching any other structure.
!*** Starting from a separatrix strike-point, the routine sets the
!*** direction of the search along the target and then calls MARCHE to
!*** find and parametrise the limiting level line.
!======================================================================
      IMPLICIT NONE

!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

!  arguments
      INTEGER nx,ny,nstruc,npstru(strumx), & 
     &        nbdef,inddef(nbdef),npx,nptot(4,npx), & 
     &        ptsep(4,npx),nivtot(nivmx),nbniv,ptxint
      REAL*8 x(nxmax),y(nymax),psi(nxmax,nymax),xstruc(npstmx,strumx), & 
     &     ystruc(npstmx,strumx),separx(npnimx,4,npxmx), & 
     &     separy(npnimx,4,npxmx),nivx(npnimx,nivmx),nivy(npnimx,nivmx), & 
     &     distnv(5,nivmx),a00(nxmax,nymax,3),a10(nxmax,nymax,3), & 
     &     a01(nxmax,nymax,3),a11(nxmax,nymax,3),stp0,stpmin
      LOGICAL racord

!  variables locales
      INTEGER ipx,idniv,fraplq,i,j,idef,nbcrb,sens,nt(2),ii,jj, & 
     &  ptxext,indlim
      REAL*8 x0,y0,x2,y2,psi0,psi2,xt(npnimx,2),yt(npnimx,2)

!  procedures
      INTEGER drctio,ifind
      REAL*8 plqdst
      INTRINSIC MOD
      EXTERNAL MARCHE,drctio,ifind,plqdst & 
     &        ,trc_stk_in,trc_stk_out
!======================================================================
!.. distnv: distance along the target from the separatrix strike-point
!           to the starting point of the limiting level line
!           (distance selector [1=real, 2=psi], curve index)
!.. nbniv : number of the limiting level lines
!.. nivx,nivy: coordinates of the points of the parametrised
!              limiting level lines (point index, curve index)
!.. nivtot: number of points for each parametrised limiting level line
!.. stp0  : initial step used for tracing the target
!.. stpmin: minimum to which the step can be reduced
!.. ipx   : X-point index
!.. idniv : level line index
!.. indlim: index of the limiting structure which stops the expansion
!           in the data region
!.. idef  : target index
!.. x0,y0 : coordinates of the separatrix strike-point
!.. x2,y2 : starting point of the level line
!.. psi0,psi2: psi values at the points 0 and 2
!.. ii,jj : cell index
!.. fraplq: index of the target to be intersected by the curve
!.. sens  : direction of movement along the target - 1=the same as the
!           target points, -1=opposite
!.. ptxint,ptxext: in the case of a disconnected double-null, indices
!                  of the internal and external X-points
!.. nbcrb : number of the limiting level lines separating the region
!.. xt,yt : points of the separating curve
!.. nt    : number of points in each curve
!.. a00,a10,a01,a11: bilinear interpolation coefficients
!          psi    = a00(,,1) + a10(,,1)*x + a01(,,1)*y + a11(,,1)*x*y
!          dpsidx = a00(,,2) + a10(,,2)*x + a01(,,2)*y + a11(,,2)*x*y
!          dpsidy = a00(,,3) + a10(,,3)*x + a01(,,3)*y + a11(,,3)*x*y
!======================================================================

!..Initialise the level lines

      DO 1 i=1, nivmx
         nivtot(i)=0
    1 CONTINUE

!..Determine the number of the separating lines (1 in every case)

      nbcrb = 1

!======================================================================
!..1.    Single null

      IF (npx .EQ. 1) THEN

         ipx = 1
         nbniv = 2

!..  Proceed region by region

!..1.1   Region 1 (SOL)

         idniv = 1

!..Indices of the starting and ending targets

         idef = 1
         fraplq = inddef(2)

!..Parametrisation of the separating curve

         nt(1)=0
       nt(2)=0

         DO 13 i=nptot(ptsep(1,ipx),ipx),1,-1
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(1,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(1,ipx),ipx)
   13    CONTINUE

         DO 14 i=2,nptot(ptsep(3,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(3,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(3,ipx),ipx)
   14    CONTINUE

         DO 15 i=2,nptot(ptsep(2,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(2,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(2,ipx),ipx)
   15    CONTINUE

!..Starting point

         x0 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y0 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Direction of the sweep

         call trc_stk_in('frtier','*15')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x0,y0,'droite')
         call trc_stk_out

!..Call the subroutine to find the limiting level line

         CALL MARCHE(x0,y0,inddef(idef),fraplq,sens,nivx(1,idniv) & 
     &      ,nivy(1,idniv),nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
     &      ystruc,a00,a10,a01,a11,indlim)

!..Initialise the distances

         distnv(1,idniv) = 0.
         distnv(2,idniv) = 0.

!..Calculate the absolute distance from the starting point to the
!  first point of the limiting level line

         x2 = nivx(1,idniv)
         y2 = nivy(1,idniv)

         distnv(1,idniv) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)), & 
     &                      ystruc(1,inddef(idef)),npstru(inddef(idef)) & 
     &                      ,'droite')

!..Calculate the psi difference between these two points.

         ii = ifind(x0,x,nx,1)
         jj = ifind(y0,y,ny,1)

         psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 + & 
     &          a11(ii,jj,1)*x0*y0

         ii = ifind(x2,x,nx,1)
         jj = ifind(y2,y,ny,1)

         psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

         distnv(2,idniv) = psi2 - psi0

!..1.2  Region 2 (PFR)

         idniv = 2

!..Indices of the starting and ending targets

         idef = 1
         fraplq = inddef(2)

!..Parametrisation of the separating curve

         nt(1)=0
       nt(2)=0

         DO 23 i=nptot(ptsep(1,ipx),ipx),1,-1
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(1,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(1,ipx),ipx)
   23    CONTINUE

         DO 24 i=2,nptot(ptsep(2,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(2,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(2,ipx),ipx)
   24    CONTINUE

!..Starting point

         x0 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y0 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Direction of the sweep.

         call trc_stk_in('frtier','*24')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x0,y0,'gauche')
         call trc_stk_out

!..Call the subroutine to find the limiting level line

         CALL MARCHE(x0,y0,inddef(idef),fraplq,sens,nivx(1,idniv) & 
     &      ,nivy(1,idniv),nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
     &      ystruc,a00,a10,a01,a11,indlim)

!..Initialise the distances

         distnv(1,idniv) = 0.
         distnv(2,idniv) = 0.

!..Calculate the absolute distance from the starting point to the
!  first point of the limiting level line

         x2 = nivx(1,idniv)
         y2 = nivy(1,idniv)

         distnv(1,idniv) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)), & 
     &                      ystruc(1,inddef(idef)),npstru(inddef(idef)) & 
     &                      ,'gauche')

!..Calculate the psi difference between these two points.

         ii = ifind(x0,x,nx,1)
         jj = ifind(y0,y,ny,1)

         psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 + & 
     &          a11(ii,jj,1)*x0*y0

         ii = ifind(x2,x,nx,1)
         jj = ifind(y2,y,ny,1)

         psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

         distnv(2,idniv) = psi2 - psi0

!======================================================================
!..2.  Connected double null

      ELSE IF ((npx .EQ. 2) .AND. (racord)) THEN

         nbniv = 4

!c<<<
!         write(0,*) 'FRTIER *..2: Connected double null'
!         write(0,*) 'inddef: ',inddef
!         write(0,'(1x,a16,1p,4e12.4)') 'separx(i,1)',
!     ,                        (separx(nptot(i,1),i,1),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separy(i,1)',
!     ,                        (separy(nptot(i,1),i,1),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separx(i,2)',
!     ,                        (separx(nptot(i,2),i,2),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separy(i,2)',
!     ,                        (separy(nptot(i,2),i,2),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separx(n,2)',
!     ,                      (separx(nptot(ptsep(i,1),1),
!     ,                                   ptsep(i,1),1),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separy(n,2)',
!     ,           (separy(nptot(ptsep(i,1),1),
!     ,                                   ptsep(i,1),1),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separx(n,1)',
!     ,           (separx(nptot(ptsep(i,2),2),
!     ,                                   ptsep(i,2),2),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separy(n,1)',
!     ,           (separy(nptot(ptsep(i,2),2),
!     ,                                   ptsep(i,2),2),i=1,4)
!         write(0,'(1x,a10,1p,6e12.4)') 'xstruc :',
!     ,                                         (xstruc(1,i),i=1,nstruc)
!         write(0,'(1x,a10,1p,6e12.4)') 'ystruc :',
!     ,                                         (ystruc(1,i),i=1,nstruc)
!         write(0,'(16h ptsep(isep,ixp)/(2i10))')
!     -                                       ((ptsep(i,j),j=1,2),i=1,4)
!         write(0,'(1x,a6,8(3x,a6,3x))') 'isep',
!     ,                            'xex(n)','yex(n)', 'xex(1)','yex(1)',
!     ,                            'xin(n)','yin(n)', 'xin(1)','yin(1)'
!         write(0,'(1x,i6,1p,8e12.4)') (i,
!     ,                    separx(nptot(ptsep(i,1),i),ptsep(i,1),i),
!     ,                    separy(nptot(ptsep(i,1),i),ptsep(i,1),i),
!     ,                    separx(1,ptsep(i,1),i),
!     ,                    separy(1,ptsep(i,1),i),
!     ,                    separx(nptot(ptsep(i,2),i),ptsep(i,2),i),
!     ,                    separy(nptot(ptsep(i,2),i),ptsep(i,2),i),
!     ,                    separx(1,ptsep(i,2),i),
!     ,                    separy(1,ptsep(i,2),i), i=1,4)
!c>>>

!..  Proceed region by region

!..2.1  Region 1 (right)

         idniv = 1

!..Indices of the starting and ending targets

         idef = 1
         fraplq = inddef(3)

!..Parametrisation of the separating curve

         nt(1)=0
       nt(2)=0

         ipx = 1

         DO 33 i=nptot(ptsep(1,ipx),ipx),1,-1
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(1,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(1,ipx),ipx)
   33    CONTINUE

         DO 34 i=2,nptot(ptsep(3,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(3,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(3,ipx),ipx)
   34    CONTINUE

         ipx = 2

         DO 35 i=2,nptot(ptsep(1,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(1,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(1,ipx),ipx)
   35    CONTINUE

!..Starting point

         ipx = 1
         x0 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y0 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Direction of the sweep.

         call trc_stk_in('frtier','*35')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x0,y0,'droite')
         call trc_stk_out

!..Call the subroutine to find the limiting level line

         CALL MARCHE(x0,y0,inddef(idef),fraplq,sens,nivx(1,idniv) & 
     &      ,nivy(1,idniv),nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
     &      ystruc,a00,a10,a01,a11,indlim)

!..Initialise the distances

         distnv(1,idniv) = 0.
         distnv(2,idniv) = 0.

!..Calculate the absolute distance from the starting point to the
!  first point of the limiting level line

         x2 = nivx(1,idniv)
         y2 = nivy(1,idniv)

         distnv(1,idniv) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)), & 
     &                      ystruc(1,inddef(idef)),npstru(inddef(idef)) & 
     &                      ,'droite')

!..Calculate the psi difference between these two points.

         ii = ifind(x0,x,nx,1)
         jj = ifind(y0,y,ny,1)

         psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 + & 
     &          a11(ii,jj,1)*x0*y0

         ii = ifind(x2,x,nx,1)
         jj = ifind(y2,y,ny,1)

         psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

         distnv(2,idniv) = psi2 - psi0

!..2.2  Region 2 (top PFR)

         idniv = 2

!..Indices of the starting and ending targets

         idef = 1
         fraplq = inddef(2)

!..Parametrisation of the separating curve

         nt(1)=0
       nt(2)=0

         ipx = 1

         DO 43 i=nptot(ptsep(1,ipx),ipx),1,-1
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(1,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(1,ipx),ipx)
   43    CONTINUE

         DO 44 i=2,nptot(ptsep(2,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(2,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(2,ipx),ipx)
   44    CONTINUE

!..Starting point

         ipx = 1
         x0 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y0 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Direction of the sweep.

         call trc_stk_in('frtier','*44')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x0,y0,'gauche')
         call trc_stk_out

!..Call the subroutine to find the limiting level line

         CALL MARCHE(x0,y0,inddef(idef),fraplq,sens,nivx(1,idniv) & 
     &      ,nivy(1,idniv),nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
     &      ystruc,a00,a10,a01,a11,indlim)

!..Initialise the distances

         distnv(1,idniv) = 0.
         distnv(2,idniv) = 0.

!..Calculate the absolute distance from the starting point to the
!  first point of the limiting level line

         x2 = nivx(1,idniv)
         y2 = nivy(1,idniv)

         distnv(1,idniv) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)), & 
     &                      ystruc(1,inddef(idef)),npstru(inddef(idef)) & 
     &                      ,'gauche')

!..Calculate the psi difference between these two points.

         ii = ifind(x0,x,nx,1)
         jj = ifind(y0,y,ny,1)

         psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 + & 
     &          a11(ii,jj,1)*x0*y0

         ii = ifind(x2,x,nx,1)
         jj = ifind(y2,y,ny,1)

         psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

         distnv(2,idniv) = psi2 - psi0

!..2.3  Region 3 (left)

         idniv = 3

!..Indices of the starting and ending targets

         idef = 2
         fraplq = inddef(4)

!..Parametrisation of the separating curve

         nt(1)=0
       nt(2)=0

         ipx = 1

         DO 53 i=nptot(ptsep(2,ipx),ipx),1,-1
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(2,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(2,ipx),ipx)
   53    CONTINUE

         DO 54 i=2,nptot(ptsep(4,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(4,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(4,ipx),ipx)
   54    CONTINUE

         ipx = 2

         DO 55 i=2,nptot(ptsep(2,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(2,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(2,ipx),ipx)
   55    CONTINUE

!..Starting point

         ipx = 1
         x0 = separx(nptot(ptsep(2,ipx),ipx),ptsep(2,ipx),ipx)
         y0 = separy(nptot(ptsep(2,ipx),ipx),ptsep(2,ipx),ipx)

!..Direction of the sweep.

         call trc_stk_in('frtier','*55')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x0,y0,'gauche')
         call trc_stk_out

!..Call the subroutine to find the limiting level line

         CALL MARCHE(x0,y0,inddef(idef),fraplq,sens,nivx(1,idniv) & 
     &      ,nivy(1,idniv),nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
     &      ystruc,a00,a10,a01,a11,indlim)

!..Initialise the distances

         distnv(1,idniv) = 0.
         distnv(2,idniv) = 0.

!..Calculate the absolute distance from the starting point to the
!  first point of the limiting level line

         x2 = nivx(1,idniv)
         y2 = nivy(1,idniv)

         distnv(1,idniv) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)), & 
     &                      ystruc(1,inddef(idef)),npstru(inddef(idef)) & 
     &                      ,'gauche')

!..Calculate the psi difference between these two points.

         ii = ifind(x0,x,nx,1)
         jj = ifind(y0,y,ny,1)

         psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 + & 
     &          a11(ii,jj,1)*x0*y0

         ii = ifind(x2,x,nx,1)
         jj = ifind(y2,y,ny,1)

         psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

         distnv(2,idniv) = psi2 - psi0

!..2.4  Region 4 (bottom PFR)

         idniv = 4

!..Indices of the starting and ending targets

         idef = 3
         fraplq = inddef(4)

!..Parametrisation of the separating curve

         nt(1)=0
       nt(2)=0

         ipx = 2

         DO 63 i=nptot(ptsep(1,ipx),ipx),1,-1
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(1,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(1,ipx),ipx)
   63    CONTINUE

         DO 64 i=2,nptot(ptsep(2,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(2,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(2,ipx),ipx)
   64    CONTINUE

!..Starting point

         ipx = 2
         x0 = separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y0 = separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Direction of the sweep.

         call trc_stk_in('frtier','*64')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x0,y0,'gauche')
         call trc_stk_out

!..Call the subroutine to find the limiting level line

         CALL MARCHE(x0,y0,inddef(idef),fraplq,sens,nivx(1,idniv) & 
     &      ,nivy(1,idniv),nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
     &      ystruc,a00,a10,a01,a11,indlim)

!..Initialise the distances

         distnv(1,idniv) = 0.
         distnv(2,idniv) = 0.

!..Calculate the absolute distance from the starting point to the
!  first point of the limiting level line

         x2 = nivx(1,idniv)
         y2 = nivy(1,idniv)

         distnv(1,idniv) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)), & 
     &                      ystruc(1,inddef(idef)),npstru(inddef(idef)) & 
     &                      ,'gauche')

!..Calculate the psi difference between these two points.

         ii = ifind(x0,x,nx,1)
         jj = ifind(y0,y,ny,1)

         psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 + & 
     &          a11(ii,jj,1)*x0*y0

         ii = ifind(x2,x,nx,1)
         jj = ifind(y2,y,ny,1)

         psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

         distnv(2,idniv) = psi2 - psi0


!======================================================================
!..3   Disconnected double null

      ELSE IF ((npx .EQ. 2) .AND. (.NOT.(racord))) THEN

         nbniv = 4
         ptxext = MOD(ptxint,2) + 1

!c<<<
!         write(0,*) 'FRTIER *..3: Disconnected double null'
!         write(0,*) 'ptxint, ptxext',ptxint, ptxext
!         write(0,*) 'inddef: ',inddef
!         write(0,'(1x,a16,1p,4e12.4)') 'separx(i,ext)',
!     ,                        (separx(nptot(i,ptxext),i,ptxext),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separy(i,ext)',
!     ,                        (separy(nptot(i,ptxext),i,ptxext),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separx(i,int)',
!     ,                        (separx(nptot(i,ptxint),i,ptxint),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separy(i,int)',
!     ,                        (separy(nptot(i,ptxint),i,ptxint),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separx(n,ext)',
!     ,                      (separx(nptot(ptsep(i,ptxext),ptxext),
!     ,                                   ptsep(i,ptxext),ptxext),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separy(n,ext)',
!     ,           (separy(nptot(ptsep(i,ptxext),ptxext),
!     ,                                   ptsep(i,ptxext),ptxext),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separx(n,int)',
!     ,           (separx(nptot(ptsep(i,ptxint),ptxint),
!     ,                                   ptsep(i,ptxint),ptxint),i=1,4)
!         write(0,'(1x,a16,1p,4e12.4)') 'separy(n,int)',
!     ,           (separy(nptot(ptsep(i,ptxint),ptxint),
!     ,                                   ptsep(i,ptxint),ptxint),i=1,4)
!         write(0,'(1x,a10,1p,(6e12.4))') 'xstruc :',
!     ,                                         (xstruc(1,i),i=1,nstruc)
!         write(0,'(1x,a10,1p,(6e12.4))') 'ystruc :',
!     ,                                         (ystruc(1,i),i=1,nstruc)
!         write(0,'(16h ptsep(isep,ixp)/(2i10))')
!     -                                       ((ptsep(i,j),j=1,2),i=1,4)
!         write(0,'(1x,a6,8(3x,a6,3x))') 'isep',
!     ,                            'xex(n)','yex(n)', 'xex(1)','yex(1)',
!     ,                            'xin(n)','yin(n)', 'xin(1)','yin(1)'
!         write(0,'(1x,i6,1p,8e12.4)') (i,
!     ,                    separx(nptot(ptsep(i,1),1),ptsep(i,1),1),
!     ,                    separy(nptot(ptsep(i,1),1),ptsep(i,1),1),
!     ,                    separx(1,ptsep(i,1),1),
!     ,                    separy(1,ptsep(i,1),1),
!     ,                    separx(nptot(ptsep(i,2),2),ptsep(i,2),2),
!     ,                    separy(nptot(ptsep(i,2),2),ptsep(i,2),2),
!     ,                    separx(1,ptsep(i,2),2),
!     ,                    separy(1,ptsep(i,2),2), i=1,4)
!c>>>

!..3.1  Region 1 (right)

         idniv = 1

!..Indices of the starting and ending targets

!ank-980619: they are always the same, independent of vertical shift
         IF (ptxint .EQ. 1) THEN
            idef = 1
            fraplq = inddef(3)
         ELSE IF (ptxint .EQ. 2) THEN
            idef = 3
            fraplq = inddef(1)
         ENDIF

!         idef = 1
!         fraplq = inddef(3)
!ank

!..Parametrisation of the separating curve

         nt(1)=0
       nt(2)=0

         DO 73 i=nptot(ptsep(3,ptxext),ptxext),1,-1
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(3,ptxext),ptxext)
            yt(nt(1),1)=separy(i,ptsep(3,ptxext),ptxext)
   73    CONTINUE

         DO 74 i=2,nptot(ptsep(1,ptxext),ptxext)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(1,ptxext),ptxext)
            yt(nt(1),1)=separy(i,ptsep(1,ptxext),ptxext)
   74    CONTINUE

!..Starting point

         x0=separx(nptot(ptsep(3,ptxext),ptxext),ptsep(3,ptxext),ptxext)
         y0=separy(nptot(ptsep(3,ptxext),ptxext),ptsep(3,ptxext),ptxext)

!..Direction of the sweep.

         call trc_stk_in('frtier','*74')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x0,y0,'droite')
         call trc_stk_out

!..Call the subroutine to find the limiting level line

         CALL MARCHE(x0,y0,inddef(idef),fraplq,sens,nivx(1,idniv) & 
     &      ,nivy(1,idniv),nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
     &      ystruc,a00,a10,a01,a11,indlim)

!..Initialise the distances

         distnv(1,idniv) = 0.
         distnv(2,idniv) = 0.

!..Calculate the absolute distance from the starting point to the
!  first point of the limiting level line

         x2 = nivx(1,idniv)
         y2 = nivy(1,idniv)

         distnv(1,idniv) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)), & 
     &                      ystruc(1,inddef(idef)),npstru(inddef(idef)) & 
     &                      ,'droite')

!..Calculate the psi difference between these two points.

         ii = ifind(x0,x,nx,1)
         jj = ifind(y0,y,ny,1)

         psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 + & 
     &          a11(ii,jj,1)*x0*y0

         ii = ifind(x2,x,nx,1)
         jj = ifind(y2,y,ny,1)

         psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

         distnv(2,idniv) = psi2 - psi0

!..3.2  Region 2 (top PFR)

         idniv = 2
         ipx = 1

!..Indices of the starting and ending targets

         idef = 1
         fraplq = inddef(2)

!..Parametrisation of the separating curve

         nt(1)=0
       nt(2)=0

         DO 83 i=nptot(ptsep(1,ipx),ipx),1,-1
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(1,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(1,ipx),ipx)
   83    CONTINUE

         DO 84 i=2,nptot(ptsep(2,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(2,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(2,ipx),ipx)
   84    CONTINUE

!..Starting point

         x0=separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y0=separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Direction of the sweep.

         call trc_stk_in('frtier','*84')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x0,y0,'gauche')
         call trc_stk_out

!..Call the subroutine to find the limiting level line

         CALL MARCHE(x0,y0,inddef(idef),fraplq,sens,nivx(1,idniv) & 
     &      ,nivy(1,idniv),nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
     &      ystruc,a00,a10,a01,a11,indlim)

!..Initialise the distances

         distnv(1,idniv) = 0.
         distnv(2,idniv) = 0.

!..Calculate the absolute distance from the starting point to the
!  first point of the limiting level line

         x2 = nivx(1,idniv)
         y2 = nivy(1,idniv)

         distnv(1,idniv) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)), & 
     &                      ystruc(1,inddef(idef)),npstru(inddef(idef)) & 
     &                      ,'gauche')

!..Calculate the psi difference between these two points.

         ii = ifind(x0,x,nx,1)
         jj = ifind(y0,y,ny,1)

         psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 + & 
     &          a11(ii,jj,1)*x0*y0

         ii = ifind(x2,x,nx,1)
         jj = ifind(y2,y,ny,1)

         psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

         distnv(2,idniv) = psi2 - psi0

!..3.3  Region 3 (left)

         idniv = 3

!..Indices of the starting and ending targets

         IF (ptxint .EQ. 1) THEN
            idef = 2
            fraplq = inddef(4)
         ELSE IF (ptxint .EQ. 2) THEN
            idef = 4
            fraplq = inddef(2)
         ENDIF

!..Parametrisation of the separating curve

         nt(1)=0
       nt(2)=0

         DO 93 i=nptot(ptsep(4,ptxext),ptxext),1,-1
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(4,ptxext),ptxext)
            yt(nt(1),1)=separy(i,ptsep(4,ptxext),ptxext)
   93    CONTINUE

         DO 94 i=2,nptot(ptsep(2,ptxext),ptxext)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(2,ptxext),ptxext)
            yt(nt(1),1)=separy(i,ptsep(2,ptxext),ptxext)
   94    CONTINUE

!..Starting point

         x0=separx(nptot(ptsep(4,ptxext),ptxext),ptsep(4,ptxext),ptxext)
         y0=separy(nptot(ptsep(4,ptxext),ptxext),ptsep(4,ptxext),ptxext)

!..Direction of the sweep.

         call trc_stk_in('frtier','*94')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x0,y0,'gauche')
         call trc_stk_out

!..Call the subroutine to find the limiting level line

         CALL MARCHE(x0,y0,inddef(idef),fraplq,sens,nivx(1,idniv) & 
     &      ,nivy(1,idniv),nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
     &      ystruc,a00,a10,a01,a11,indlim)

!..Initialise the distances

         distnv(1,idniv) = 0.
         distnv(2,idniv) = 0.

!..Calculate the absolute distance from the starting point to the
!  first point of the limiting level line

         x2 = nivx(1,idniv)
         y2 = nivy(1,idniv)

         distnv(1,idniv) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)), & 
     &                      ystruc(1,inddef(idef)),npstru(inddef(idef)) & 
     &                      ,'gauche')

!..Calculate the psi difference between these two points.

         ii = ifind(x0,x,nx,1)
         jj = ifind(y0,y,ny,1)

         psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 + & 
     &          a11(ii,jj,1)*x0*y0

         ii = ifind(x2,x,nx,1)
         jj = ifind(y2,y,ny,1)

         psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

         distnv(2,idniv) = psi2 - psi0

!..3.4  Region 4 (bottom PFR)

         idniv = 4
         ipx = 2

!..Indices of the starting and ending targets

         idef = 3
         fraplq = inddef(4)

!..Parametrisation of the separating curve

         nt(1)=0
       nt(2)=0

         DO 97 i=nptot(ptsep(1,ipx),ipx),1,-1
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(1,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(1,ipx),ipx)
   97    CONTINUE

         DO 98 i=2,nptot(ptsep(2,ipx),ipx)
            nt(1)=nt(1)+1
          if(nt(1).gt.npnimx) stop 'Increase npnimx'
            xt(nt(1),1)=separx(i,ptsep(2,ipx),ipx)
            yt(nt(1),1)=separy(i,ptsep(2,ipx),ipx)
   98    CONTINUE

!..Starting point

         x0=separx(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)
         y0=separy(nptot(ptsep(1,ipx),ipx),ptsep(1,ipx),ipx)

!..Direction of the sweep.

         call trc_stk_in('frtier','*98')
         sens = drctio(xstruc(1,inddef(idef)),ystruc(1,inddef(idef)), & 
     &                 npstru(inddef(idef)),x0,y0,'gauche')
         call trc_stk_out

!..Call the subroutine to find the limiting level line

         CALL MARCHE(x0,y0,inddef(idef),fraplq,sens,nivx(1,idniv) & 
     &      ,nivy(1,idniv),nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin, & 
     &      nx,ny,x,y,psi,nstruc,npstru,xstruc, & 
     &      ystruc,a00,a10,a01,a11,indlim)

!..Initialise the distances

         distnv(1,idniv) = 0.
         distnv(2,idniv) = 0.

!..Calculate the absolute distance from the starting point to the
!  first point of the limiting level line

         x2 = nivx(1,idniv)
         y2 = nivy(1,idniv)

         distnv(1,idniv) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)), & 
     &                      ystruc(1,inddef(idef)),npstru(inddef(idef)) & 
     &                      ,'gauche')

!..Calculate the psi difference between these two points.

         ii = ifind(x0,x,nx,1)
         jj = ifind(y0,y,ny,1)

         psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 + & 
     &          a11(ii,jj,1)*x0*y0

         ii = ifind(x2,x,nx,1)
         jj = ifind(y2,y,ny,1)

         psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

         distnv(2,idniv) = psi2 - psi0

      ENDIF

!c<<<
!      write(0,*) 'Leaving frtier'
!c>>>
      RETURN
!======================================================================
      END
