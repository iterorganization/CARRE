      subroutine limfnd(xpto,ypto,nivx,nivy,stp0,stpmin,distnv,nivtot,
     .    nbniv,nx,ny,x,y,psi,npx,ptx,pty,fctpx,
     .    nstruc,npstru,xstruc,ystruc,indplq,inddef,nbdef,
     .    a00,a10,a01,a11)
c
c  version : 07.07.97 20:10
c
c======================================================================
c  This routine finds the innermost point of the limiter. It defines
c  (ptx,pty), the coordinates of the X-point, to be the coordinates of
c  that point. Finally, it parametrises the separatrix (the flux surface
c  which goes through (ptx,pty) in curve nivx(*,1,1),nivy(*,1,1).
c======================================================================
      implicit none

cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>

c  arguments
      INTEGER nivtot(*),nbniv,
     .  nx,ny,npx,
     .  nstruc,npstru(strumx),
     .  indplq(4,npxmx),inddef(nbdmx),nbdef
      REAL(kind=R8) xpto,ypto,nivx(npnimx,*),nivy(npnimx,*),stp0,stpmin,
     .  distnv(5,*),a00(nxmax,nymax,3),a10(nxmax,nymax,3),
     .  a01(nxmax,nymax,3),a11(nxmax,nymax,3),
     .  x(nxmax),y(nymax),psi(nxmax,nymax),ptx(npxmx),pty(npxmx),
     .  fctpx(npxmx),xstruc(npstmx,strumx),ystruc(npstmx,strumx)
c
c  local variables
      real(kind=R8) zero
      parameter(zero=0.0_R8)
      integer dir,sens,nt(2),nbcrb,ind0,i,ii,jj,k,plaque,nstrp1,
     .  idef,idniv,indlim,ipx
      real(kind=R8) xt(npnimx,2),yt(npnimx,2),
     ,                                       x0,y0,psi0,x2,y2,psi2,norm
c
c  ind0: indice du point du limiteur qui est le plus rapproche de la
c        separatrice
c  x0, y0: coordonnees du point d'indice ind0

c
c  procedures
      integer ifind,horair
      real(kind=R8) aire,plqdst
      external aire,crbniv,horair,ifind,plqdst
      intrinsic sqrt
c
c  calculation
c
c  1.   definition of a line from the magnetic axis to a point outside
c       the separatrix
      if(nstruc.lt.strumx) then
        nstrp1=nstruc+1
        npstru(nstrp1)=3
        xstruc(1,nstrp1)=xpto
        ystruc(1,nstrp1)=ypto
        xstruc(2,nstrp1)=xpto
        ystruc(2,nstrp1)=max(y(1),y(ny))
        xstruc(3,nstrp1)=xpto
        ystruc(3,nstrp1)=1.0e20_R8
      else
        write(6,*)'Dimension of structures (strumx) must be increased.'
        write(6,*)'strumx, nstruc=',strumx,nstruc
        stop
      endif
c
c  2.   walk from point 1 towards point 2 until a structure is reached
      sens=1
      idniv=1
c       courbe a ne pas traverser
      nbcrb=1
      nt(1)=2
      nt(2)=0
      xt(1,nbcrb)=xpto
      yt(1,nbcrb)=ypto
      norm=sqrt((xstruc(2,nstrp1)-xstruc(1,nstrp1))**2+
     .  (ystruc(2,nstrp1)-ystruc(1,nstrp1))**2)
      xt(2,nbcrb)=xpto-1.e-2*stpmin*(ystruc(2,nstrp1)-ystruc(1,nstrp1))
     .  /norm
      yt(2,nbcrb)=ypto+1.e-2*stpmin*(xstruc(2,nstrp1)-xstruc(1,nstrp1))
     .  /norm

      CALL MARCHE(xpto,ypto,nstrp1,nstrp1,sens,nivx(1,idniv),
     .      nivy(1,idniv),
     .      nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin,
     .      nx,ny,x,y,psi,nstrp1,npstru,xstruc,
     .      ystruc,a00,a10,a01,a11,indlim)
      plaque=indlim
c
c  2.1  definition des indices de plaque pour usage dans maille
      indplq(1,1)=plaque
      indplq(2,1)=plaque
      inddef(1)=plaque
      inddef(2)=plaque

c
c  3.   On trouve le point du limiteur qui est le plus rapproche de la
c       derniere surface de flux parametrisee.
      call plurap(xstruc(1,plaque),ystruc(1,plaque),npstru(plaque),
     .  nivx(1,idniv),nivy(1,idniv),nivtot(idniv),x0,y0,ind0)
c
c  4.   parametrisation de la separatrice.
      ptx(1)=x0
      pty(1)=y0
      nbcrb=1
      nt(1)=0
      ii = ifind(x0,x,nx,1)
      jj = ifind(y0,y,ny,1)

      psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 +
     .       a11(ii,jj,1)*x0*y0

      ipx = 1
      dir = 0
      k = 1
      nbdef=0

      nivx(k,idniv)=x0
      nivy(k,idniv)=y0
      CALL CRBNIV(ii,jj,k,dir,nxmax,nymax,nx,ny,x,y,psi,psi2,
     .  nivx(1,idniv),nivy(1,idniv),npnimx,strumx,npstmx,
     .  nstruc,npstru,xstruc,ystruc,indlim,xt,yt,nt,nbcrb,plaque,x0,y0)
      call insert(indlim,inddef,nbdef,ipx)
      indplq(idniv,ipx) = indlim
      CALL CRBNIV(ii,jj,k,dir,nxmax,nymax,nx,ny,x,y,psi,psi2,
     .  nivx(1,idniv),nivy(1,idniv),npnimx,strumx,npstmx,
     .  nstruc,npstru,xstruc,ystruc,indlim,xt,yt,nt,nbcrb,plaque,x0,y0)
      nivtot(idniv)=k
      call insert(indlim,inddef,nbdef,ipx)
      indplq(idniv,ipx) = indlim

      if((nivx(1,idniv)-nivx(nivtot(idniv),idniv))**2
     .  +(nivy(1,idniv)-nivy(nivtot(idniv),idniv))**2. gt.
     .  2.*(x(2)-x(1))**2) then
        write(6,*)'attention: la courbe parametrisee est mal fermee.'
        print*,'nivtot=',nivtot(idniv)
        print*,'nivx/y(1)=',nivx(1,idniv),nivy(1,idniv)
        print*,'nivx/y(f)=',nivx(nivtot(idniv),idniv),
     .    nivy(nivtot(idniv),idniv)
      endif
      nivx(nivtot(idniv),idniv)=nivx(1,idniv)
      nivy(nivtot(idniv),idniv)=nivy(1,idniv)
      if(aire(nivx,nivy,nivtot(idniv)).lt.zero) then
        do i=1,nivtot(idniv)/2
          ii=nivtot(idniv)-i+1
          psi2=nivx(i,idniv)
          nivx(i,idniv)=nivx(ii,idniv)
          nivx(ii,idniv)=psi2
          psi2=nivy(i,idniv)
          nivy(i,idniv)=nivy(ii,idniv)
          nivy(ii,idniv)=psi2
        enddo
      endif
c
c  5.   Recherche de la frontiere exterieure
      idniv=2
      sens=1
      sens=horair(xpto,ypto,x0,y0,xstruc(1,plaque),ystruc(1,plaque),
     .  npstru(plaque),sens)
      nbcrb=1
      nt(nbcrb)=nivtot(1)
      do i=1,nt(nbcrb)
        xt(i,nbcrb)=nivx(i,1)
        yt(i,nbcrb)=nivy(i,1)
      enddo
      CALL MARCHE(nivx(1,1),nivy(1,1),plaque,plaque,sens,nivx(1,idniv),
     .      nivy(1,idniv),
     .      nivtot(idniv),nbcrb,xt,yt,nt,stp0,stpmin,
     .      nx,ny,x,y,psi,nstruc,npstru,xstruc,
     .      ystruc,a00,a10,a01,a11,indlim)

c
c  5.1  Calcul de la distance entre le point de contact et la derniere
c       surface de flux.

      idef=1

      distnv(1,idef) = 0.
      distnv(2,idef) = 0.

*..Calcul de la distance absolue entre le point de depart et le
*  premier point de la ligne de niveau frontiere.

      x2 = nivx(1,2)
      y2 = nivy(1,2)

      distnv(1,idef) = plqdst(x0,y0,x2,y2,xstruc(1,inddef(idef)),
     .                   ystruc(1,inddef(idef)),npstru(inddef(idef))
     .                   ,'droite')

*..Calcul de la difference de psi entre ces 2 memes points.

      ii = ifind(x0,x,nx,1)
      jj = ifind(y0,y,ny,1)

      psi0 = a00(ii,jj,1) + a10(ii,jj,1)*x0 + a01(ii,jj,1)*y0 +
     .       a11(ii,jj,1)*x0*y0

      ii = ifind(x2,x,nx,1)
      jj = ifind(y2,y,ny,1)

      psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 +
     .       a11(ii,jj,1)*x2*y2

      distnv(2,idef) = psi2 - psi0
c
c  6.   Definition du nombre de courbes de niveau calculees.
      nbniv=2
c
c  7.   Calcul de psi aux points X et O.
      ipx=npx
      ii=ifind(ptx(ipx),x,nx,1)
      jj=ifind(pty(ipx),y,ny,1)
      fctpx(ipx) = a00(ii,jj,1) + a10(ii,jj,1)*ptx(ipx)
     .           + a01(ii,jj,1)*pty(ipx)
     .           + a11(ii,jj,1)*ptx(ipx)*pty(ipx)
      ipx=npx+1
      ii=ifind(ptx(ipx),x,nx,1)
      jj=ifind(pty(ipx),y,ny,1)
      fctpx(ipx) = a00(ii,jj,1) + a10(ii,jj,1)*ptx(ipx)
     .           + a01(ii,jj,1)*pty(ipx)
     .           + a11(ii,jj,1)*ptx(ipx)*pty(ipx)

      return
      end
