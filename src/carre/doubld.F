      SUBROUTINE DOUBLD(bouclx,boucly,xn1,yn1,nn1,pas,
     .  nprad,plaque,xext,yext,xptxex,yptxex,xpto,ypto,nx,ny,x,y,
     .  psi,nstruc,npstru,xstruc,ystruc,a00,a10,a01,
     .  a11,repart,xcrb2,ycrb2,npcrb2)
c
c  version : 08.07.98 21:05
c
c======================================================================
cank -- The comments are translated from French, sorry for errors!

c*** This subroutine finds, in the case of a disconnected double null,
c*** the point on the loop of the inner separatrix where it should be
c*** split into two separatrices (thus forcing a mesh point to be at
c*** the outer X-point)
c======================================================================
      IMPLICIT NONE

c======================================================================
c a faire:
c  Definition of xmail, ymail before and after a call to doubld.
c  Do not forget the guard!

c  N.B. Here, the attempt to work with the relaxation scheme (everything
c       starting and ending with c---) has been abandoned. The approach
c       presently used is considered to be the best one in practice.
c       The code between c--- has also been commented with "crlx".
c======================================================================

cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>

*  arguments
      INTEGER nx,ny,nstruc,npstru(nstruc),
     .        nn1,nprad,plaque,repart,npcrb2

      REAL(kind=R8)
     .     x(nxmax),y(nymax),psi(nxmax,nymax),xstruc(npstmx,nstruc),
     .     ystruc(npstmx,nstruc),bouclx,boucly,xptxex,yptxex,xn1(nn1),
     .     yn1(nn1),pas(nrmamx),xext,yext,a00(nxmax,nymax,3),
     .     a10(nxmax,nymax,3),a01(nxmax,nymax,3),a11(nxmax,nymax,3),
     .     xcrb2(npcrb2),ycrb2(npcrb2),
     .     xpto,ypto

*  variables en common

#include <COMORT.F>
#include <COMRLX.F>

*  variables locales
      INTEGER ipas,indstr,ianc,inouv,ind,ii,jj,ir,dir,i,nn(2),crbnmx,
     .        npcrb(2),nbcrb,nrmx,sens,iint(2),nint,sns
      REAL(kind=R8)
     .       ll,zero,pasini,epsmai,dist,dernie,valfct,ecart1,ecart2,
     .       epsiln,xn(npnimx,2),yn(npnimx,2),fctini,fctnew,x2,y2
      PARAMETER(zero=0.0_R8,epsmai=1.0e-6_R8,epsiln=1.0E-08_R8,
     ,                                       crbnmx=npnimx,nrmx=nrmamx)
      REAL(kind=R8) xcrb(crbnmx,2),ycrb(crbnmx,2),xbcl(nrmx),ybcl(nrmx),
     .       xint(2),yint(2)
      logical lc,li,l_dbg
      integer n_call
#ifdef DBG
      data l_dbg /.true./
#else
      data l_dbg /.false./
#endif
      data n_call /0/

*  procedures
      INTEGER indsgm,ifind,drctio
      REAL(kind=R8) aazero,long,nulort,ruban
      LOGICAL chgdir,in,cross
      INTRINSIC MOD,SQRT
      EXTERNAL aazero,long,COORD,indsgm,ifind,CRBNIV,nulort,
     .         UNTANG,SAUTE,chgdir,in,cross,ruban,drctio
     ,        ,trc_stk_in,trc_stk_out,trc_stk
c======================================================================
c*** Input
c.. xn1,yn1: points of parametrised inner separatrix
c.. nn1    : number of these points
c.. pas    : radial steps (distances or psi values) for the surfaces
c..          between the inner and outer separatrices
c.. nppol  : number of grid points between the X-points in poloidal
c..          direction
c.. nprad  : number of grid surfaces between the inner and outer
c..          separatrices
c.. plaque : index of the leading target
c.. [xy]ext: coordinates of the strike point of the outer separatrix
c..            on the leading target
c.. [xy]ptxex: coordinates of the outer X-point
c.. [xy]pto: coordinates of the O-point
c.. x,y    : coordinates of the grid on which the psi values are given
c.. nx,ny  : their dimensions
c.. psi    : psi values
c.. nstruc : number of the structures
c.. npstru : numbers of segments in the structures
c.. [xy]struc: coordinates of the structure corners
c.. a00..a11: interpolation coefficients for psi
c.. repart : selector of the radial grid distribution (1=l, 2=psi)
c.. [xy]crb2: points of parametrised outer separatrix
c.. npcrb2 : number of these points

c*** Output
c.. boucl[xy]: coordinates of the splitting point on the separatrix
c..            loop (???)

c======================================================================
*
*..calculs
*
      n_call=n_call+1
cc<<<
      l_dbg=l_dbg.and.n_call.le.4
      if(l_dbg) then
        write(0,*) '===> Entering doubld. n_call = ',n_call
        write(0,'(1x,2a5,6a7,a9,3a12)') 'nx','ny','nstruc','nn1',
     ,        'nprad','plaque','npcrb2','repart',
     ,        'xext','yext','xptxex','yptxex'
        write(0,'(1x,2i5,6i7,1p,4e12.4)') nx,ny,nstruc,nn1,nprad,
     ,                     plaque,npcrb2,repart,xext,yext,xptxex,yptxex
        write(0,'(1x,a6,1p,8e12.4/(7x,8e12.4))') 'pas: ',
     ,                                               (pas(i),i=1,nprad)
      end if
cc>>>

*..Copy the arguments into the local variables

      x2=xext
      y2=yext

*..Direction of circulation

      call trc_stk_in('doubld','*5')
      sens=drctio(xstruc(1,plaque),ystruc(1,plaque),npstru(plaque),
     .       x2,y2,'gauche')
      call trc_stk_out
cc<<<
      if(l_dbg) write(0,*) 'After drctio: sens = ',sens
cc>>>

*..Define the second boundary curve

      nbcrb=2

      do 5 i=1, npcrb2
         xcrb(i,2)=xcrb2(i)
         ycrb(i,2)=ycrb2(i)
    5 CONTINUE
      npcrb(2)=npcrb2

*..definition de period pour le bloc comort

      period=zero

*..Define the first (reference) boundary curve and its length

      do 10 i=1,nn1
         xn(i,1)=xn1(i)
         yn(i,1)=yn1(i)
   10 CONTINUE

      nn(1)=nn1
      ll=long(xn(1,1),yn(1,1),nn(1))

*..Calculate the psi value at the first point of the reference curve

      ii=ifind(xn(1,1),x,nx,1)
      jj=ifind(yn(1,1),y,ny,1)

      fctini=a00(ii,jj,1) + a10(ii,jj,1)*xn(1,1) + 
     .       a01(ii,jj,1)*yn(1,1) + a11(ii,jj,1)*xn(1,1)*yn(1,1)

      valfct=fctini
cc<<<
      if(l_dbg) then
        write(0,*) '*10: ll,ii,jj,fctini =',ll,ii,jj,fctini
      end if
cc>>>

*..Identify xbcl and ybcl for the first curve

      xbcl(nprad)=xptxex
      ybcl(nprad)=yptxex

*..Search successively for the points xbcl,ybxl

      ianc=2
      inouv=1
      do 25 ir=nprad-1,2,-1
         ianc=MOD(ianc,2) + 1
         inouv=MOD(inouv,2) + 1
         x1=x2
         y1=y2
         ll1=ll
cc<<<
      if(l_dbg) then
        write(0,'(a8,2a7,a9,4a12)')
     -           ' ---- ir','ianc','inouv','ll','x1','x2','y1','y2'
        write(0,'(1x,3i7,1p,5e12.4)') ir,ianc,inouv,ll,x1,x2,y1,y2
      end if
cc>>>

*..Find the starting point of the new level line

         IF (repart .EQ. 1) then

            fctnew=pas(ir)

c            call trc_stk_in('doubld','repart=1')
c            CALL SAUTE(xstruc(1,plaque),ystruc(1,plaque),
c     .                npstru(plaque),x1,y1,valfct,x2,y2,pas(ir),sens,
c     .                repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)
c            call trc_stk_out

         else if (repart .EQ. 2) then

            fctnew=valfct - pas(ir)

c            call trc_stk_in('doubld','repart=2')
c            CALL SAUTE(xstruc(1,plaque),ystruc(1,plaque),
c     .                npstru(plaque),x1,y1,valfct,x2,y2,fctnew,sens,
c     .                repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)
c            call trc_stk_out

         end if

         call trc_stk_in('doubld','*12')
         CALL SAUTE(xstruc(1,plaque),ystruc(1,plaque),
     .                npstru(plaque),x1,y1,valfct,x2,y2,fctnew,sens,
     .                repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)
         call trc_stk_out

         xn(1,inouv)=x2
         yn(1,inouv)=y2
         nn(inouv)=1

         ind=indsgm(xstruc(1,plaque),ystruc(1,plaque),
     ,                                            npstru(plaque),x2,y2)
cc<<<
         if(l_dbg) then
           write(0,'(1x,a12,a4,a9,5a12)') 'After SAUTE:',
     ,                      'ind','x2','y2','valfct','fctnew','x1','y1'
           write(0,'(13x,i4,1p,6e12.4)') ind,x2,y2,valfct,fctnew,x1,y1
         end if
cc>>>

*..Parametrise the level line passing through this point

         ii=ifind(x2,x,nx,1)
         jj=ifind(y2,y,ny,1)

         valfct=a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 +
     .          a11(ii,jj,1)*x2*y2

*..Define the first boundary curve

         do 14 i=1, nn(ianc)
            xcrb(i,1)=xn(i,ianc)
            ycrb(i,1)=yn(i,ianc)
   14    CONTINUE

         npcrb(1)=nn(ianc)

*..Find the second point

         dir=0

         CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi,
     .             valfct,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx,
     .             nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,
     .             nbcrb,plaque,x2,y2)

*..Make sure that the level line departs in the right direction

         ecart1=SQRT((x2 - xstruc(ind,plaque))**2 +
     .               (y2 - ystruc(ind,plaque))**2)
         ecart2=SQRT((x2 - xstruc(ind+1,plaque))**2 +
     .               (y2 - ystruc(ind+1,plaque))**2)
cc<<<
         if(l_dbg) then
           write(0,'(2x,6a4,a9,3a12)') 'ind','dir','ii','nx','jj','ny',
     ,                                      'x2','y2','ecart1','ecart2'
           write(0,'(1x,6i4,1p,4e12.4)') ind,dir,ii,nx,jj,ny,
     ,                                              x2,y2,ecart1,ecart2
           write(0,'(a10,7a12)')  'xstruc','ystruc',
     ,                'xstruc+','ystruc+','x(nx)','y(ny)','x(1)','y(1)'
           write(0,'(1x,1p,8e12.4)')
     ,           xstruc(ind,plaque),ystruc(ind,plaque),
     ,           xstruc(ind+1,plaque),ystruc(ind+1,plaque),
     ,           x(nx),y(ny),x(1),y(1)
         end if
cc>>>

         IF ((ABS(x2-x(nx)).LT.epsiln) .OR. (ABS(y2-y(ny)).LT.epsiln)
     .     .OR. (ABS(x2-x(1)).LT.epsiln) .OR. (ABS(y2-y(1)).LT.epsiln))
     .                                                         then

            IF (((dir .EQ. 1) .AND. (ii .EQ. nx))
     .       .OR. ((dir .EQ. 2) .AND. (jj .EQ. ny))
     .       .OR. ((dir .EQ. 3) .AND. (ii .EQ. 1))
     .       .OR. ((dir .EQ. 4) .AND. (jj .EQ. 1))) then

               nn(inouv)=1
               dir=MOD(dir+1,4) + 1
               ii=ii - MOD(dir-2,2)
               jj=jj - MOD(dir-3,2)

            end if

         else if ((ecart1 .LT. epsiln) .OR. (ecart2 .LT. epsiln)) then

            IF (chgdir(xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc)))
     .                                                          then

               nn(inouv)=1
               dir=MOD(dir+1,4) + 1
               ii=ii - MOD(dir-2,2)
               jj=jj - MOD(dir-3,2)

            end if

         else

c            IF (chgdir(xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc)))
c     .                                                           THEN

          lc=chgdir(xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc))
cc<<<
c          write(0,'(2x,3a7,a9,3a12)') 'chgdir','inouv','ianc',
c     ,                           'xn(o)','yn(o)','xn(a)','yn(a)'
c          write(0,'(1x,l7,2i7,1p,4e12.4)') lc,
c     ,         inouv,ianc,xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc)
cc>>>
          if(lc) then
            li=in(xn(2,inouv),yn(2,inouv),xstruc(1,plaque),
     .                  ystruc(1,plaque),npstru(plaque))
            lc=cross(ind,xn(1,inouv),yn(1,inouv),
     .                    xstruc(1,plaque),ystruc(1,plaque),
     .                    npstru(plaque))
cc<<<
c               write(0,'(1x,7a7,4a12)') 'in','cross','ind',
c     ,                'plaque','npstru','inouv','xn1','yn1','xn2','yn2'
c               write(0,'(1x,2l7,4i7,1p,4e12.4)') li,lc,ind,plaque,
c     ,                    npstru(plaque),inouv,xn(1,inouv),yn(1,inouv),
c     ,                                         xn(2,inouv),yn(2,inouv)
cc>>>

c               IF ((in(xn(2,inouv),yn(2,inouv),xstruc(1,plaque),
c     .                  ystruc(1,plaque),npstru(plaque)))
c     .            .OR. (cross(ind,xn(1,inouv),yn(1,inouv),
c     .                    xstruc(1,plaque),ystruc(1,plaque),
c     .                    npstru(plaque)))) then

               if(li .or. lc) then
                  nn(inouv)=1
                  dir=MOD(dir+1,4) + 1
                  ii=ii - MOD(dir-2,2)
                  jj=jj - MOD(dir-3,2)

               else

                  write(*,*) 'Problem in doubld (n_call =',n_call,') ',
     ,                                '- may well be an internal error'
                  write(*,*) 'The program cannot find the location of',
     ,                             ' the next flux surface on a target'
                  write(*,*) '==> If a file "carre.dat" from an old ',
     ,              'case exists in this directory, try the following.'
                  write(*,*) '    First, replace all assignments to ',
     ,              'npr(i) in the input part of the file, ',
     ,              'after "$parameters,"'
                  write(*,*) '    by npr(i)=2  (here i means 1,2,...).'
                  write(*,*) '    If this does not help, remove - or ',
     ,              'rename - the file "carre.dat" and try again'

                  call trc_stk
                  call pltend
                  STOP '==== first STOP'

               end if

            else

               li=in(xn(2,inouv),yn(2,inouv),xstruc(1,plaque),
     ,                  ystruc(1,plaque),npstru(plaque))
               lc=cross(ind,xn(1,inouv),yn(1,inouv),xstruc(1,plaque),
     ,                               ystruc(1,plaque),npstru(plaque))
cc<<<
c               write(0,'(1x,7a7,4a12)') 'in','cross','ind',
c     ,                'plaque','npstru','inouv','xn1','yn1','xn2','yn2'
c               write(0,'(1x,2l7,4i7,1p,4e12.4)') li,lc,ind,plaque,
c     ,                    npstru(plaque),inouv,xn(1,inouv),yn(1,inouv),
c     ,                                         xn(2,inouv),yn(2,inouv)
cc>>>
               if(li .or. lc) then
c               IF ((in(xn(2,inouv),yn(2,inouv),xstruc(1,plaque),
c     .                  ystruc(1,plaque),npstru(plaque)))
c     .            .OR. (cross(ind,xn(1,inouv),yn(1,inouv),
c     .                    xstruc(1,plaque),ystruc(1,plaque),
c     .                    npstru(plaque)))) THEN

                  write(*,*) 'Problem in doubld (n_call =',n_call,')'
                  write(*,*) '==> If a file "carre.dat" from an old ',
     ,                 'case exists in this directory, try to remove ',
     ,                                                       'it first'
                  call trc_stk
                  call pltend
                  STOP '==== second STOP'

               end if
            end if
         end if

*..Continue with successive points until striking a structure

         CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi,
     .             valfct,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx,
     .             nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,
     .             nbcrb,plaque,x2,y2)

*..Definition de xn2 et yn2 pour le bloc common comort.

         do 18 ipas=1,nn(inouv)
            xn2(ipas)=xn(ipas,inouv)
            yn2(ipas)=yn(ipas,inouv)
   18    CONTINUE

         npni2=nn(inouv)

*..On definit maintenant le point tangent au point X exterieur de la
*  nouvelle ligne de niveau a partir de celui de la precedente.

         dernie=zero
         ll=long(xn2,yn2,npni2)
           d1=zero

*..Definition de x1, y1, ux1,uy1 et d1 pour le bloc common comort.

           x1=xbcl(ir+1)
           y1=ybcl(ir+1)

           CALL UNTANG(xn(1,ianc),yn(1,ianc),nn(ianc),x1,y1,ux1,uy1,
     .                  d1,period)

           d1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),x1,y1,d1)

*..Calculate the intersection between the segment connecting the
*  external X-point with the O-point and the innermost level line

           sns=1
           call intrsc(x1,y1,xpto,ypto,xn(1,inouv),yn(1,inouv),
     .       nn(inouv),xint,yint,iint,nint,sns)
           if(nint.gt.0) then
             d1=zero
             d1=ruban(xn(1,inouv),yn(1,inouv),nn(inouv),xint(nint),
     .          yint(nint),d1)
             dernie=0.75*d1
             pasini=min(ll,1.25*d1)-dernie
             dist=aazero(nulort,dernie,pasini,epsmai,zero,dernie,
     .         dernie+pasini,50)
           else
             dernie=0.55*ll
             pasini=0.3*ll
             dist=aazero(nulort,dernie,pasini,epsmai,zero,dernie,ll,50)
           end if

           CALL COORD(xn(1,inouv),yn(1,inouv),nn(inouv),dist,
     .              xbcl(ir),ybcl(ir))

   25 CONTINUE

*..Find the point at the inner separatrix

*..Definition de xn2 et yn2 pour le bloc common comort

      do 30 ipas=1,npcrb2

         xn2(ipas)=xcrb2(ipas)
         yn2(ipas)=ycrb2(ipas)

   30 CONTINUE

      npni2=npcrb2

      ll1=ll

*..On definit maintenant le point sur la boucle de la separatrice
*  interieure qui divisera cette separatrice en deux.

      dernie=zero
      ll=long(xn2,yn2,npni2)
        d1=zero

*..Definition de x1, y1, ux1,uy1 et d1 pour le bloc common comort.

        x1=xbcl(2)
        y1=ybcl(2)
        CALL UNTANG(xn(1,inouv),yn(1,inouv),nn(inouv),x1,y1,ux1,uy1,
     .                  d1,period)
        d1=ruban(xn(1,inouv),yn(1,inouv),nn(inouv),x1,y1,d1)

        sns=1
        call intrsc(x1,y1,xpto,ypto,xn(1,inouv),yn(1,inouv),
     .    nn(inouv),xint,yint,iint,nint,sns)
        if(nint.gt.0) then
          d1=zero
          d1=ruban(xn(1,inouv),yn(1,inouv),nn(inouv),xint(nint),
     .       yint(nint),d1)
          dernie=0.75*d1
          pasini=min(ll,1.25*d1)-dernie
          dist=aazero(nulort,dernie,pasini,epsmai,zero,dernie,
     .      dernie+pasini,50)
        else
          dernie=0.45*ll
          pasini=0.3*ll
          dist=aazero(nulort,dernie,pasini,epsmai,zero,dernie,ll,50)
        end if
        CALL COORD(xn2,yn2,npni2,dist,bouclx,boucly)

cc<<<
c      write(0,*) '<=== Leaving doubld'
cc>>>
      RETURN
c======================================================================
      END
