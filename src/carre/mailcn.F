      SUBROUTINE MAILCN(mailx,maily,xn1,yn1,nn1,pntrat,pas,nppol,nprad,
     .               x2,y2,xfin,yfin,fctini,nx,ny,x,y,psi,
     .               nstruc,npstru,xstruc,ystruc,
     .               a00,a10,a01,a11,repart,xptxo,yptxo,xpto,ypto,
     .               nivx,nivy,nivtot,nbniv,distxo)
c
c  version : 07.07.97 18:47
c
c======================================================================
      IMPLICIT NONE

*..  Cette sous-routine fait le maillage curviligne orthogonal dans
*  la region centrale.

*  variables en common

cank-970707: dimensions from the file
*  dimensions
#include <CARREDIM.F>

#include <COMORT.F>
#include <COMLAN.F>
#include <COMRLX.F>

*  arguments
      INTEGER nx,ny,nstruc,npstru(nstruc),
     .  nn1,nppol,nprad,repart,nivtot(nivmx),nbniv

      REAL*8 x(nxmax),y(nymax),psi(nxmax,nymax),xstruc(npstmx,nstruc),
     .  ystruc(npstmx,nstruc),mailx(npmamx,nrmamx),
     .  maily(npmamx,nrmamx),xn1(nn1),yn1(nn1),pas(nrmamx),x2,y2,
     .  xfin,yfin,fctini,a00(nxmax,nymax,3),a10(nxmax,nymax,3),
     .  a01(nxmax,nymax,3),a11(nxmax,nymax,3),
     .  xptxo,yptxo,xpto,ypto,nivx(npnimx,nivmx),
     .  nivy(npnimx,nivmx),pntrat,distxo

*  variables locales
      INTEGER ipas,indstr,ianc,inouv,ii,jj,ir,dir,ipol,i,nn(2),nt,sens
     .        ,npcrb(2),plaque
      REAL*8 ll,zero,pasini,epsmai,dist,dernie,valfct,xn(npnimx,2),
     .       yn(npnimx,2),fctnew,xt(3),yt(3)
      PARAMETER(zero=0.,epsmai=1.e-6)
      REAL*8 xcrb(npnimx,2),ycrb(npnimx,2)
      CHARACTER*1 reponse

*  procedures
      INTEGER ifind
      REAL*8 aazero,long,nulort,ruban
      INTRINSIC MOD,int,abs
      EXTERNAL aazero,long,COORD1,ifind,CRBNIV,nulort,UNTANG,ruban
c
c  remarque: cette routine doit etre appelee apres frtier ou, dans le
c            cas d'une configuration limiteur, apres limfnd car elle
c            suppose que nbniv associe aux frontieres exterieures est
c            deja connu. La derniere frontiere est donc la frontiere
c            interieure.
c======================================================================
*
*..calculs
*
c***
c     print*,'entree dans mailcn: nxmax, ny, npstmx, nstruc= ',
c    .  nxmax,ny,npstmx,nstruc
c     print*,'npmamx,nrmamx, nn1, nxmax,nymax,nivmx=',
c    ,       npmamx,nrmamx,nn1,nxmax,nm2,
c    .  nivmx
c     print*,'npnimx=',npnimx
c***

*..Initialisation.

      plaque=0

      DO 10 i=1,nn1
         xn(i,1)=xn1(i)
         yn(i,1)=yn1(i)
   10 CONTINUE

      nn(1)=nn1

      IF (repart .EQ. 2) THEN

         valfct=fctini

         xt(1)=xn(1,1)
         yt(1)=yn(1,1)
         xt(2)=xfin
         yt(2)=yfin
         xt(3)=xt(1)
         yt(3)=yt(1)
         nt=3

      ENDIF

      DO 25 ir=2, nprad

c---
        print*,'ir=',ir
c---
        if(pntrat.ge.distxo .and. ir.eq.nprad) then
          do 12 ipol=1,nppol
          mailx(ipol,ir)=xpto
          maily(ipol,ir)=ypto
   12     continue

        else

          ianc=MOD(ir,2) + 1
          inouv=MOD(ir-1,2) + 1

          x1=x2
          y1=y2

          IF (repart .EQ. 1) THEN

            x2=x1 + xptxo*pas(ir-1)
            y2=y1 + yptxo*pas(ir-1)

          ELSE IF (repart .EQ. 2) THEN

            fctnew=valfct + pas(ir-1)
            sens=1

            CALL SAUTE(xt,yt,nt,x1,y1,valfct,x2,y2,fctnew,sens,
     .               repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

          ENDIF

*..Parametrisation de la ligne de niveau qui passe par ce point.

          xn(1,inouv)=x2
          yn(1,inouv)=y2
          nn(inouv)=1

          ii=ifind(x2,x,nx,1)
          jj=ifind(y2,y,ny,1)

          valfct=a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 +
     .           a11(ii,jj,1)*x2*y2

*..Definition de la courbe a ne pas traverser.

          DO 14 i=1, nn(ianc)
           xcrb (i,1)=xn(i,ianc)
           ycrb (i,1)=yn(i,ianc)
   14     CONTINUE

          npcrb(1)=nn(ianc)
          npcrb(2)=0

*..Recherche du deuxieme point.

          dir=0

c***
c         print*,'call crbniv1 - mailcn'
c***
          CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi,
     .            valfct,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx,
     .            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1,
     .            plaque,x2,y2)

*..Il faut s'assurer que la ligne de niveau part dans la bonne
*  direction.

c0195     IF (xn(2,inouv) .LT. xn(1,inouv)) THEN
          if((xn(2,inouv)-xn(1,inouv))*(xn(2,ianc)-xn(1,ianc))+
     .          (yn(2,inouv)-yn(1,inouv))*(yn(2,ianc)-yn(1,ianc))
     .          .lt.zero) then
            nn(inouv)=1
            dir=MOD(dir+1,4) + 1
            ii=ii - MOD(dir-2,2)
            jj=jj - MOD(dir-3,2)
          ENDIF
*..Pour les points successifs, on poursuit jusqu'a ce qu'on ferme la
*  boucle.
c***
c         print*,'call crbniv2 - mailcn'
c***

          CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi,
     .            valfct,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx,
     .            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1,
     .            plaque,x2,y2)

*..Le dernier point de la courbe est egal au premier.

          xn(nn(inouv),inouv)=xn(1,inouv)
          yn(nn(inouv),inouv)=yn(1,inouv)

*..Definition de xn2 et yn2 pour le bloc common comort.

          DO 18 ipas=1,nn(inouv)
            xn2(ipas)=xn(ipas,inouv)
            yn2(ipas)=yn(ipas,inouv)
   18     CONTINUE

          npni2=nn(inouv)

*..On definit maintenant les points de maille de la nouvelle ligne
*  de niveau a partir de ceux de la precedente.

          ll=long(xn2,yn2,npni2)
          period=ll
c***
c         print*,'ll, period=',ll,period
c***

c---
          if(nrelax.ge.0) then
            d1=zero
            ipol1=2
            ipoln=nppol
            if(ir.eq.2) l0(nppol)=long(xn(1,ianc),yn(1,ianc),nn(ianc))
c
c  1.   on dispose d'abord les points proportionellement a ceux de la
c       ligne precedente
            do ipol=1,ipoln
              d1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ipol,ir-1),
     .           maily(ipol,ir-1),d1)
              if(ir.eq.2) l0(ipol)=d1
              l1(ipol)=(l0(ipol)/l0(nppol))*ll
              call coord1(xn(1,inouv),yn(1,inouv),nn(inouv),
     .          l1(ipol),mailx(ipol,ir),maily(ipol,ir),period)
            enddo
            if(nrelax.gt.0) then
c
              l1(nppol+1)=l1(nppol)+l1(2)-l1(1)
              mailx(nppol+1,ir-1)=mailx(2,ir-1)
              maily(nppol+1,ir-1)=maily(2,ir-1)
              mailx(nppol+1,ir)=mailx(2,ir)
              maily(nppol+1,ir)=maily(2,ir)
c***
c             print*,'call clort'
c***
c
c  2.   initialisation de la fonction qui doit s'annuler pour une
c       distribution orthogonale
              call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir),
     .          maily(1,ir),ort1,nppol+1,pasmin,zero,zero,l0,l1)
c***
c             print*,'point 3'
c***
c
c  3.   on procede a un premier deplacement des noeuds
              l2(1)=l1(1)
              l2(nppol+1)=l1(nppol+1)
              do ipol=ipol1,ipoln
                if(ort1(ipol).gt.zero) then
                  l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol+1)
                else
                  l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol-1)
                endif
                call coord1(xn(1,inouv),yn(1,inouv),nn(inouv),
     .            l2(ipol),mailx(ipol,ir),maily(ipol,ir),period)
              enddo
c***
c             print*,'point 4'
c***
c
c  4.   on relaxe les points de facon iterative pour realiser la
c       meilleure orthogonalite possible
              do i=1,nrelax
                call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir),
     .            maily(1,ir),ort2,nppol+1,pasmin,zero,zero,l0,l2)
                ortmax=zero
                do ipol=ipol1,ipoln
                  if(abs(ort2(ipol)).gt.rlcept) then
                    del=-ort2(ipol)*(l2(ipol)-l1(ipol))
     .                             /(ort2(ipol)-ort1(ipol))
                    if(del.gt.zero) then
                      del=min(del,relax*(l2(ipol+1)-l2(ipol)))
                    else
                      del=max(del,relax*(l2(ipol-1)-l2(ipol)))
                    endif
                    if(del.ne.zero) then
                      l1(ipol)=l2(ipol)
                      ort1(ipol)=ort2(ipol)
                      l2(ipol)=l1(ipol)+del
                    endif
                    call coord1(xn(1,inouv),yn(1,inouv),nn(inouv),
     .               l2(ipol),mailx(ipol,ir),maily(ipol,ir),period)
                  endif
                  ortmax=max(ortmax,abs(ort2(ipol)))
                enddo
                if(ortmax.le.rlcept) go to 19
c***
c               print*,'point 5'
c***
c
c  5.   on redefinit la reference et les positions des points pivots
                l1(1)=l1(nppol)-period
                l1(nppol+1)=l1(2)+period
                l2(1)=l1(1)
                l2(nppol+1)=l1(nppol+1)
                mailx(1,ir)=mailx(nppol,ir)
                maily(1,ir)=maily(nppol,ir)
                mailx(nppol+1,ir)=mailx(2,ir)
                maily(nppol+1,ir)=maily(2,ir)
              enddo
c***
              if(sellan(1:8).eq.'francais') then
                print*,
     .           'L''algorithme adaptatif d''optimisation de '//
     .           'l''orthogonalite de la maille n''a pas converge !'
                print*,'ortmax=',ortmax
                print*,'Pensez a relancer le programme avec: '
                print*,
     .           '1) Un plus grand nombre d''iterations, '//
     .            'actuellement nrelax = ',nrelax
                print*,
     .           '2) Un autre facteur de relaxation, '//
     .            'actuellement relax = ',relax
                print*,
     .           '3) Un espacement minimal plus petit, '//
     .            'actuellement pasmin = ',pasmin
                print*,
     .           '4) Un critere d''orthogonalite moins strict, '//
     .            'actuellement rlcept = ',rlcept
                print*,
     .           'Voulez-vous continuer (o/n) ?'
              elseif(sellan(1:7).eq.'english') then
                print*,
     .           'Mesh adaptation algorithm to optimize orthogonality'//
     .           ' did not converge !'
                print*,'ortmax=',ortmax
                print*,'Consider re-running with: '
                print*,
     .           '1) A larger number of iterations, '//
     .            'currently nrelax = ',nrelax
                print*,
     .           '2) Changing the relaxation factor, '//
     .            'currently relax = ',relax
                print*,
     .           '3) A smaller minimal spacing, '//
     .            'currently pasmin = ',pasmin
                print*,
     .           '4) A less stringent orthogonality criterion, '//
     .            'currently rlcept = ',rlcept
                print*,
     .           'Do you wish to continue (y/n) ?'
              endif
              if ( .not. AUTOCONTINUE ) then
                 read(5,*) reponse
                 if(reponse(1:1).eq.'n' .or. reponse(1:1).eq.'N') then
                    call pltend
                    stop
                 endif
              endif
              if(sellan(1:8).eq.'francais') then
                print*,
     .           'Verifiez attentivement la qualite de la maille '//
     .           'a la sortie du programme !'
              elseif(sellan(1:7).eq.'english') then
                print*,
     .           'Please check the quality of the mesh '//
     .           'carefully upon completion !'
              endif
c***
 19           continue
c***
c             print*,'apres 19 continue'
c***

            endif

          else
c---
            dernie=-0.025
            DO 20 ipol=1,nppol-1

*..Definition de x1 et y1 pour le bloc common comort.

              x1=mailx(ipol, ir-1)
              y1=maily(ipol, ir-1)

              CALL UNTANG(xn(1,ianc),yn(1,ianc),nn(ianc),x1,y1,ux1,uy1,
     .                   dernie,period)

              pasini=0.3*ll
              dist=aazero(nulort,dernie,pasini,epsmai,zero,dernie,ll,50)
              dernie=dist
              CALL COORD(xn(1,inouv),yn(1,inouv),nn(inouv),dist,
     .               mailx(ipol,ir),maily(ipol,ir))

   20       CONTINUE
c---
          endif
c---

*..Les premier et dernier points de la maille coincident.

          mailx(nppol,ir)=mailx(1,ir)
          maily(nppol,ir)=maily(1,ir)
        endif

   25 CONTINUE
c***
c     print*,'apres 25 continue'
c***
c
c  on definit la frontiere interieure
      if(pntrat.lt.distxo) then
        nbniv=nbniv+1
        nivtot(nbniv)=nn(inouv)
        do 30 i=1,nn(inouv)
        nivx(i,nbniv)=xn(i,inouv)
        nivy(i,nbniv)=yn(i,inouv)
   30   continue
      endif

*..On remet la periode a zero.

      period=zero

      RETURN
      END
