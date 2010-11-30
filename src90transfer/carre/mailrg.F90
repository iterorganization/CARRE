      SUBROUTINE MAILRG(mailx,maily,xn1,yn1,nn1,sens,pas,nppol,nprad, & 
     &            plaque,x2,y2,nx,ny,x,y,psi,nstruc,npstru, & 
     &            xstruc,ystruc,a00,a10,a01,a11, & 
     &            repart,gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2, & 
     &            xnlast,ynlast,nnlast,nuldec,sol,xpto,ypto,gdpsi, & 
     &            racpsi,a,gdr,r,ra,rho,somort,somortp,gdsomortp, & 
     &            somortpur,somortpurp,gdsomortpurp,sompropo, & 
     &            sompropop,gdsompropop,somvarr,somvarrp,gdsomvarrp, & 
     &            somtot,somtotp,gdsomtotp,segt,ireg,modif,xmini,xmaxi, & 
     &            ymini,ymaxi,separx,separy,ptsep,npx,nptot, & 
     &            nivx,nivy, & 
     &            nivtot,nbniv,np1,npr,xmail,ymail,nreg,re,distxo, & 
     &            xptxo,yptxo,pntrat,xfin,yfin,fctfin,nprsb,ipx)
!
!
!  version : 07.07.97 19:11
!
!======================================================================
      IMPLICIT NONE

!..  Cette sous-routine fait le maillage curviligne orthogonal dans
!  une region.


!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

!  arguments
      INTEGER nx,ny,nstruc,npstru(nstruc),nn1,sens,nppol,nprad, & 
     &        plaque,repart,nbcrb,npcrb2,nnlast,sol,ireg,modif, & 
     &        npx,nptot(4,npxmx),nbniv, & 
     &        nivtot(nbniv),ptsep(4,npx),npr(*),np1(*),nreg,re, & 
     &        nprsb(nregmx),ipx

      REAL*8 x(nxmax),y(nymax),psi(nxmax,nymax),xstruc(npstmx,nstruc), & 
     &     ystruc(npstmx,nstruc),mailx(npmamx,nrmamx), & 
     &     maily(npmamx,nrmamx),xn1(nn1),yn1(nn1),pas(nrmamx),x2,y2, & 
     &     a00(nxmax,nymax,3),a10(nxmax,nymax,3),a01(nxmax,nymax,3), & 
     &     a11(nxmax,nymax,3),gardd1,gardd2, & 
     &     xcrb2(npcrb2),ycrb2(npcrb2),xnlast(nnlast),ynlast(nnlast), & 
     &     xpto,ypto,a,gdr(nrmamx),r(nrmamx), & 
     &     ra(nrmamx), & 
     &     rho(nrmamx), gdpsi(nrmamx),racpsi(nrmamx), & 
     &     somort(nrmamx),somortp(nrmamx),gdsomortp, & 
     &     somortpur(nrmamx),somortpurp(nrmamx),gdsomortpurp, & 
     &     sompropo(nrmamx),sompropop(nrmamx),gdsompropop, & 
     &     somvarr(nrmamx),somvarrp(nrmamx),gdsomvarrp, & 
     &     somtot(nrmamx),somtotp(nrmamx),gdsomtotp,segt(nrmamx), & 
     &     xmini,xmaxi,ymini,ymaxi,separx(npnimx,4,npxmx), & 
     &     separy(npnimx,4,npxmx), & 
     &     nivx(npnimx,nbniv),nivy(npnimx,nbniv), & 
     &     xmail(npmamx,nrmamx,nrmamx),ymail(npmamx,nrmamx,nrmamx), & 
     &     xptxo,yptxo,pntrat,xfin,yfin,fctfin,distxo

      LOGICAL nuldec

!  variables en common

#include <COMORT.F>
#include <COMLAN.F>
#include <COMRLX.F>

!  variables locales
      INTEGER ipas,indstr,ianc,inouv,ind,ii,jj,ir,dir,ipol,i,nn(2), & 
     &  ig1,ig2,npcrb(2),ntt,sensspe,choix,interir,interirbis,deci, & 
     &  rr,rrr,rrrr,pra,sra,tra,prb,srb,trb,prc,src,trc,prd,srd,trd, & 
     &  pre,sre,tre,prf,srf,trf,prg,srg,trg,pri,sri,tri,prj,srj,trj, & 
     &  prk,srk,trk,prl,srl,trl,idep,iarr,prh,srh,trh,n,nnc,iseg
      REAL*8 ll,zero,pasini,epsmai,dist,dernie,valfct,ecart1,ecart2, & 
     &       epsiln,xn(npnimx,2),yn(npnimx,2),dd1,dd2,gard1,gard2 & 
     &       ,fctini,fctnew,fctxo,x22,y22,x23,y23,xtt(5),ytt(5),fctanc, & 
     &        longu,tpr,xc(npnimx),yc(npnimx)
      PARAMETER(zero=0.,epsmai=1.e-6,epsiln=1.E-08)
      REAL*8 xcrb(npnimx,2),ycrb(npnimx,2),coefa,coefb,coefc,coefd, & 
     &      coefe,coeff,coefg,coefh,coefi,coefj,coefk,coefl,coef,lsp,ls, & 
     &      vx(nrmamx),vy(nrmamx),lsd
      LOGICAL interval
      CHARACTER*1 reponse
      save valfct,fctini,fctxo,fctanc

!  procedures
      INTEGER indsgm,ifind,drctio
      REAL*8 aazero,long,nulort,ruban
      LOGICAL chgdir,in,cross
      INTRINSIC MOD,SQRT
      EXTERNAL aazero,long,COORD,indsgm,ifind,CRBNIV,nulort, & 
     &         UNTANG,SAUTE,chgdir,in,cross,ruban,clort,drctio
!======================================================================
!..calculs
!

!..Copie des valeurs de garde pour le bloc common comort.

      garde1=gardd1
      garde2=gardd2
      somort=0.0
      somortp=0.0
      gdsomortp=0.0
      somortpur=0.0
      somortpurp=0.0
      gdsomortpurp=0.0
      sompropo=0.0
      sompropop=0.0
      gdsompropop=0.0
      somvarr=0.0
      somvarrp=0.0
      gdsomvarrp=0.0
      somtot=0.0
      somtotp=0.0
      gdsomtotp=0.0
      segt=0.0

!..S'il y a deux courbes a ne pas traverser alors on definit la deuxieme

      IF (nbcrb .EQ. 2) THEN

         DO 5 i=1, npcrb2
            xcrb(i,2)=xcrb2(i)
            ycrb(i,2)=ycrb2(i)
    5    CONTINUE
         npcrb(2)=npcrb2

      ENDIF

!
!  definition de period pour le bloc comort
      period=0.
      DO 10 i=1,nn1
         xn(i,1)=xn1(i)
         yn(i,1)=yn1(i)
   10 CONTINUE

      nn(1)=nn1
      ll=long(xn(1,1),yn(1,1),nn(1))
!***
!     print*,'nbcrb, ll=',nbcrb,ll
!***

!..Calcul de la fonction pour le premier point de la courbe de reference

      ii=ifind(xn(1,1),x,nx,1)
      jj=ifind(yn(1,1),y,ny,1)

      fctini=a00(ii,jj,1) + a10(ii,jj,1)*xn(1,1) + & 
     &       a01(ii,jj,1)*yn(1,1) + a11(ii,jj,1)*xn(1,1)*yn(1,1)

      valfct=fctini
!***
!     print*,'valfct=',valfct
!***

!
!  calcul des indices de garde
      if(garde1+garde2.ge.ll .and. nrelax.lt.0) then
        write(6,100)garde1,garde2,ll
 100    format(/' Arret dans mailrg: garde1+garde2 > ll'/ & 
     &         ' garde1=',1pe10.3,' garde2=',1pe10.3,' ll=',1pe10.3)
        call pltend
        stop
      endif
      d1=0.
      ig1=2
      ig2=nppol-1
      do 12 ipol=2,nppol-1
        d1=ruban(xn1,yn1,nn1,mailx(ipol,1),maily(ipol,1),d1)
        if(d1.lt.garde1) ig1=ipol+1
        if(ll-d1.ge.garde2) ig2=ipol
12    continue

      do iseg=1,ireg-1
        nprsb(iseg)=npr(iseg)
      enddo

      do iseg=ireg+1,nreg
        nprsb(iseg)=1
      enddo

! la valeur du sol est précisé dans 'maille' selon l'indice de région et la configuration
! sol = 1 indicates an SOL region
       if (sol.EQ.1) then

         ii = ifind(xpto,x,nx,1)
       jj = ifind(ypto,y,ny,1)
         fctxo= a00(ii,jj,1) + a10(ii,jj,1)*xpto + a01(ii,jj,1)*ypto + & 
     &          a11(ii,jj,1)*xpto*ypto

! on place un faux segment qui sert d'appui à saute
         xtt(1)= xpto-0.1
         ytt(1)= ypto
         xtt(2)= x(nx)
       ytt(2)= ypto
       xtt(3)= xtt(1)
       ytt(3)= ytt(1)
         ntt=3

! la variable de sens est dedoublée pour ne pas pertuber la valeur du sens qui
! correspond à la plaque

       sensspe=1
         CALL SAUTE(xtt,ytt,ntt,xpto,ypto,fctxo,x22,y22,fctini,sensspe, & 
     &              2,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

!..Calcul des longueurs

         gdpsi(1)=1.0
       racpsi(1)=1.0
       gdr(1)=x22
         r(1)=x22-xpto
       a=r(1)
       ra(1)= r(1)-a
       rho(1)=r(1)/a

         fctanc=valfct

      else

         gdpsi(1)=1.0
       racpsi(1)=1.0

      endif

      DO 25 ir=2, nprad

!---
         print*, 'ir=', ir
!---

         ianc=MOD(ir,2) + 1
         inouv=MOD(ir-1,2) + 1

         x1=x2
         y1=y2

         ll1=ll

!..On verifie si on est a la derniere serie de points de maille dans la
!  direction poloidale, dans le cas de la region entre les deux separa-
!  trices du double nul deconnecte.

         IF ((ir .EQ. nprad) .AND. (nuldec)) THEN

!..Copie de la derniere courbe parametrisee passee en argument.

            DO 13 i=1, nnlast

               xn(i,inouv)=xnlast(i)
               yn(i,inouv)=ynlast(i)

   13       CONTINUE

            nn(inouv)=nnlast

            mailx(1,ir)=xnlast(1)
            maily(1,ir)=ynlast(1)

         ENDIF

         IF (repart .EQ. 1) THEN

            CALL SAUTE(xstruc(1,plaque),ystruc(1,plaque), & 
     &                npstru(plaque),x1,y1,valfct,x2,y2,pas(ir-1),sens, & 
     &                repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

         ELSE IF (repart .EQ. 2) THEN

            fctnew=valfct + pas(ir-1)

            CALL SAUTE(xstruc(1,plaque),ystruc(1,plaque), & 
     &                npstru(plaque),x1,y1,valfct,x2,y2,fctnew,sens, & 
     &                repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

         ENDIF
!***
!        print*,'x1, y1=',x1,y1
!        print*,'x2, y2=',x2,y2
!***

         xn(1,inouv)=x2
         yn(1,inouv)=y2
         mailx(1,ir)=x2
         maily(1,ir)=y2
         nn(inouv)=1

         ind=indsgm(xstruc(1,plaque),ystruc(1,plaque), & 
     &                  npstru(plaque),x2,y2)

!..Parametrisation de la ligne de niveau qui passe par ce point.

         ii=ifind(x2,x,nx,1)
         jj=ifind(y2,y,ny,1)

         valfct=a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + & 
     &          a11(ii,jj,1)*x2*y2

!..Definition de la pemiere courbe a ne pas traverser.

         DO 14 i=1, nn(ianc)
            xcrb(i,1)=xn(i,ianc)
            ycrb(i,1)=yn(i,ianc)
   14    CONTINUE

         npcrb(1)=nn(ianc)
         npcrb(2)=0

!..Recherche du deuxieme point.

         dir=0

         CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi, & 
     &            valfct,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx, & 
     &            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1, & 
     &            plaque,x2,y2)
!***
!        print*,'apres crbniv:'
!        print*,'x/y inouv=',xn(1,inouv),yn(1,inouv)
!        print*,'          ',xn(2,inouv),yn(2,inouv)
!        print*,'x/y ianc =',xn(1,ianc),yn(1,ianc)
!        print*,'          ',xn(2,ianc),yn(2,ianc)
!***

!..Il faut s'assurer que la ligne de niveau part dans la bonne
!  direction.
!  We must ensure that stepping along the contour line starts
!  in the right direction

         ecart1=SQRT((x2 - xstruc(ind,plaque))**2 + & 
     &               (y2 - ystruc(ind,plaque))**2)
         ecart2=SQRT((x2 - xstruc(ind+1,plaque))**2 + & 
     &               (y2 - ystruc(ind+1,plaque))**2)

         IF ((ABS(x2-x(nx)).LT.epsiln) .OR. (ABS(y2-y(ny)).LT.epsiln) & 
     &     .OR. (ABS(x2-x(1)).LT.epsiln) .OR. (ABS(y2-y(1)).LT.epsiln)) & 
     &                                                         THEN

            IF (((dir .EQ. 1) .AND. (ii .EQ. nx)) & 
     &       .OR. ((dir .EQ. 2) .AND. (jj .EQ. ny)) & 
     &       .OR. ((dir .EQ. 3) .AND. (ii .EQ. 1)) & 
     &       .OR. ((dir .EQ. 4) .AND. (jj .EQ. 1))) THEN

               nn(inouv)=1
               dir=MOD(dir+1,4) + 1
               ii=ii - MOD(dir-2,2)
               jj=jj - MOD(dir-3,2)

            ENDIF

         ELSE IF ((ecart1 .LT. epsiln) .OR. (ecart2 .LT. epsiln)) THEN

            IF (chgdir(xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc))) & 
     &                                                          THEN

               nn(inouv)=1
               dir=MOD(dir+1,4) + 1
               ii=ii - MOD(dir-2,2)
               jj=jj - MOD(dir-3,2)

            ENDIF

         ELSE

            IF (chgdir(xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc))) & 
     &                                                           THEN

               IF ((in(xn(2,inouv),yn(2,inouv),xstruc(1,plaque), & 
     &                  ystruc(1,plaque),npstru(plaque))) & 
     &            .OR. (cross(ind,xn(1,inouv),yn(1,inouv), & 
     &                    xstruc(1,plaque),ystruc(1,plaque), & 
     &                    npstru(plaque))))   THEN

                  nn(inouv)=1
                  dir=MOD(dir+1,4) + 1
                  ii=ii - MOD(dir-2,2)
                  jj=jj - MOD(dir-3,2)

               ELSE

                  PRINT *,'probleme de marche 1 dans mailrg'
                  call pltend
                  STOP

               ENDIF

            ELSE

               IF ((in(xn(2,inouv),yn(2,inouv),xstruc(1,plaque), & 
     &                  ystruc(1,plaque),npstru(plaque))) & 
     &            .OR. (cross(ind,xn(1,inouv),yn(1,inouv), & 
     &                    xstruc(1,plaque),ystruc(1,plaque), & 
     &                    npstru(plaque))))   THEN

                  PRINT *,'probleme de marche 2 dans mailrg'
                  call pltend
                  STOP

               ENDIF
            ENDIF
         ENDIF

!..Pour les points successifs, on poursuit jusqu'a ce qu'on frappe une
!  structure.

         CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi, & 
     &            valfct,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx, & 
     &            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1, & 
     &            plaque,x2,y2)

!.. Calcul de grand psi
         gdpsi(ir)= (valfct-fctxo)/(fctini-fctxo)
         racpsi(ir)= sqrt(gdpsi(ir))

         if (sol.EQ.1) then

         x1 = gdr(ir-1)
         y1=ypto
         sensspe=1 ! drctio(xtt,ytt,ntt,x1,y1,'d')
           CALL SAUTE(xtt,ytt,ntt,x1,y1,fctanc,x23,y23,fctnew,sensspe, & 
     &                repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

!..Calcul des longueurs

         gdr(ir)=x23
           r(ir)=x23-xpto
         ra(ir)= r(ir)-a
           rho(ir)=r(ir)/a
         endif

!..Definition de xn2 et yn2 pour le bloc common comort.

         DO 18 ipas=1,nn(inouv)
            xn2(ipas)=xn(ipas,inouv)
            yn2(ipas)=yn(ipas,inouv)
   18    CONTINUE

         npni2=nn(inouv)

!..On definit maintenant les points de maille de la nouvelle ligne
!  de niveau a partir de ceux de la precedente.

         dernie=0.0
         ll=long(xn2,yn2,npni2)
!---
         if(nrelax.ge.0) then
           d1=0.
           ipol1=2
           ipoln=nppol-1
!
!  1.   on dispose d'abord les points proportionellement a ceux de la
!       ligne precedente
           l1(1)=zero
           l1(nppol)=ll
           l0(1)=zero
           if(ir.eq.2) l0(nppol)=ll1
           do ipol=ipol1,ipoln
             d1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ipol,ir-1), & 
     &          maily(ipol,ir-1),d1)
             if(ir.eq.2) l0(ipol)=d1
!            l1(ipol)=(d1/ll1)*ll
             l1(ipol)=(l0(ipol)/l0(nppol))*ll
             CALL COORD(xn(1,inouv),yn(1,inouv),nn(inouv),l1(ipol), & 
     &               mailx(ipol,ir),maily(ipol,ir))
           enddo
!
           mailx(nppol,ir)=xn(nn(inouv),inouv)
           maily(nppol,ir)=yn(nn(inouv),inouv)
!
           if(nrelax.gt.0) then
!
!  2.   on initialise la fonction qui doit s'annuler pour une
!       distribution orthogonale
             call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir), & 
     &         maily(1,ir),ort1,nppol,pasmin,garde1,garde2,l0,l1, & 
     &         ortpur1,propo1,varr1,tot1)
!
!  3.   on procede a un premier deplacement des noeuds
               l2(1)=zero
               l2(nppol)=l1(nppol)
               segt(ir)=l2(nppol)
               do ipol=ipol1,ipoln
                 if(ort1(ipol).gt.zero) then
                   l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol+1)
                 else
                   l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol-1)
                 endif
                 call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol), & 
     &             mailx(ipol,ir),maily(ipol,ir))

              somort(ir)= somort(ir)+ (ort1(ipol)/nppol)
              somortpur(ir)= somortpur (ir)+ (ortpur1(ipol)/nppol)
                sompropo(ir)= sompropo(ir)+ (propo1(ipol)/nppol)
                somvarr(ir)= somvarr(ir)+ (varr1(ipol)/nppol)
                somtot(ir)= somtot(ir)+ (tot1(ipol)/nppol)

               enddo
!
!  4.   on relaxe les points de facon iterative pour realiser la
!       meilleure orthogonalite possible
             do i=1,nrelax
               call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir), & 
     &           maily(1,ir),ort2,nppol,pasmin,garde1,garde2,l0,l2, & 
     &           ortpur2,propo2,varr2,tot2)
               ortmax=zero
               do ipol=ipol1,ipoln
                 if(abs(ort2(ipol)).gt.rlcept) then
                   del=-ort2(ipol)*(l2(ipol)-l1(ipol)) & 
     &                            /(ort2(ipol)-ort1(ipol))
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
                   call coord(xn(1,inouv),yn(1,inouv),nn(inouv), & 
     &               l2(ipol),mailx(ipol,ir),maily(ipol,ir))
                 endif
                 ortmax=max(ortmax,abs(ort2(ipol)))
               enddo
               if(ortmax.le.rlcept) go to 19
             enddo
             if(sellan(1:8).eq.'francais') then
               print*, & 
     &          'L''algorithme adaptatif d''optimisation de '// & 
     &          'l''orthogonalite de la maille n''a pas converge !'
               print*,'ortmax=',ortmax
               print*,'Pensez a relancer le programme avec: '
               print*, & 
     &          '1) Un plus grand nombre d''iterations, '// & 
     &           'actuellement nrelax = ',nrelax
               print*, & 
     &          '2) Un autre facteur de relaxation, '// & 
     &           'actuellement relax = ',relax
               print*, & 
     &          '3) Un espacement minimal plus petit, '// & 
     &           'actuellement pasmin = ',pasmin
               print*, & 
     &          '4) Un critere d''orthogonalite moins strict, '// & 
     &           'actuellement rlcept = ',rlcept
               print*, & 
     &          '5) Des longueurs de garde plus importantes '// & 
     &           '(ou nulles), actuellement tgarde = ',garde1,garde2
               print*, & 
     &          'Voulez-vous continuer (o/n) ?'
             elseif(sellan(1:7).eq.'english') then
               print*, & 
     &          'Mesh adaptation algorithm to optimize orthogonality'// & 
     &          ' did not converge !'
               print*,'ortmax=',ortmax
               print*,'Consider re-running with: '
               print*, & 
     &          '1) A larger number of iterations, '// & 
     &           'currently nrelax = ',nrelax
               print*, & 
     &          '2) Changing the relaxation factor, '// & 
     &           'currently relax = ',relax
               print*, & 
     &          '3) A smaller minimal spacing, '// & 
     &           'currently pasmin = ',pasmin
               print*, & 
     &          '4) A less stringent orthogonality criterion, '// & 
     &           'currently rlcept = ',rlcept
               print*, & 
     &          '5) Longer (or zero) guard lengths, '// & 
     &           'currently tgarde = ',garde1,garde2
               print*, & 
     &          'Do you wish to continue (y/n) ?'
             endif
             if ( .not. AUTOCONTINUE ) then
                read(5,*) reponse
                if(reponse(1:1).eq.'n' .or. reponse(1:1).eq.'N') then
                   call pltend
                   stop
                endif
             endif
             if(sellan(1:8).eq.'francais') then
               print*, & 
     &          'Verifiez attentivement la qualite de la maille '// & 
     &          'a la sortie du programme !'
             elseif(sellan(1:7).eq.'english') then
               print*, & 
     &          'Please check the quality of the mesh '// & 
     &          'carefully upon completion !'
             endif
 19          continue
             do ipol=ipol1,ipoln
              somortp(ir)= somortp(ir)+ (ort2(ipol)/nppol)
                somortpurp(ir)= somortpurp(ir)+ (ortpur2(ipol)/nppol)
                sompropop(ir)= sompropop(ir)+ (propo2(ipol)/nppol)
                somvarrp(ir)= somvarrp(ir)+ (varr2(ipol)/nppol)
                somtotp(ir)= somtotp(ir)+ (tot2(ipol)/nppol)
              enddo

           endif

         else
!---
           d1=zero
           DO 20 ipol=2, ig2
!..Definition de x1, y1, ux1,uy1 et d1 pour le bloc common comort.
             x1=mailx(ipol, ir-1)
             y1=maily(ipol, ir-1)
             CALL UNTANG(xn(1,ianc),yn(1,ianc),nn(ianc),x1,y1,ux1,uy1, & 
     &                   d1,period)
             d1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),x1,y1,d1)
             pasini=0.3*ll
             dist=aazero(nulort,dernie,pasini,epsmai,zero,dernie,ll,50)
             dernie=dist
             CALL COORD(xn(1,inouv),yn(1,inouv),nn(inouv),dist, & 
     &               mailx(ipol,ir),maily(ipol,ir))
 20        CONTINUE

!
!  distribution des points dans la region de garde
          gard2=ruban(xn(1,inouv),yn(1,inouv),nn(inouv),mailx(ig1,ir), & 
     &      maily(ig1,ir),zero)
          gard1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ig1,ir-1), & 
     &      maily(ig1,ir-1),zero)
          do 22 ipol=2,ig1-1
          dd1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ipol,ir-1), & 
     &      maily(ipol,ir-1),zero)
          dd2=dd1/gard1*gard2
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),dd2, & 
     &      mailx(ipol,ir),maily(ipol,ir))
22        continue
          gard2=ll-ruban(xn(1,inouv),yn(1,inouv),nn(inouv), & 
     &      mailx(ig2,ir),maily(ig2,ir),zero)
          gard1=ll1-ruban(xn(1,ianc),yn(1,ianc),nn(ianc), & 
     &      mailx(ig2,ir-1),maily(ig2,ir-1),zero)
          do 23 ipol=ig2+1,nppol-1
          dd1=ll1-ruban(xn(1,ianc),yn(1,ianc),nn(ianc), & 
     &      mailx(ipol,ir-1),maily(ipol,ir-1),zero)
          dd2=ll-dd1/gard1*gard2
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),dd2, & 
     &      mailx(ipol,ir),maily(ipol,ir))
23        continue

!..Le dernier point de la maille coincide avec le dernier point de la
!  courbe.
         mailx(nppol,ir)=xn(nn(inouv),inouv)
         maily(nppol,ir)=yn(nn(inouv),inouv)
!---
         endif
!---
   25 CONTINUE

       do ir=2,nprad

       gdsomortp= gdsomortp + (somortp(ir)/(nprad-1))
       gdsomortpurp= gdsomortpurp+ (somortpurp(ir)/(nprad-1))
       gdsompropop= gdsompropop+ (sompropop(ir)/(nprad-1))
       gdsomvarrp= gdsomvarrp+ (somvarrp(ir)/(nprad-1))
       gdsomtotp= gdsomtotp+ (somtotp(ir)/(nprad-1))

      enddo

  888 CONTINUE
      RETURN
      END
