      SUBROUTINE MAILRG(mailx,maily,xn1,yn1,nn1,sens,pas,nppol,nprad,
     .            plaque,x2,y2,nx,ny,x,y,psi,nstruc,npstru,
     .            xstruc,ystruc,a00,a10,a01,a11,
     .            repart,gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,
     .            xnlast,ynlast,nnlast,nuldec,sol,xpto,ypto,gdpsi,
     .            racpsi,a,gdr,r,ra,rho,somort,somortp,gdsomortp,
     .            somortpur,somortpurp,gdsomortpurp,sompropo,
     .            sompropop,gdsompropop,somvarr,somvarrp,gdsomvarrp,
     .            somtot,somtotp,gdsomtotp,segt,ireg,modif,xmini,xmaxi,
     .            ymini,ymaxi,separx,separy,ptsep,npx,nptot,
     .            nivx,nivy,
     .            nivtot,nbniv,np1,npr,xmail,ymail,nreg,nprsb)
c
c
c  version : 07.07.97 19:11
c
c======================================================================
      IMPLICIT NONE

*..  Cette sous-routine fait le maillage curviligne orthogonal dans
*  une region.


cank-970707: dimensions from the file
*  dimensions
#include <CARRETYPES.F>
#include <CARREDIM.F>

*  arguments
      INTEGER nx,ny,nstruc,npstru(nstruc),nn1,sens,nppol,nprad,
     .        plaque,repart,nbcrb,npcrb2,nnlast,sol,ireg,modif,
     .        npx,nptot(4,npxmx),nbniv,
     .        nivtot(nbniv),ptsep(4,npx),npr(*),np1(*),nreg,
     .        nprsb(nregmx)

      REAL(kind=R8)
     .     x(nxmax),y(nymax),psi(nxmax,nymax),xstruc(npstmx,nstruc),
     .     ystruc(npstmx,nstruc),mailx(npmamx,nrmamx),
     .     maily(npmamx,nrmamx),xn1(nn1),yn1(nn1),pas(nrmamx),x2,y2,
     .     a00(nxmax,nymax,3),a10(nxmax,nymax,3),a01(nxmax,nymax,3),
     .     a11(nxmax,nymax,3),gardd1,gardd2,
     .     xcrb2(npcrb2),ycrb2(npcrb2),xnlast(nnlast),ynlast(nnlast),
     .     xpto,ypto,a,gdr(nrmamx),r(nrmamx),
     .     ra(nrmamx),
     .     rho(nrmamx), gdpsi(nrmamx),racpsi(nrmamx),
     .     somort(nrmamx),somortp(nrmamx),gdsomortp,
     .     somortpur(nrmamx),somortpurp(nrmamx),gdsomortpurp,
     .     sompropo(nrmamx),sompropop(nrmamx),gdsompropop,
     .     somvarr(nrmamx),somvarrp(nrmamx),gdsomvarrp,
     .     somtot(nrmamx),somtotp(nrmamx),gdsomtotp,segt(nrmamx),
     .     xmini,xmaxi,ymini,ymaxi,separx(npnimx,4,npxmx),
     .     separy(npnimx,4,npxmx),
     .     nivx(npnimx,nbniv),nivy(npnimx,nbniv),
     .     xmail(npmamx,nrmamx,nrmamx),ymail(npmamx,nrmamx,nrmamx)

      LOGICAL nuldec

*  variables en common

#include <COMORT.F>
#include <COMLAN.F>
#include <COMRLX.F>

*  variables locales
      INTEGER ipas,indstr,ianc,inouv,ind,ii,jj,ir,dir,ipol,i,nn(2),
     .  ig1,ig2,npcrb(2),ntt,sensspe,iseg
      REAL(kind=R8)
     .       ll,zero,pasini,epsmai,dist,dernie,valfct,ecart1,ecart2,
     .       epsiln,xn(npnimx,2),yn(npnimx,2),dd1,dd2,gard1,gard2,
     .       fctini,fctnew,fctxo,x22,y22,x23,y23,xtt(5),ytt(5),fctanc
      PARAMETER(zero=0.0_R8,epsmai=1.0e-6_R8,epsiln=1.0E-08_R8)
      REAL(kind=R8)
     .      xcrb(npnimx,2),ycrb(npnimx,2)
      CHARACTER*1 reponse
      save valfct,fctini,fctxo,fctanc

#ifdef MODIF
      INTEGER n, rr, choix, idep, iarr, pra, pri, sra, tra
      REAL(kind=R8) coef, coefa, ls, lsd, lsp, tpr
      REAL(kind=R8) vx(nrmamx), vy(nrmamx)
#endif

*  procedures
      INTEGER indsgm,ifind,drctio
      REAL(kind=R8) aazero,long,nulort,ruban
      LOGICAL chgdir,in,cross
      INTRINSIC MOD,SQRT
      EXTERNAL aazero,long,COORD,indsgm,ifind,CRBNIV,nulort,
     .         UNTANG,SAUTE,chgdir,in,cross,ruban,clort,drctio
c======================================================================
*..calculs
*

*..Copie des valeurs de garde pour le bloc common comort.

      garde1=gardd1
      garde2=gardd2
      somort=0.0_R8
      somortp=0.0_R8
      gdsomortp=0.0_R8
      somortpur=0.0_R8
      somortpurp=0.0_R8
      gdsomortpurp=0.0_R8
      sompropo=0.0_R8
      sompropop=0.0_R8
      gdsompropop=0.0_R8
      somvarr=0.0_R8
      somvarrp=0.0_R8
      gdsomvarrp=0.0_R8
      somtot=0.0_R8
      somtotp=0.0_R8
      gdsomtotp=0.0_R8
      segt=0.0_R8

*..S'il y a deux courbes a ne pas traverser alors on definit la deuxieme

      IF (nbcrb .EQ. 2) THEN

         DO 5 i=1, npcrb2
            xcrb(i,2)=xcrb2(i)
            ycrb(i,2)=ycrb2(i)
    5    CONTINUE
         npcrb(2)=npcrb2

      ENDIF

c
c  definition de period pour le bloc comort
      period=0.
      DO 10 i=1,nn1
         xn(i,1)=xn1(i)
         yn(i,1)=yn1(i)
   10 CONTINUE

      nn(1)=nn1
      ll=long(xn(1,1),yn(1,1),nn(1))
c***
c     print*,'nbcrb, ll=',nbcrb,ll
c***

*..Calcul de la fonction pour le premier point de la courbe de reference

      ii=ifind(xn(1,1),x,nx,1)
      jj=ifind(yn(1,1),y,ny,1)

      fctini=a00(ii,jj,1) + a10(ii,jj,1)*xn(1,1) +
     .       a01(ii,jj,1)*yn(1,1) + a11(ii,jj,1)*xn(1,1)*yn(1,1)

      valfct=fctini
c***
c     print*,'valfct=',valfct
c***

c
c  calcul des indices de garde
      if(garde1+garde2.ge.ll .and. nrelax.lt.0) then
        write(6,100)garde1,garde2,ll
 100    format(/' Arret dans mailrg: garde1+garde2 > ll'/
     .          ' garde1=',1pe10.3,' garde2=',1pe10.3,' ll=',1pe10.3)
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

* the value of sol is given in 'maille' according to the region index and the configuration
* sol = 1 indicates an SOL region
       if (sol.EQ.1) then

         ii = ifind(xpto,x,nx,1)
         jj = ifind(ypto,y,ny,1)
         fctxo= a00(ii,jj,1) + a10(ii,jj,1)*xpto + a01(ii,jj,1)*ypto +
     .          a11(ii,jj,1)*xpto*ypto

* we locate a fictitious segment which serves as a baseline for saute
         xtt(1)= xpto-0.1
         ytt(1)= ypto
         xtt(2)= x(nx)
         ytt(2)= ypto
         xtt(3)= xtt(1)
         ytt(3)= ytt(1)
         ntt=3

* we duplicate the 'sens' (direction) variable so as not to overwrite the
* direction value used for the plate

         sensspe=1
         CALL SAUTE(xtt,ytt,ntt,xpto,ypto,fctxo,x22,y22,fctini,sensspe,
     .              2,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

*..Computation of the lengths

         gdpsi(1)=1.0_R8
         racpsi(1)=1.0_R8
         gdr(1)=x22
         r(1)=x22-xpto
         a=r(1)
         ra(1)= r(1)-a
         rho(1)=r(1)/a

         fctanc=valfct

      else

         gdpsi(1)=1.0_R8
         racpsi(1)=1.0_R8

      endif

      DO 25 ir=2, nprad
c---
         print*, 'ir=', ir
c---

         ianc=MOD(ir,2) + 1
         inouv=MOD(ir-1,2) + 1

         x1=x2
         y1=y2

         ll1=ll

*..On verifie si on est a la derniere serie de points de maille dans la
*  direction poloidale, dans le cas de la region entre les deux separa-
*  trices du double nul deconnecte.

         IF ((ir .EQ. nprad) .AND. (nuldec)) THEN

*..Copie de la derniere courbe parametrisee passee en argument.

            DO 13 i=1, nnlast

               xn(i,inouv)=xnlast(i)
               yn(i,inouv)=ynlast(i)

   13       CONTINUE

            nn(inouv)=nnlast

            mailx(1,ir)=xnlast(1)
            maily(1,ir)=ynlast(1)

            GO TO 17

         ENDIF

         IF (repart .EQ. 1) THEN

            CALL SAUTE(xstruc(1,plaque),ystruc(1,plaque),
     .                 npstru(plaque),x1,y1,valfct,x2,y2,pas(ir-1),sens,
     .                 repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

         ELSE IF (repart .EQ. 2) THEN

            fctnew=valfct + pas(ir-1)

            CALL SAUTE(xstruc(1,plaque),ystruc(1,plaque),
     .                 npstru(plaque),x1,y1,valfct,x2,y2,fctnew,sens,
     .                 repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

         ENDIF
c***
#ifdef DBG
        print*,'x1, y1=',x1,y1
        print*,'x2, y2=',x2,y2
#endif
c***

         xn(1,inouv)=x2
         yn(1,inouv)=y2
         mailx(1,ir)=x2
         maily(1,ir)=y2
         nn(inouv)=1

         ind=indsgm(xstruc(1,plaque),ystruc(1,plaque),
     .              npstru(plaque),x2,y2)

*..Parametrisation de la ligne de niveau qui passe par ce point.

         ii=ifind(x2,x,nx,1)
         jj=ifind(y2,y,ny,1)

         valfct=a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 +
     .          a11(ii,jj,1)*x2*y2

*..Definition de la pemiere courbe a ne pas traverser.

         DO 14 i=1, nn(ianc)
            xcrb(i,1)=xn(i,ianc)
            ycrb(i,1)=yn(i,ianc)
   14    CONTINUE

         npcrb(1)=nn(ianc)
         npcrb(2)=0

*..Recherche du deuxieme point.

         dir=0

         CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi,
     .            valfct,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx,
     .            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1,
     .            plaque,x2,y2)
c***
c        print*,'apres crbniv:'
c        print*,'x/y inouv=',xn(1,inouv),yn(1,inouv)
c        print*,'          ',xn(2,inouv),yn(2,inouv)
c        print*,'x/y ianc =',xn(1,ianc),yn(1,ianc)
c        print*,'          ',xn(2,ianc),yn(2,ianc)
c***

*..Il faut s'assurer que la ligne de niveau part dans la bonne
*  direction.
*  We must ensure that stepping along the contour line starts
*  in the right direction

         ecart1=SQRT((x2 - xstruc(ind,plaque))**2 +
     .               (y2 - ystruc(ind,plaque))**2)
         ecart2=SQRT((x2 - xstruc(ind+1,plaque))**2 +
     .               (y2 - ystruc(ind+1,plaque))**2)

         IF ((ABS(x2-x(nx)).LT.epsiln) .OR. (ABS(y2-y(ny)).LT.epsiln)
     .     .OR. (ABS(x2-x(1)).LT.epsiln) .OR. (ABS(y2-y(1)).LT.epsiln))
     .                                                         THEN

            IF (((dir .EQ. 1) .AND. (ii .EQ. nx))
     .       .OR. ((dir .EQ. 2) .AND. (jj .EQ. ny))
     .       .OR. ((dir .EQ. 3) .AND. (ii .EQ. 1))
     .       .OR. ((dir .EQ. 4) .AND. (jj .EQ. 1))) THEN

               nn(inouv)=1
               dir=MOD(dir+1,4) + 1
               ii=ii - MOD(dir-2,2)
               jj=jj - MOD(dir-3,2)

            ENDIF

         ELSE IF ((ecart1 .LT. epsiln) .OR. (ecart2 .LT. epsiln)) THEN

            IF (chgdir(xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc)))
     .                                                          THEN

               nn(inouv)=1
               dir=MOD(dir+1,4) + 1
               ii=ii - MOD(dir-2,2)
               jj=jj - MOD(dir-3,2)

            ENDIF

         ELSE

            IF (chgdir(xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc)))
     .                                                           THEN

               IF ((in(xn(2,inouv),yn(2,inouv),xstruc(1,plaque),
     .                  ystruc(1,plaque),npstru(plaque)))
     .            .OR. (cross(ind,xn(1,inouv),yn(1,inouv),
     .                    xstruc(1,plaque),ystruc(1,plaque),
     .                    npstru(plaque)))) THEN

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

               IF ((in(xn(2,inouv),yn(2,inouv),xstruc(1,plaque),
     .                  ystruc(1,plaque),npstru(plaque)))
     .            .OR. (cross(ind,xn(1,inouv),yn(1,inouv),
     .                    xstruc(1,plaque),ystruc(1,plaque),
     .                    npstru(plaque)))) THEN

                  PRINT *,'probleme de marche 2 dans mailrg'
                  call pltend
                  STOP

               ENDIF
            ENDIF
         ENDIF

*..Pour les points successifs, on poursuit jusqu'a ce qu'on frappe une
*  structure.

         CALL CRBNIV(ii,jj,nn(inouv),dir,nxmax,nymax,nx,ny,x,y,psi,
     .            valfct,xn(1,inouv),yn(1,inouv),npnimx,nstruc,npstmx,
     .            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1,
     .            plaque,x2,y2)

*.. Calcul de grand psi
         gdpsi(ir)= (valfct-fctxo)/(fctini-fctxo)
         racpsi(ir)= sqrt(gdpsi(ir))

         if (sol.EQ.1) then

           x1 = gdr(ir-1)
           y1=ypto
           sensspe=1 ! drctio(xtt,ytt,ntt,x1,y1,'d')
           CALL SAUTE(xtt,ytt,ntt,x1,y1,fctanc,x23,y23,fctnew,sensspe,
     .                repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

*..Calcul des longueurs

           gdr(ir)=x23
           r(ir)=x23-xpto
           ra(ir)= r(ir)-a
           rho(ir)=r(ir)/a
         endif

   17    CONTINUE

*..Definition de xn2 et yn2 pour le bloc common comort.

         DO 18 ipas=1,nn(inouv)
            xn2(ipas)=xn(ipas,inouv)
            yn2(ipas)=yn(ipas,inouv)
   18    CONTINUE

         npni2=nn(inouv)

*..On definit maintenant les points de maille de la nouvelle ligne
*  de niveau a partir de ceux de la precedente.

         dernie=0.0
         ll=long(xn2,yn2,npni2)
c---
         if(nrelax.ge.0) then
           d1=0.
           ipol1=2
           ipoln=nppol-1
c
c  1.   on dispose d'abord les points proportionellement a ceux de la
c       ligne precedente
           l1(1)=zero
           l1(nppol)=ll
           l0(1)=zero
           if(ir.eq.2) l0(nppol)=ll1
           do ipol=ipol1,ipoln
             d1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ipol,ir-1),
     .          maily(ipol,ir-1),d1)
             if(ir.eq.2) l0(ipol)=d1
c            l1(ipol)=(d1/ll1)*ll
             l1(ipol)=(l0(ipol)/l0(nppol))*ll
             CALL COORD(xn(1,inouv),yn(1,inouv),nn(inouv),l1(ipol),
     .               mailx(ipol,ir),maily(ipol,ir))
           enddo
c
           mailx(nppol,ir)=xn(nn(inouv),inouv)
           maily(nppol,ir)=yn(nn(inouv),inouv)
c
           if(nrelax.gt.0) then
c
c  2.   on initialise la fonction qui doit s'annuler pour une
c       distribution orthogonale
             call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir),
     .         maily(1,ir),ort1,nppol,pasmin,garde1,garde2,l0,l1,
     .         ortpur1,propo1,varr1,tot1)
c
c  3.   on procede a un premier deplacement des noeuds
               l2(1)=zero
               l2(nppol)=l1(nppol)
               segt(ir)=l2(nppol)
               do ipol=ipol1,ipoln
                 if(ort1(ipol).gt.zero) then
                   l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol+1)
                 else
                   l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol-1)
                 endif
                 call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

                somort(ir)= somort(ir)+ (ort1(ipol)/nppol)
                somortpur(ir)= somortpur (ir)+ (ortpur1(ipol)/nppol)
                sompropo(ir)= sompropo(ir)+ (propo1(ipol)/nppol)
                somvarr(ir)= somvarr(ir)+ (varr1(ipol)/nppol)
                somtot(ir)= somtot(ir)+ (tot1(ipol)/nppol)

               enddo
c
c  4.   on relaxe les points de facon iterative pour realiser la
c       meilleure orthogonalite possible
             do i=1,nrelax
               call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir),
     .           maily(1,ir),ort2,nppol,pasmin,garde1,garde2,l0,l2,
     .           ortpur2,propo2,varr2,tot2)
               ortmax=zero
               do ipol=ipol1,ipoln
                 if(abs(ort2(ipol)).gt.rlcept) then
                   if (abs(ort2(ipol)-ort1(ipol)).gt.rlcept**2) then
                     del=-ort2(ipol)*(l2(ipol)-l1(ipol))
     .                            /(ort2(ipol)-ort1(ipol))
                   else
                     del=zero
                   end if
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
                   call coord(xn(1,inouv),yn(1,inouv),nn(inouv),
     .               l2(ipol),mailx(ipol,ir),maily(ipol,ir))
                 endif
                 ortmax=max(ortmax,abs(ort2(ipol)))
               enddo

               if(ortmax.le.rlcept) go to 19

             enddo

             if(sellan(1:8).eq.'francais') then
               print*,
     .          'L''algorithme adaptatif d''optimisation de '//
     .          'l''orthogonalite de la maille n''a pas converge !'
               print*,'ortmax=',ortmax
               print*,'Pensez a relancer le programme avec: '
               write (6,'(a,i6)') 
     .          '1) Un plus grand nombre d''iterations, '//
     .           'actuellement nrelax = ',nrelax
               write (6,'(a,1p,1e12.5)')
     .          '2) Un autre facteur de relaxation, '//
     .           'actuellement relax = ',relax
               write (6,'(a,1p,1e12.5)')
     .          '3) Un espacement minimal plus petit, '//
     .           'actuellement pasmin = ',pasmin
               write (6,'(a,1p,1e12.5)')
     .          '4) Un critere d''orthogonalite moins strict, '//
     .           'actuellement rlcept = ',rlcept
               write (6,'(a,1p,2e12.5)')
     .          '5) Des longueurs de garde plus importantes '//
     .           '(ou nulles), actuellement tgarde = ',garde1,garde2
               print*,
     .          'Voulez-vous continuer (o/n) ?'
             elseif(sellan(1:7).eq.'english') then
               print*,
     .          'Mesh adaptation algorithm to optimize orthogonality'//
     .          ' did not converge !'
               print*,'ortmax=',ortmax
               print*,'Consider re-running with: '
               write (6,'(a,i6)')
     .          '1) A larger number of iterations, '//
     .           'currently nrelax = ',nrelax
               write (6,'(a,1p,1e12.5)')
     .          '2) Changing the relaxation factor, '//
     .           'currently relax = ',relax
               write (6,'(a,1p,1e12.5)')
     .          '3) A smaller minimal spacing, '//
     .           'currently pasmin = ',pasmin
               write (6,'(a,1p,1e12.5)')
     .          '4) A less stringent orthogonality criterion, '//
     .           'currently rlcept = ',rlcept
               write (6,'(a,1p,2e12.5)')
     .          '5) Longer (or zero) guard lengths, '//
     .           'currently tgarde = ',garde1,garde2
               print*,
     .          'Do you wish to continue (y/n) ?'
             endif
             read(5,*) reponse
             if(reponse(1:1).eq.'n' .or. reponse(1:1).eq.'N') then
               call pltend
               stop
             endif
             if(sellan(1:8).eq.'francais') then
               print*,
     .          'Verifiez attentivement la qualite de la maille '//
     .          'a la sortie du programme !'
             elseif(sellan(1:7).eq.'english') then
               print*,
     .          'Please check the quality of the mesh '//
     .          'carefully upon completion !'
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
c---
           d1=zero
           DO 20 ipol=2, ig2
*..Definition de x1, y1, ux1,uy1 et d1 pour le bloc common comort.
             x1=mailx(ipol, ir-1)
             y1=maily(ipol, ir-1)
             CALL UNTANG(xn(1,ianc),yn(1,ianc),nn(ianc),x1,y1,ux1,uy1,
     .                   d1,period)
             d1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),x1,y1,d1)
             pasini=0.3*ll
             dist=aazero(nulort,dernie,pasini,epsmai,zero,dernie,ll,50)
             dernie=dist
             CALL COORD(xn(1,inouv),yn(1,inouv),nn(inouv),dist,
     .               mailx(ipol,ir),maily(ipol,ir))
 20        CONTINUE

c  distribution des points dans la region de garde
           gard2=ruban(xn(1,inouv),yn(1,inouv),nn(inouv),mailx(ig1,ir),
     .      maily(ig1,ir),zero)
           gard1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ig1,ir-1),
     .      maily(ig1,ir-1),zero)
           do 22 ipol=2,ig1-1
             dd1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ipol,ir-1),
     .        maily(ipol,ir-1),zero)
             dd2=dd1/gard1*gard2
             call coord(xn(1,inouv),yn(1,inouv),nn(inouv),dd2,
     .        mailx(ipol,ir),maily(ipol,ir))
22         continue
           gard2=ll-ruban(xn(1,inouv),yn(1,inouv),nn(inouv),
     .      mailx(ig2,ir),maily(ig2,ir),zero)
           gard1=ll1-ruban(xn(1,ianc),yn(1,ianc),nn(ianc),
     .      mailx(ig2,ir-1),maily(ig2,ir-1),zero)
           do 23 ipol=ig2+1,nppol-1
             dd1=ll1-ruban(xn(1,ianc),yn(1,ianc),nn(ianc),
     .        mailx(ipol,ir-1),maily(ipol,ir-1),zero)
             dd2=ll-dd1/gard1*gard2
             call coord(xn(1,inouv),yn(1,inouv),nn(inouv),dd2,
     .        mailx(ipol,ir),maily(ipol,ir))
23         continue

*..Le dernier point de la maille coincide avec le dernier point de la
*  courbe.
           mailx(nppol,ir)=xn(nn(inouv),inouv)
           maily(nppol,ir)=yn(nn(inouv),inouv)
c---
         endif

         nprsb(ireg)=ir
#ifdef MODIF
1000     if (modif.eq.1) then
           if (ir.eq.2) then
             write(6,*)
     .        'Vous avez choisi d''effectuer des modifications'
             write(6,*) 'sur la position des noeuds. Les modifications '
             write(6,*) 's''effectueront par ligne de niveau.'
           endif
           write(6,*) 'Voulez-vous que les modifications se fassent'
           write(6,*) 'uniformément le long de la ligne de niveau ou '
           write(6,*) 'qu''elles se fassent par section?'
  123      write(6,*) 'Répondez 1 pour uniforme, 2 pour les sections, '
           write(6,*) 'et 3 pour des sections avec des pas predisposés.'
           read(5,*,end=270,err=123) choix
  270      continue

           if (choix.eq.1) then

  345        write(6,*) 'Choississez un coefficient'
             write(6,*) ' (qui multipliera la valeur actuelle des ipol)'
             read(5,*,end=271,err=345) coef
  271        continue
             do ipol=2,ipoln
               l2(ipol)=l2(ipol)* coef
             enddo
  709        write(6,*) 'Définissez les bords.'
  710        write(6,*) 'Limite inférieure en R'
             read(5,*,end=711, err=710) xmini
  711        write(6,*) 'Limite supérieure en R'
             read(5,*,end=712,err=711) xmaxi
  712        write(6,*) 'Limite inférieure en Z'
             read (5,*,end=713,err=712) ymini
  713        write(6,*) 'Limite supérieure en Z'
             read(5,*,end=714,err=713) ymaxi

  714        call endpag
             call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,nprsb,xmail,ymail,nreg,.false.)
             write(6,*) 'Voulez-vous redefinir les bords?'
  715        write(6,*) 'Répondez 1 si c''est oui et 0 si c''est non'
             read(5,*,end=716,err=715) rr
  716        if (rr.eq.1) go to 709

           elseif (choix.eq.2) then

             do iseg = 6,1,-1

               lsp=(l2(nppol)/(iseg+1))
               if (iseg.eq.6) then
                 lsd = 0
                 idep = 1
               else
                 lsd=(l2(nppol)/(iseg+2))
                 idep = iarr
               endif
               iarr=idep

               vx(1)=xmail(idep,ir,ireg)
               vy(1)=ymail(idep,ir,ireg)
               ls = 0
               do while (ls.lt.lsp-lsd)
                 iarr=iarr+1
                 n=iarr-idep+1
                 vx(iarr-idep+1)=xmail(iarr,ir,ireg)
                 vy(iarr-idep+1)=ymail(iarr,ir,ireg)
                 ls=long(vx,vy,n)
               enddo

               xmini= minval(xmail(idep:iarr,1:ir,ireg))
               xmaxi= maxval(xmail(idep:iarr,1:ir,ireg))
               ymini= minval(ymail(idep:iarr,1:ir,ireg))
               ymaxi= maxval(ymail(idep:iarr,1:ir,ireg))

               tpr=n

               call endpag
               call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,nprsb,xmail,ymail,nreg,.false.)

 1211          write(6,*) 'Voulez-vous redefinir les bords?'
               write(6,*) 'Répondez 1 si c''est oui et 0 si c''est non'
               read(5,*,end=1212,err=1211) pra
 1212          if (pra.eq.0) go to 350

 1213          write(6,*) 'Définissez les bords.'
 1214          write(6,*) 'Limite inférieure en R'
               read(5,*,end=1215, err=1214) xmini
 1215          write(6,*) 'Limite supérieure en R'
               read(5,*,end=1216,err=1215) xmaxi
 1216          write(6,*) 'Limite inférieure en Z'
               read (5,*,end=1217,err=1216) ymini
 1217          write(6,*) 'Limite supérieure en Z'
               read(5,*,end=1218,err=1217) ymaxi

 1218          call endpag
               call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,nprsb,xmail,ymail,nreg,.false.)

 1219          write(6,*) 'Recommencer?'
               write(6,*) 'Répondez 1 si c''est oui et 0 si c''est non'
               read(5,*,end=1220,err=1219) sra
 1220          if (sra.eq.1) go to 1213

  350          write(6,*) 'Choississez un coefficient qui sera utilisé '
               write(6,'(a,i1)') 'dans la partie inférieure à 1/',iseg+1
               write(6,*) 'de nppol et qui multipliera la valeur '
               write(6,*) 'actuelle des ipol'
               read(5,*,end=310,err=350) coefa

  310          continue
               do ipol=2,ipoln
                 if(l2(ipol).ge.lsd.and.l2(ipol).le.lsp)then
                   l2(ipol)= l2(ipol)*coefa
                   call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .              mailx(ipol,ir),maily(ipol,ir))
                 endif
               enddo

               call endpag
               call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,nprsb,xmail,ymail,nreg,.false.)

  430          write(6,*) 'Souhaitez-vous choisir un autre coefficient?'
               write(6,*) 'Répondez 1 si oui, 0 si non.'
               read(5,*,end=340,err=430) tra
  340          continue
               if (tra.eq.1) go to 350

             enddo

             do iseg = 1,6

               lsd=(iseg*l2(nppol)/(iseg+1))
               if (iseg.eq.6) then
                 lsp = l2(nppol)
               else
                 lsp =((iseg+1)*l2(nppol)/(iseg+2))
               endif
               idep=iarr
               iarr=idep

               vx(1)=xmail(idep,ir,ireg)
               vy(1)=ymail(idep,ir,ireg)
               ls = 0
               do while (ls.lt.lsp-lsd.and.iarr.lt.nppol)
                 iarr=iarr+1
                 n=iarr-idep+1
                 vx(iarr-idep+1)=xmail(iarr,ir,ireg)
                 vy(iarr-idep+1)=ymail(iarr,ir,ireg)
                 ls=long(vx,vy,n)
               enddo

               xmini= minval(xmail(idep:iarr,1:ir,ireg))
               xmaxi= maxval(xmail(idep:iarr,1:ir,ireg))
               ymini= minval(ymail(idep:iarr,1:ir,ireg))
               ymaxi= maxval(ymail(idep:iarr,1:ir,ireg))

               tpr=n

               call endpag
               call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,nprsb,xmail,ymail,nreg,.false.)

 2211          write(6,*) 'Voulez-vous redefinir les bords?'
               write(6,*) 'Répondez 1 si c''est oui et 0 si c''est non'
               read(5,*,end=2212,err=2211) pra
 2212          if (pra.eq.0) go to 2350

 2213          write(6,*) 'Définissez les bords.'
 2214          write(6,*) 'Limite inférieure en R'
               read(5,*,end=2215, err=2214) xmini
 2215          write(6,*) 'Limite supérieure en R'
               read(5,*,end=2216,err=2215) xmaxi
 2216          write(6,*) 'Limite inférieure en Z'
               read (5,*,end=2217,err=2216) ymini
 2217          write(6,*) 'Limite supérieure en Z'
               read(5,*,end=2218,err=2217) ymaxi

 2218          call endpag
               call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,nprsb,xmail,ymail,nreg,.false.)

 2219          write(6,*) 'Recommencer?'
               write(6,*) 'Répondez 1 si c''est oui et 0 si c''est non'
               read(5,*,end=2220,err=2219) sra
 2220          if (sra.eq.1) go to 2213

 2350          write(6,*) 'Choississez un coefficient qui sera utilisé '
               write(6,'(a,i1,a,i1)') 'dans la partie supérieure à ',
     .          iseg,'/',iseg+1
               write(6,*) 'de nppol et qui multipliera la valeur '
               write(6,*) 'actuelle des ipol'
               read(5,*,end=2310,err=2350) coefa

 2310          continue
               do ipol=2,ipoln
                 if(l2(ipol).ge.lsd.and.l2(ipol).le.lsp)then
                   l2(ipol)= l2(ipol)*coefa
                   call coord(xn(1,inouv),yn(1,inouv),nn(inouv),
       .            l2(ipol),mailx(ipol,ir),maily(ipol,ir))
                 endif
               enddo

               call endpag
               call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,nprsb,xmail,ymail,nreg,.false.)

 2430          write(6,*) 'Souhaitez-vous choisir un autre coefficient?'
               write(6,*) 'Répondez 1 si oui, 0 si non.'
               read(5,*,end=2340,err=2430) tra
 2340          continue
               if (tra.eq.1) go to 2350

             enddo

           elseif (choix.eq.3) then

             IF (ir.eq.2) then

               do ipol=2,ipoln

       if(l2(ipol).lt.l2(nppol)/1.and.l2(ipol).gt.l2(nppol)*6/7)then
                   l2(ipol)=l2(ipol)*1.00
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)*6/7.and.l2(ipol).gt.l2(nppol)*5/6)
     .   then
                   l2(ipol)=l2(ipol)*1.001
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)*5/6.and.l2(ipol).gt.l2(nppol)*4/5)
     .   then
                   l2(ipol)=l2(ipol)*1.001
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)*4/5.and.l2(ipol).gt.l2(nppol)*3/4)
     .   then
                   l2(ipol)=l2(ipol)*1.001
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)*3/4.and.l2(ipol).gt.l2(nppol)*2/3)
     .   then
                   l2(ipol)=l2(ipol)*1.001
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)*2/3.and.l2(ipol).gt.l2(nppol)/2)then
                   l2(ipol)= l2(ipol)*1.009
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/2.and.l2(ipol).gt.l2(nppol)/3)then
                   l2(ipol)= l2(ipol)*1.01
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/3.and.l2(ipol).gt.l2(nppol)/4)then
                   l2(ipol)= l2(ipol)*1.015
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/4.and.l2(ipol).gt.l2(nppol)/5)then
                   l2(ipol)= l2(ipol)*1.02
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/5.and.l2(ipol).gt.l2(nppol)/6)then
                   l2(ipol)= l2(ipol)*1.022
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/6.and.l2(ipol).gt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.025
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.025
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
       endif

      enddo
        ELSEIF (ir.eq.3) then

      do        ipol=2,ipoln
         if(l2(ipol).lt.l2(nppol)/1.1.and.l2(ipol).gt.l2(nppol)/1.5)then
                   l2(ipol)=l2(ipol)*1.005
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/1.5.and.l2(ipol).gt.l2(nppol)/2)then
                   l2(ipol)= l2(ipol)*1.017
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/2.and.l2(ipol).gt.l2(nppol)/3)then
                   l2(ipol)= l2(ipol)*1.017
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/3.and.l2(ipol).gt.l2(nppol)/4)then
                   l2(ipol)= l2(ipol)*1.026
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/4.and.l2(ipol).gt.l2(nppol)/5)then
                   l2(ipol)= l2(ipol)*1.034
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/5.and.l2(ipol).gt.l2(nppol)/6)then
                   l2(ipol)= l2(ipol)*1.036
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/6.and.l2(ipol).gt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.045
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


        elseif(l2(ipol).lt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.04
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
        endif

       enddo

       ELSEIF (ir.eq.4) then

      do        ipol=2,ipoln
         if(l2(ipol).lt.l2(nppol)/1.1.and.l2(ipol).gt.l2(nppol)/1.5)then
                   l2(ipol)=l2(ipol)*1.007
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/1.5.and.l2(ipol).gt.l2(nppol)/2)then
                   l2(ipol)= l2(ipol)*1.016
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/2.and.l2(ipol).gt.l2(nppol)/3)then
                   l2(ipol)= l2(ipol)*1.02
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/3.and.l2(ipol).gt.l2(nppol)/4)then
                   l2(ipol)= l2(ipol)*1.032
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/4.and.l2(ipol).gt.l2(nppol)/5)then
                   l2(ipol)= l2(ipol)*1.04
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/5.and.l2(ipol).gt.l2(nppol)/6)then
                   l2(ipol)= l2(ipol)*1.048
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/6.and.l2(ipol).gt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.06
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


        elseif(l2(ipol).lt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.05
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
               endif

       enddo

       ELSEIF (ir.eq.5) then

      do        ipol=2,ipoln
         if(l2(ipol).lt.l2(nppol)/1.1.and.l2(ipol).gt.l2(nppol)/1.5)then
                   l2(ipol)=l2(ipol)*1.012
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/1.5.and.l2(ipol).gt.l2(nppol)/2)then
                   l2(ipol)= l2(ipol)*1.02
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/2.and.l2(ipol).gt.l2(nppol)/3)then
                   l2(ipol)= l2(ipol)*1.02
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/3.and.l2(ipol).gt.l2(nppol)/4)then
                   l2(ipol)= l2(ipol)*1.036
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/4.and.l2(ipol).gt.l2(nppol)/5)then
                   l2(ipol)= l2(ipol)*1.045
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/5.and.l2(ipol).gt.l2(nppol)/6)then
                   l2(ipol)= l2(ipol)*1.055
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/6.and.l2(ipol).gt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.062
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


        elseif(l2(ipol).lt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.062
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
               endif

       enddo

         ELSEIF (ir.eq.6) then

      do        ipol=2,ipoln
         if(l2(ipol).lt.l2(nppol)/1.1.and.l2(ipol).gt.l2(nppol)/1.5)then
                   l2(ipol)=l2(ipol)*1.009
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/1.5.and.l2(ipol).gt.l2(nppol)/2)then
                   l2(ipol)= l2(ipol)*1.016
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/2.and.l2(ipol).gt.l2(nppol)/3)then
                   l2(ipol)= l2(ipol)*1.022
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/3.and.l2(ipol).gt.l2(nppol)/4)then
                   l2(ipol)= l2(ipol)*1.04
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/4.and.l2(ipol).gt.l2(nppol)/5)then
                   l2(ipol)= l2(ipol)*1.052
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/5.and.l2(ipol).gt.l2(nppol)/6)then
                   l2(ipol)= l2(ipol)*1.056
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/6.and.l2(ipol).gt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.061
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


        elseif(l2(ipol).lt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.063
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
               endif

       ENDDO
       ELSEIF (ir.eq.7) then

      do        ipol=2,ipoln
         if(l2(ipol).lt.l2(nppol)/1.1.and.l2(ipol).gt.l2(nppol)/1.5)then
                   l2(ipol)=l2(ipol)*1.01
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/1.5.and.l2(ipol).gt.l2(nppol)/2)then
                   l2(ipol)= l2(ipol)*1.016
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/2.and.l2(ipol).gt.l2(nppol)/3)then
                   l2(ipol)= l2(ipol)*1.022
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/3.and.l2(ipol).gt.l2(nppol)/4)then
                   l2(ipol)= l2(ipol)*1.038
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/4.and.l2(ipol).gt.l2(nppol)/5)then
                   l2(ipol)= l2(ipol)*1.052
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/5.and.l2(ipol).gt.l2(nppol)/6)then
                   l2(ipol)= l2(ipol)*1.058
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/6.and.l2(ipol).gt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.063
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


        elseif(l2(ipol).lt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.058
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
               endif


       enddo
      ELSEIF (ir.eq.8) then

      do        ipol=2,ipoln
         if(l2(ipol).lt.l2(nppol)/1.1.and.l2(ipol).gt.l2(nppol)/1.5)then
                   l2(ipol)=l2(ipol)*1.01
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/1.5.and.l2(ipol).gt.l2(nppol)/2)then
                   l2(ipol)= l2(ipol)*1.016
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/2.and.l2(ipol).gt.l2(nppol)/3)then
                   l2(ipol)= l2(ipol)*1.02
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/3.and.l2(ipol).gt.l2(nppol)/4)then
                   l2(ipol)= l2(ipol)*1.039
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/4.and.l2(ipol).gt.l2(nppol)/5)then
                   l2(ipol)= l2(ipol)*1.045
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/5.and.l2(ipol).gt.l2(nppol)/6)then
                   l2(ipol)= l2(ipol)*1.047
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/6.and.l2(ipol).gt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.051
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


        elseif(l2(ipol).lt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.049
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
               endif


       enddo

       ELSEIF (ir.eq.9) then

      do        ipol=2,ipoln
         if(l2(ipol).lt.l2(nppol)/1.1.and.l2(ipol).gt.l2(nppol)/1.5)then
                   l2(ipol)=l2(ipol)*1.005
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/1.5.and.l2(ipol).gt.l2(nppol)/2)then
                   l2(ipol)= l2(ipol)*1.01
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/2.and.l2(ipol).gt.l2(nppol)/3)then
                   l2(ipol)= l2(ipol)*1.017
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/3.and.l2(ipol).gt.l2(nppol)/4)then
                   l2(ipol)= l2(ipol)*1.029
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/4.and.l2(ipol).gt.l2(nppol)/5)then
                   l2(ipol)= l2(ipol)*1.033
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/5.and.l2(ipol).gt.l2(nppol)/6)then
                   l2(ipol)= l2(ipol)*1.043
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/6.and.l2(ipol).gt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.044
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


        elseif(l2(ipol).lt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.043
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
               endif


       enddo

          ELSEIF (ir.eq.10) then

      do        ipol=2,ipoln
         if(l2(ipol).lt.l2(nppol)/1.1.and.l2(ipol).gt.l2(nppol)/1.5)then
                   l2(ipol)=l2(ipol)*1.00
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/1.5.and.l2(ipol).gt.l2(nppol)/2)then
                   l2(ipol)= l2(ipol)*1.00
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/2.and.l2(ipol).gt.l2(nppol)/3)then
                   l2(ipol)= l2(ipol)*1.00
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/3.and.l2(ipol).gt.l2(nppol)/4)then
                   l2(ipol)= l2(ipol)*1.005
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/4.and.l2(ipol).gt.l2(nppol)/5)then
                   l2(ipol)= l2(ipol)*1.005
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/5.and.l2(ipol).gt.l2(nppol)/6)then
                   l2(ipol)= l2(ipol)*1.01
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/6.and.l2(ipol).gt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.01
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


        elseif(l2(ipol).lt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.0
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
               endif


       enddo

       ELSEIF (ir.eq.11) then

      do        ipol=2,ipoln
         if(l2(ipol).lt.l2(nppol)/1.1.and.l2(ipol).gt.l2(nppol)/1.5)then
                   l2(ipol)=l2(ipol)*1.00
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/1.5.and.l2(ipol).gt.l2(nppol)/2)then
                   l2(ipol)= l2(ipol)*1.00
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/2.and.l2(ipol).gt.l2(nppol)/3)then
                   l2(ipol)= l2(ipol)*1.00
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/3.and.l2(ipol).gt.l2(nppol)/4)then
                   l2(ipol)= l2(ipol)*1.005
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/4.and.l2(ipol).gt.l2(nppol)/5)then
                   l2(ipol)= l2(ipol)*1.005
         call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


       elseif(l2(ipol).lt.l2(nppol)/5.and.l2(ipol).gt.l2(nppol)/6)then
                   l2(ipol)= l2(ipol)*1.01
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))

       elseif(l2(ipol).lt.l2(nppol)/6.and.l2(ipol).gt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.01
        call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))


        elseif(l2(ipol).lt.l2(nppol)/7)then
                   l2(ipol)= l2(ipol)*1.0
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
               endif


       enddo
         ELSEIF (ir.ge.12) then


c         IF (ir.lt.nprad/2) then


      do        ipol=2,ipoln
              if(l2(ipol).lt.l2(nppol)/1.1) then
                l2(ipol)=l2(ipol)*1.0

                 endif
                if(l2(ipol).lt.l2(nppol)/4) then
                   l2(ipol)= l2(ipol)*1.015


                 call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
              ENDIF
            enddo
        ENDIF
          ENDIF ! choix

          call endpag
        call trace(xmini,xmaxi,ymini,ymaxi,separx,separy,ptsep,
     .           npx,nptot,
     .           nstruc,npstru,xstruc,ystruc,nivx,nivy,nivtot,nbniv,
     .           np1,nprsb,xmail,ymail,nreg,.false.)

 321      write(6,*) 'Voulez-vous refaire des modifications sur cette '
          write(6,*) 'ligne de niveau?'
          write(6,*) '(1 si c''est oui et 0 si c''est non)'
          read(5,*,end=25,err=321) pri
          if (pri.eq.1) goto 1000

        endif  ! modif
#endif
c---
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
