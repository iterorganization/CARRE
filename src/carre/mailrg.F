      SUBROUTINE MAILRG(mailx,maily,xn1,yn1,nn1,sens,pas,nppol,nprad,
     .            plaque,x2,y2,nx,ny,x,y,psi,nstruc,npstru,
     .            xstruc,ystruc,a00,a10,a01,a11,
     .            repart,gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2,
     .            xnlast,ynlast,nnlast,nuldec)
c
c  version : 07.07.97 19:11
c
c======================================================================
      IMPLICIT NONE

*..  Cette sous-routine fait le maillage curviligne orthogonal dans
*  une region.


cank-970707: dimensions from the file
*  dimensions
#include <CARREDIM.F>

*  arguments
      INTEGER nx,ny,nstruc,npstru(nstruc),nn1,sens,nppol,nprad
     .        ,plaque,repart,nbcrb,npcrb2,nnlast

      REAL*8 x(nxmax),y(nymax),psi(nxmax,nymax),xstruc(npstmx,nstruc),
     .     ystruc(npstmx,nstruc),mailx(npmamx,nrmamx),
     .     maily(npmamx,nrmamx),xn1(nn1),yn1(nn1),pas(nrmamx),x2,y2,
     .     a00(nxmax,nymax,3),a10(nxmax,nymax,3),a01(nxmax,nymax,3),
     .     a11(nxmax,nymax,3),gardd1,gardd2,
     .     xcrb2(npcrb2),ycrb2(npcrb2),xnlast(nnlast),ynlast(nnlast)

      LOGICAL nuldec

*  variables en common

#include <COMORT.F>
#include <COMLAN.F>
#include <COMRLX.F>

*  variables locales
      INTEGER ipas,indstr,ianc,inouv,ind,ii,jj,ir,dir,ipol,i,nn(2),
     .  ig1,ig2,npcrb(2)
      REAL*8 ll,zero,pasini,epsmai,dist,dernie,valfct,ecart1,ecart2,
     .       epsiln,xn(npnimx,2),yn(npnimx,2),dd1,dd2,gard1,gard2
     .       ,fctini,fctnew
      PARAMETER(zero=0.,epsmai=1.e-6,epsiln=1.E-08)
      REAL*8 xcrb(npnimx,2),ycrb(npnimx,2)
      CHARACTER*1 reponse

*  procedures
      INTEGER indsgm,ifind
      REAL*8 aazero,long,nulort,ruban
      LOGICAL chgdir,in,cross
      INTRINSIC MOD,SQRT
      EXTERNAL aazero,long,COORD,indsgm,ifind,CRBNIV,nulort,
     .         UNTANG,SAUTE,chgdir,in,cross,ruban,clort
c======================================================================
*..calculs
*

*..Copie des valeurs de garde pour le bloc common comort.

      garde1=gardd1
      garde2=gardd2

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
     .         ' garde1=',1pe10.3,' garde2=',1pe10.3,' ll=',1pe10.3)
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
     .                npstru(plaque),x1,y1,valfct,x2,y2,pas(ir-1),sens,
     .                repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

         ELSE IF (repart .EQ. 2) THEN

            fctnew=valfct + pas(ir-1)

            CALL SAUTE(xstruc(1,plaque),ystruc(1,plaque),
     .                npstru(plaque),x1,y1,valfct,x2,y2,fctnew,sens,
     .                repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

         ENDIF
c***
c        print*,'x1, y1=',x1,y1
c        print*,'x2, y2=',x2,y2
c***

         xn(1,inouv)=x2
         yn(1,inouv)=y2
         mailx(1,ir)=x2
         maily(1,ir)=y2
         nn(inouv)=1

         ind=indsgm(xstruc(1,plaque),ystruc(1,plaque),
     .                  npstru(plaque),x2,y2)

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
     .                    npstru(plaque))))   THEN

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
     .                    npstru(plaque))))   THEN

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
     .         maily(1,ir),ort1,nppol,pasmin,garde1,garde2,l0,l1)
c
c  3.   on procede a un premier deplacement des noeuds
               l2(1)=zero
               l2(nppol)=l1(nppol)
               do ipol=ipol1,ipoln
                 if(ort1(ipol).gt.zero) then
                   l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol+1)
                 else
                   l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol-1)
                 endif
                 call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol),
     .             mailx(ipol,ir),maily(ipol,ir))
               enddo
c
c  4.   on relaxe les points de facon iterative pour realiser la
c       meilleure orthogonalite possible
             do i=1,nrelax
               call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir),
     .           maily(1,ir),ort2,nppol,pasmin,garde1,garde2,l0,l2)
               ortmax=zero
               do ipol=ipol1,ipoln
                 if(abs(ort2(ipol)).gt.rlcept) then
                   del=-ort2(ipol)*(l2(ipol)-l1(ipol))
     .                            /(ort2(ipol)-ort1(ipol))
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
               print*,
     .          '1) Un plus grand nombre d''iterations, '//
     .           'actuellement nrelax = ',nrelax
               print*,
     .          '2) Un autre facteur de relaxation, '//
     .           'actuellement relax = ',relax
               print*,
     .          '3) Un espacement minimal plus petit, '//
     .           'actuellement pasmin = ',pasmin
               print*,
     .          '4) Un critere d''orthogonalite moins strict, '//
     .           'actuellement rlcept = ',rlcept
               print*,
     .          '5) Des longueurs de garde plus importantes, '//
     .           'actuellement tgarde = ',garde1,garde2
               print*,
     .          'Voulez-vous continuer (o/n) ?'
             elseif(sellan(1:7).eq.'english') then
               print*,
     .          'Mesh adaptation algorithm to optimize orthogonality'//
     .          ' did not converge !'
               print*,'ortmax=',ortmax
               print*,'Consider re-running with: '
               print*,
     .          '1) A larger number of iterations, '//
     .           'currently nrelax = ',nrelax
               print*,
     .          '2) Changing the relaxation factor, '//
     .           'currently relax = ',relax
               print*,
     .          '3) A smaller minimal spacing, '//
     .           'currently pasmin = ',pasmin
               print*,
     .          '4) A less stringent orthogonality criterion, '//
     .           'currently rlcept = ',rlcept
               print*,
     .          '5) Longer guard lengths, '//
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

c
c  distribution des points dans la region de garde
          gard2=ruban(xn(1,inouv),yn(1,inouv),nn(inouv),mailx(ig1,ir),
     .      maily(ig1,ir),zero)
          gard1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ig1,ir-1),
     .      maily(ig1,ir-1),zero)
          do 22 ipol=2,ig1-1
          dd1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ipol,ir-1),
     .      maily(ipol,ir-1),zero)
          dd2=dd1/gard1*gard2
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),dd2,
     .      mailx(ipol,ir),maily(ipol,ir))
22        continue
          gard2=ll-ruban(xn(1,inouv),yn(1,inouv),nn(inouv),
     .      mailx(ig2,ir),maily(ig2,ir),zero)
          gard1=ll1-ruban(xn(1,ianc),yn(1,ianc),nn(ianc),
     .      mailx(ig2,ir-1),maily(ig2,ir-1),zero)
          do 23 ipol=ig2+1,nppol-1
          dd1=ll1-ruban(xn(1,ianc),yn(1,ianc),nn(ianc),
     .      mailx(ipol,ir-1),maily(ipol,ir-1),zero)
          dd2=ll-dd1/gard1*gard2
          call coord(xn(1,inouv),yn(1,inouv),nn(inouv),dd2,
     .      mailx(ipol,ir),maily(ipol,ir))
23        continue

*..Le dernier point de la maille coincide avec le dernier point de la
*  courbe.
         mailx(nppol,ir)=xn(nn(inouv),inouv)
         maily(nppol,ir)=yn(nn(inouv),inouv)
c---
         endif
c---
   25 CONTINUE

      RETURN
      END
