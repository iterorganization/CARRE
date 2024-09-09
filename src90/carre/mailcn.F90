      SUBROUTINE MAILCN(mailx,maily, &
     &               xn1,yn1,nn1,pntrat,pas,nppol,nprad, &
     &               x2,y2,fctini,nx,ny,x,y,psi, &
     &               nstruc,npstru,xstruc,ystruc, &
     &               a00,a10,a01,a11,repart,xptxo,yptxo,xpto,ypto, &
     &               nivx,nivy,nivtot,nbniv,distxo,diag,ireg)
!
!  version : 07.07.97 18:47
!
!======================================================================
      use KindDefinitions, only : rKind
#ifdef USE_SILO
      use SiloIO
#endif
      use CarreDiagnostics, only : CarreDiag, cdClearRegion
      use CarreSiloIO, only : csIOSetSurface, csIOSetRelax
      use carre_niveau, only : crbniv
      use carre_criteria, only : clort
      use carre_dimensions
      use comlan
      use comrlx
      use comort

      IMPLICIT NONE
#ifdef USE_SILO
      logical, parameter :: DEBUGFILES_MAILCN = .false.
#endif

!..  Cette sous-routine fait le maillage curviligne orthogonal dans
!  la region centrale.

!  arguments
      INTEGER nx,ny,nstruc,npstru(nstruc), &
     &  nn1,nppol,nprad,repart,nivtot(nivmx),nbniv,ireg

      REAL(rKind) :: x(nxmax),y(nymax),psi(nxmax,nymax),xstruc(npstmx,strumx), &
     &  ystruc(npstmx,strumx),mailx(npmamx,nrmamx), &
     &  maily(npmamx,nrmamx),xn1(nn1),yn1(nn1),pas(nrmamx),x2,y2, &
     &  fctini,a00(nxmax,nymax,3),a10(nxmax,nymax,3), &
     &  a01(nxmax,nymax,3),a11(nxmax,nymax,3), &
     &  xptxo,yptxo,xpto,ypto,nivx(npnimx,nivmx), &
     &  nivy(npnimx,nivmx),pntrat,distxo

      type(CarreDiag), intent(inout) :: diag

!  variables locales
      INTEGER ipas,indstr,ianc,inouv,ii,jj,ir,dir,ipol,i,nn(2),nt,sens &
     &        ,npcrb(2),plaque
      REAL(rKind) :: ll,zero,pasini,epsmai,dist,dernie,valfct,xn(npnimx,2), &
     &       yn(npnimx,2),fctnew,xt(3),yt(3)
      PARAMETER(zero=0.,epsmai=1.e-6)
      REAL(rKind) :: xcrb(npnimx,2),ycrb(npnimx,2)
      CHARACTER*1 reponse

      ! ort1,2: for current radial surface
      ! ort1: grid optimization function for first distribution of points
      ! ort2: grid optimization function for current distribution of points
      REAL(rKind) :: ort1(npmamx), ort2(npmamx)
      ! individual contributions to the grid optimization function for current
      ! radial surface (i.e. individual components of ort1,2)
      ! ortpur: orthogonality function,
      ! propo: proportional distribution w.r.t. separatrix,
      ! varr: minimal distance between points
      REAL(rKind) :: ortpur(npmamx),propo(npmamx),varr(npmamx)

#ifdef USE_SILO
      ! cort,cortpur/propo/varr: optimization function + individual contributions for final
      ! grid point distribution for all grid nodes placed up to now
      REAL(rKind) :: cort(npmamx,nrmamx),cortpur(npmamx,nrmamx), &
                   & cpropo(npmamx,nrmamx),cvarr(npmamx,nrmamx)
#endif

      integer :: ntt
      REAL(rKind) :: fctxo, xtt(5), ytt(5), x22, y22, x23, y23, fctanc

!  procedures
      INTEGER ifind
      REAL(rKind) :: aazero,leng,nulort,ruban
      INTRINSIC MOD,abs
      EXTERNAL aazero,leng,COORD1,ifind,nulort,UNTANG,ruban,saute,pltend,coord
!
!  remarque: cette routine doit etre appelee apres frtier ou, dans le
!            cas d'une configuration limiteur, apres limfnd car elle
!            suppose que nbniv associe aux frontieres exterieures est
!            deja connu. La derniere frontiere est donc la frontiere
!            interieure.
!======================================================================
!
!..calculs
!
!***
!     print*,'entree dans mailcn: nxmax, ny, npstmx, nstruc= ',
!    .  nxmax,ny,npstmx,nstruc
!     print*,'npmamx,nrmamx, nn1, nxmax,nymax,nivmx=',
!    ,       npmamx,nrmamx,nn1,nxmax,nm2,
!    .  nivmx
!     print*,'npnimx=',npnimx
!***

!..Initialisation.

      ! clear diagnostic values for this region
      call cdClearRegion( diag, ireg )

      plaque=0

      DO 10 i=1,nn1
         xn(i,1)=xn1(i)
         yn(i,1)=yn1(i)
   10 CONTINUE

      nn(1)=nn1

!.. On prepare une structure artificielle pour trouver la distance
!.. horizontale vers l'exterieur entre le point O et la separatrice
!.. soit le petit rayon a du plasma

      IF (repart .EQ. 2) THEN

         valfct=fctini

         xt(1)=xn(1,1)
         yt(1)=yn(1,1)
         xt(2)=xpto
         yt(2)=ypto
         xt(3)=xt(1)
         yt(3)=yt(1)
         nt=3

      ENDIF

      ii = ifind(xpto,x,nx,1)
      jj = ifind(ypto,y,ny,1)
      fctxo= a00(ii,jj,1) + a10(ii,jj,1)*xpto + a01(ii,jj,1)*ypto + &
     &       a11(ii,jj,1)*xpto*ypto
      xtt(1)= xpto-0.1
      ytt(1)= ypto
      xtt(2)= x(nx)
      ytt(2)= ypto
      xtt(3)= xtt(1)
      ytt(3)= ytt(1)
      ntt=3
      sens=1
      CALL SAUTE(xtt,ytt,ntt,xpto,ypto,fctxo,x22,y22,fctini,sens, &
     &            2,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

!..Calcul des longueurs

      diag%gdpsi(1,ireg)=1.0
      diag%racpsi(1,ireg)=1.0
      diag%gdr(1,ireg)=x22
      diag%r(1,ireg)=x22-xpto
      diag%a(ireg)=diag%r(1,ireg)
      diag%ra(1,ireg)= diag%r(1,ireg)-diag%a(ireg)
      diag%rho(1,ireg)=diag%r(1,ireg)/diag%a(ireg)

      DO 25 ir=2, nprad

        call csioSetSurface( ir )

!---
        print*,'ir=',ir
!---
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

            CALL SAUTE(xt,yt,nt,x1,y1,valfct,x2,y2,fctnew,sens, &
     &               repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

          ENDIF

          fctanc=valfct
!..Parametrisation de la ligne de niveau qui passe par ce point.

          xn(1,inouv)=x2
          yn(1,inouv)=y2
          nn(inouv)=1

          ii=ifind(x2,x,nx,1)
          jj=ifind(y2,y,ny,1)

          valfct=a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + &
     &           a11(ii,jj,1)*x2*y2

!..Definition de la courbe a ne pas traverser.

          DO 14 i=1, nn(ianc)
           xcrb (i,1)=xn(i,ianc)
           ycrb (i,1)=yn(i,ianc)
   14     CONTINUE

          npcrb(1)=nn(ianc)
          npcrb(2)=0

!..Recherche du deuxieme point.

          dir=0

!***
!         print*,'call crbniv1 - mailcn'
!***
          CALL CRBNIV(ii,jj,nn(inouv),dir,nx,ny,x,y,psi, &
     &            valfct,xn(1,inouv),yn(1,inouv), &
     &            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1, &
     &            plaque,x2,y2)

!..Il faut s'assurer que la ligne de niveau part dans la bonne
!  direction.

!0195     IF (xn(2,inouv) .LT. xn(1,inouv)) THEN
          if((xn(2,inouv)-xn(1,inouv))*(xn(2,ianc)-xn(1,ianc))+ &
     &       (yn(2,inouv)-yn(1,inouv))*(yn(2,ianc)-yn(1,ianc)) &
     &          .lt.zero) then
            nn(inouv)=1
            dir=MOD(dir+1,4) + 1
            ii=ii - MOD(dir-2,2)
            jj=jj - MOD(dir-3,2)
          ENDIF
!..Pour les points successifs, on poursuit jusqu'a ce qu'on ferme la
!  boucle.
!***
!         print*,'call crbniv2 - mailcn'
!***

          CALL CRBNIV(ii,jj,nn(inouv),dir,nx,ny,x,y,psi, &
     &            valfct,xn(1,inouv),yn(1,inouv), &
     &            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1, &
     &            plaque,x2,y2)

!.. Calcul de grand psi

          diag%gdpsi(ir,ireg)= (valfct-fctxo)/(fctini-fctxo)
          diag%racpsi(ir,ireg)= sqrt(diag%gdpsi(ir,ireg))
          sens=2
          x1 = diag%gdr(ir-1,ireg)
          y1 = ypto
          CALL SAUTE(xtt,ytt,ntt,x1,y1,fctanc,x23,y23,fctnew,sens, &
     &               repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)

!..Calcul des longueurs

          diag%gdr(ir,ireg)=x23
          diag%r(ir,ireg)=x23-xpto
          diag%ra(ir,ireg)=diag%r(ir,ireg)-diag%a(ireg)
          diag%rho(ir,ireg)=diag%r(ir,ireg)/diag%a(ireg)
!..Le dernier point de la courbe est egal au premier.

          xn(nn(inouv),inouv)=xn(1,inouv)
          yn(nn(inouv),inouv)=yn(1,inouv)

!..Definition de xn2 et yn2 pour le bloc common comort.

          DO 18 ipas=1,nn(inouv)
            xn2(ipas)=xn(ipas,inouv)
            yn2(ipas)=yn(ipas,inouv)
   18     CONTINUE

          npni2=nn(inouv)

!..On definit maintenant les points de maille de la nouvelle ligne
!  de niveau a partir de ceux de la precedente.

          ll=leng(xn2,yn2,npni2)
          period=ll
!***
!         print*,'ll, period=',ll,period
!***

!---
          if(nrelax.ge.0) then
            d1=zero
            ipol1=2
            ipoln=nppol
            if(ir.eq.2) l0(nppol)=leng(xn(1,ianc),yn(1,ianc),nn(ianc))
!
!  1.   on dispose d'abord les points proportionellement a ceux de la
!       ligne precedente
            do ipol=1,ipoln
              d1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ipol,ir-1), &
     &           maily(ipol,ir-1),d1)
              if(ir.eq.2) l0(ipol)=d1
              l1(ipol)=(l0(ipol)/l0(nppol))*ll
              call coord1(xn(1,inouv),yn(1,inouv),nn(inouv), &
     &          l1(ipol),mailx(ipol,ir),maily(ipol,ir),period)
            enddo
            if(nrelax.gt.0) then
!
              l1(nppol+1)=l1(nppol)+l1(2)-l1(1)
              mailx(nppol+1,ir-1)=mailx(2,ir-1)
              maily(nppol+1,ir-1)=maily(2,ir-1)
              mailx(nppol+1,ir)=mailx(2,ir)
              maily(nppol+1,ir)=maily(2,ir)
!***
!             print*,'call clort'
!***
!
!  2.   initialisation de la fonction qui doit s'annuler pour une
!       distribution orthogonale
              call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir), &
     &          maily(1,ir),ort1,nppol+1,pasmin,zero,zero,l0,l1, &
     &          ortpur,propo,varr)

#ifdef USE_SILO
              ! collect optimization function for all surfaces
              cort(1:nppol,ir) = ort1(1:nppol)
              cortpur(1:nppol,ir) = ortpur(1:nppol)
              cpropo(1:nppol,ir) = propo(1:nppol)
              cvarr(1:nppol,ir) = varr(1:nppol)
#endif

!***
!             print*,'point 3'
!***
!
!  3.   on procede a un premier deplacement des noeuds
              l2(1)=l1(1)
              l2(nppol+1)=l1(nppol+1)
              diag%segt(ir,ireg)=l2(nppol)
              do ipol=ipol1,ipoln
                if(ort1(ipol).gt.zero) then
                  l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol+1)
                else
                  l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol-1)
                endif
                call coord1(xn(1,inouv),yn(1,inouv),nn(inouv), &
     &            l2(ipol),mailx(ipol,ir),maily(ipol,ir),period)

                diag%somort(ir,ireg)= diag%somort(ir,ireg)+(ort1(ipol)/nppol)
                diag%somortpur(ir,ireg)= diag%somortpur(ir,ireg)+(ortpur(ipol)/nppol)
                diag%sompropo(ir,ireg)= diag%sompropo(ir,ireg)+(propo(ipol)/nppol)
                diag%somvarr(ir,ireg)= diag%somvarr(ir,ireg)+(varr(ipol)/nppol)
              enddo
!***
!             print*,'point 4'
!***
!
!  4.   on relaxe les points de facon iterative pour realiser la
!       meilleure orthogonalite possible
              do i=1,nrelax
                call csioSetRelax( i )

                call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir), &
     &            maily(1,ir),ort2,nppol+1,pasmin,zero,zero,l0,l2, &
     &            ortpur,propo,varr)

#ifdef USE_SILO
                ! collect optimization function for all surfaces
                cort(1:nppol,ir) = ort2(1:nppol)
                cortpur(1:nppol,ir) = ortpur(1:nppol)
                cpropo(1:nppol,ir) = propo(1:nppol)
                cvarr(1:nppol,ir) = varr(1:nppol)
#endif

                ortmax=zero
                do ipol=ipol1,ipoln
                  if(abs(ort2(ipol)).gt.rlcept) then
                    del=-ort2(ipol)*(l2(ipol)-l1(ipol)) &
     &                             /(ort2(ipol)-ort1(ipol))
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
                    call coord1(xn(1,inouv),yn(1,inouv),nn(inouv), &
     &               l2(ipol),mailx(ipol,ir),maily(ipol,ir),period)
                  endif
                  ortmax=max(ortmax,abs(ort2(ipol)))
                enddo

#ifdef USE_SILO
                ! TODO: maybe move this into a subroutine in CarreSiloIO
                ! write out current grid status

                ! entire grid created so far
                if (DEBUGFILES_MAILCN) then
                call csioOpenFile()

                call siloWriteQuadGrid( csioDbfile, "region", &
                     & nppol, ir, &
                     & mailx(1:nppol, 1:ir), maily(1:nppol, 1:ir) )

                call siloWriteQuadData( csioDbfile, "region", "cort", &
                     & cort(1:nppol, 1:ir), DB_NODECENT )
                call siloWriteQuadData( csioDbfile, "region", "cortpur", &
                     & cortpur(1:nppol, 1:ir), DB_NODECENT )
                call siloWriteQuadData( csioDbfile, "region", "cpropo", &
                     & cpropo(1:nppol, 1:ir), DB_NODECENT )
                call siloWriteQuadData( csioDbfile, "region", "cvarr", &
                     & cvarr(1:nppol, 1:ir), DB_NODECENT )

                ! only current flux surface
                call siloWriteLineSegmentGridFromPoints( csioDbfile, &
                     & "currentsurface", mailx(1:nppol,ir), maily(1:nppol,ir) )
                call siloWriteUMData( csioDbfile, "currentsurface", "ort", &
                     & siloExpandSegmentData( ort2(1:nppol) ), DB_NODECENT )
                call siloWriteUMData( csioDbfile, "currentsurface", "ortpur", &
                     & siloExpandSegmentData( ortpur(1:nppol) ), DB_NODECENT )
                call siloWriteUMData( csioDbfile, "currentsurface", "propo", &
                     & siloExpandSegmentData( propo(1:nppol) ), DB_NODECENT )
                call siloWriteUMData( csioDbfile, "currentsurface", "varr", &
                     & siloExpandSegmentData( varr(1:nppol) ), DB_NODECENT )
                end if
#endif

                if(ortmax.le.rlcept) go to 19
!***
!               print*,'point 5'
!***
!
!  5.   on redefinit la reference et les positions des points pivots
                l1(1)=l1(nppol)-period
                l1(nppol+1)=l1(2)+period
                l2(1)=l1(1)
                l2(nppol+1)=l1(nppol+1)
                mailx(1,ir)=mailx(nppol,ir)
                maily(1,ir)=maily(nppol,ir)
                mailx(nppol+1,ir)=mailx(2,ir)
                maily(nppol+1,ir)=maily(2,ir)
              enddo
!***
              if(sellan(1:8).eq.'francais') then
                print*, &
     &           'L''algorithme adaptatif d''optimisation de '// &
     &           'l''orthogonalite de la maille n''a pas converge !'
                print*,'ortmax=',ortmax
                print*,'Pensez a relancer le programme avec: '
                print '(a,1p,i8)', &
     &           '1) Un plus grand nombre d''iterations, '// &
     &            'actuellement nrelax = ',nrelax
                print '(a,1p,1f12.4)', &
     &           '2) Un autre facteur de relaxation, '// &
     &            'actuellement relax = ',relax
                print '(a,1p,1f12.4)', &
     &           '3) Un espacement minimal plus petit, '// &
     &            'actuellement pasmin = ',pasmin
                print '(a,1p,1f12.4)', &
     &           '4) Un critere d''orthogonalite moins strict, '// &
     &            'actuellement rlcept = ',rlcept
                if ( .not. AUTOCONTINUE ) print*, &
     &           'Voulez-vous continuer (o/n) ?'
              elseif(sellan(1:7).eq.'english') then
                print*, &
     &           'Mesh adaptation algorithm to optimize orthogonality'// &
     &           ' did not converge !'
                print*,'ortmax=',ortmax
                print*,'Consider re-running with: '
                print '(a,1p,i8)', &
     &           '1) A larger number of iterations, '// &
     &            'currently nrelax = ',nrelax
                print '(a,1p,1f12.4)', &
     &           '2) Changing the relaxation factor, '// &
     &            'currently relax = ',relax
                print '(a,1p,1f12.4)', &
     &           '3) A smaller minimal spacing, '// &
     &            'currently pasmin = ',pasmin
                print '(a,1p,1f12.4)', &
     &           '4) A less stringent orthogonality criterion, '// &
     &            'currently rlcept = ',rlcept
                if ( .not. AUTOCONTINUE ) print*, &
     &           'Do you wish to continue (y/n) ?'
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
     &           'Verifiez attentivement la qualite de la maille '// &
     &           'a la sortie du programme !'
              elseif(sellan(1:7).eq.'english') then
                print*, &
     &           'Please check the quality of the mesh '// &
     &           'carefully upon completion !'
              endif
!***
 19           continue
              do ipol=ipol1,ipoln
                      diag%somortp(ir,ireg) = diag%somortp(ir,ireg) + &
                                            & (ort2(ipol)/nppol)
                      diag%somortpurp(ir,ireg) = diag%somortpurp(ir,ireg) + &
                                            & (ortpur(ipol)/nppol)
                      diag%sompropop(ir,ireg) = diag%sompropop(ir,ireg) + &
                                            & (propo(ipol)/nppol)
                      diag%somvarrp(ir,ireg) = diag%somvarrp(ir,ireg) + &
                                            & (varr(ipol)/nppol)
              enddo
!             print*,'apres 19 continue'
!***

            endif

          else
!---
            dernie=-0.025
            DO 20 ipol=1,nppol-1

!..Definition de x1 et y1 pour le bloc common comort.

              x1=mailx(ipol, ir-1)
              y1=maily(ipol, ir-1)

              CALL UNTANG(xn(1,ianc),yn(1,ianc),nn(ianc),x1,y1,ux1,uy1, &
     &                   dernie,period)

              pasini=0.3*ll
              dist=aazero(nulort,dernie,pasini,epsmai,zero,dernie,ll,50)
              dernie=dist
              CALL COORD(xn(1,inouv),yn(1,inouv),nn(inouv),dist, &
     &               mailx(ipol,ir),maily(ipol,ir))

   20       CONTINUE
!---
          endif
!---

!..Les premier et dernier points de la maille coincident.

          mailx(nppol,ir)=mailx(1,ir)
          maily(nppol,ir)=maily(1,ir)
        endif

   25 CONTINUE
!***
!     print*,'apres 25 continue'
!***
!
!  on definit la frontiere interieure
!..On remet la periode a zero.
      if(pntrat.lt.distxo) then
        nbniv=nbniv+1
        nivtot(nbniv)=nn(inouv)
        do 30 i=1,nn(inouv)
          nivx(i,nbniv)=xn(i,inouv)
          nivy(i,nbniv)=yn(i,inouv)
   30   continue
      endif

!..On remet la periode a zero.

      period=zero

      do ir=2,nprad
              diag%gdsomortp(ireg) = diag%gdsomortp(ireg) + &
                                   & (diag%somortp(ir,ireg)/(nprad-1))
              diag%gdsomortpurp(ireg) = diag%gdsomortpurp(ireg) + &
                                   & (diag%somortpurp(ir,ireg)/(nprad-1))
              diag%gdsompropop(ireg) = diag%gdsompropop(ireg) + &
                                   & (diag%sompropop(ir,ireg)/(nprad-1))
              diag%gdsomvarrp(ireg) = diag%gdsomvarrp(ireg) + &
                                   & (diag%somvarrp(ir,ireg)/(nprad-1))
      enddo
      RETURN
      END
