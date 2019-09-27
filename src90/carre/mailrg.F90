SUBROUTINE MAILRG(mailx,maily,xn1,yn1,nn1,sens,pas,nppol,nprad, & 
     & plaque,x2,y2,nx,ny,x,y,psi,xpto,ypto,nstruc,npstru, & 
     & xstruc,ystruc,a00,a10,a01,a11, & 
     & repart,gardd1,gardd2,nbcrb,xcrb2,ycrb2,npcrb2, & 
     & xnlast,ynlast,nnlast,nuldec,&
     & xpind,xpx,xpy,&
     & solregion,diag,ireg,&
     & struct, extended_grid_limiter)
!
!  version : 07.07.97 19:11
!
!======================================================================
      use CarreSiloIO
      use CarreDiagnostics
      use carre_niveau
      use carre_criteria
      use carre_target
#ifdef USE_SILO
      use SiloIO
#endif
      use carre_target

      IMPLICIT NONE
      
      logical, parameter :: DEBUGFILES_MAILRG = .false.

      ! Cette sous-routine fait le maillage curviligne orthogonal dans
      ! une region.
      ! 
      ! Parameters:
      ! nuldec: if .true., indicates we are gridding the inner SOL of a disconnected double null
      ! xpind: poloidal grid point index of the outer X-point
      ! xpx,ypx: coordinates of outer X-point

#include <CARREDIM.F>

!  arguments
      INTEGER nx,ny,nstruc,npstru(nstruc),nn1,sens,nppol,nprad & 
     &        ,plaque,repart,nbcrb,npcrb2,nnlast,ireg,xpind

      REAL*8 x(nxmax),y(nymax),psi(nxmax,nymax),xstruc(npstmx,nstruc), & 
     &     ystruc(npstmx,nstruc),mailx(npmamx,nrmamx), & 
     &     maily(npmamx,nrmamx),xn1(nn1),yn1(nn1),pas(nrmamx),x2,y2, & 
     &     a00(nxmax,nymax,3),a10(nxmax,nymax,3),a01(nxmax,nymax,3), & 
     &     a11(nxmax,nymax,3),gardd1,gardd2, & 
     &     xcrb2(npcrb2),ycrb2(npcrb2),xnlast(nnlast),ynlast(nnlast),&
     &     xpx, xpy
      REAL*8 :: xpto, ypto

      ! FIXME: find a better solution for avoiding the explicit extended_grid_limiter flag.
      LOGICAL nuldec, extended_grid_limiter
      logical solregion ! is region part of SOL?

      type(CarreDiag), intent(inout) :: diag
      type(CarreStructures), intent(in) :: struct

!  variables en common

#include <COMORT.F>
#include <COMLAN.F>
#include <COMRLX.F>

!  variables locales
      INTEGER ipas,indstr,ianc,inouv,ind,ii,jj,ir,dir,ipol,i,nn(2), & 
     &  ig1,ig2,npcrb(2)
      REAL*8 ll,zero,pasini,epsmai,dist,dernie,valfct,ecart1,ecart2, & 
     &       epsiln,xn(npnimx,2),yn(npnimx,2),dd1,dd2,gard1,gard2, & 
     &       fctini,fctnew
      PARAMETER(zero=0.,epsmai=1.e-6,epsiln=1.E-08)
      REAL*8 xcrb(npnimx,2),ycrb(npnimx,2)
      CHARACTER*1 reponse

      ! ort1,2: for current radial surface
      ! ort1: grid optimization function for first distribution of points
      ! ort2: grid optimization function for current distribution of points
      REAL*8 :: ort1(npmamx), ort2(npmamx)
      ! individual contributions to the grid optimization function for current
      ! radial surface (i.e. individual components of ort1,2)
      ! ortpur: orthogonality function,
      ! propo: proportional distribution w.r.t. separatrix,
      ! varr: minimal distance between points
      REAL*8 :: ortpur(npmamx),propo(npmamx),varr(npmamx)

      ! cort,cortpur/propo/varr: optimization function + individual contributions for final
      ! grid point distribution for all grid nodes placed up to now
      REAL*8 :: cort(npmamx,nrmamx),cortpur(npmamx,nrmamx),cpropo(npmamx,nrmamx),cvarr(npmamx,nrmamx)

      integer :: ntt, sensspe
      REAL*8 :: fctxo, xtt(5), ytt(5), x22, y22, x23, y23, fctanc, length_xp

!  procedures
      INTEGER ifind
      REAL*8 aazero,long,nulort,ruban
      LOGICAL chgdir,cross
      INTRINSIC MOD,SQRT
      EXTERNAL aazero,long,COORD,ifind,nulort, & 
     &         UNTANG,SAUTE,chgdir,cross,ruban
!======================================================================
!..calculs
!

!..Copie des valeurs de garde pour le bloc common comort.

      garde1=gardd1
      garde2=gardd2
	
      ! clear diagnostic values for this region
      call cdClearRegion( diag, ireg )

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

!..Calcul de la fonction pour le premier point de la courbe de reference

      ii=ifind(xn(1,1),x(1:nx),nx,1)
      jj=ifind(yn(1,1),y(1:ny),ny,1)

      fctini=a00(ii,jj,1) + a10(ii,jj,1)*xn(1,1) + & 
     &       a01(ii,jj,1)*yn(1,1) + a11(ii,jj,1)*xn(1,1)*yn(1,1)

      valfct=fctini

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

        
      ! compute psi at O-point
      ii = ifind(xpto,x,nx,1)
      jj = ifind(ypto,y,ny,1)
      fctxo= a00(ii,jj,1) + a10(ii,jj,1)*xpto + a01(ii,jj,1)*ypto + a11(ii,jj,1)*xpto*ypto

      ! la valeur du sol est précisé dans 'maille' selon l'indice de région et la configuration
      ! sol = 1 indicates an SOL region
      if (solregion) then

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
                   & 2,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)
              
              !..Calcul des longueurs
              diag%gdpsi(1,ireg)=1.0
              diag%racpsi(1,ireg)=1.0
              diag%gdr(1,ireg)=x22
              diag%r(1,ireg)=x22-xpto
              diag%a(ireg)=diag%r(1,ireg)
              diag%ra(1,ireg)= diag%r(1,ireg)-diag%a(ireg)
              diag%rho(1,ireg)=diag%r(1,ireg)/diag%a(ireg)
              
              fctanc=valfct
      else
              diag%gdpsi(1,ireg)=1.0
              diag%racpsi(1,ireg)=1.0
      endif

      DO 25 ir=2, nprad

         call csioSetSurface( ir )

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

            xn(1:nnlast, inouv) = xnlast(1:nnlast)
            yn(1:nnlast, inouv) = ynlast(1:nnlast)

            nn(inouv)=nnlast

            mailx(1,ir)=xnlast(1)
            maily(1,ir)=ynlast(1)

            ! in this case, do not search for the level line

        else
            ! in the default situation, we have to find the level line 


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

! Before creating a new grid line by distributing points on a poloidal grid line 
! (which is a niveau line in psi), the code sets up two curves that are not be 
! crossed (basically to restrict the region in which the poloidal grid line can
! exist).

!..Definition de la pemiere courbe a ne pas traverser.
!..Define the first limiting line

         DO 14 i=1, nn(ianc)
            xcrb(i,1)=xn(i,ianc)
            ycrb(i,1)=yn(i,ianc)
   14    CONTINUE

         npcrb(1)=nn(ianc)
         npcrb(2)=0

!..Recherche du deuxieme point.
!..Looking for the second (end) point of this poloidal grid line.
!..Do this by tracing the level line for the current psi value until
!..it hits a structure or limiting curve. 
         dir=0
!..Compute the level line in direction 0 and figure out what happened later
         CALL CRBNIV(ii,jj,nn(inouv),dir,nx,ny,x,y,psi, & 
              &            valfct,xn(1,inouv),yn(1,inouv), & 
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
!  From the first computation of the level line, now figure out
!  what the proper direction is.

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

            IF (chgdir(xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc))) THEN

               nn(inouv)=1
               dir=MOD(dir+1,4) + 1
               ii=ii - MOD(dir-2,2)
               jj=jj - MOD(dir-3,2)

            ENDIF

         ELSE

            if ( extended_grid_limiter ) then 

               ! Special treatment for limiter cases with extended grids. The limiter structure is just a line
               ! (i.e. infinitesimal thin). Do not do any of the tests to check whether we are on the "inside" of it,
               ! but just turn around.

               IF (chgdir(xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc))) then
                  nn(inouv)=1
                  dir=MOD(dir+1,4) + 1
                  ii=ii - MOD(dir-2,2)
                  jj=jj - MOD(dir-3,2)
               end IF

            else

               ! Check whether we are going in the same direction as the previous poloidal grid line.
               ! If not, check whether we are inside a closed structure, or on the internal side
               ! of an open structure, or whether we just crossed a structure element of the target plate.
               ! If any of these is the case, reverse the direction an try again.
               ! Otherwise, there's a serious problem.

               IF (chgdir(xn(1,inouv),yn(1,inouv),xn(1,ianc),yn(1,ianc))) THEN

                  IF ((inStruct(xn(2,inouv),yn(2,inouv),xstruc(1,plaque), & 
                       &   ystruc(1,plaque),npstru(plaque))) & 
                       & .OR. (cross(ind,xn(1,inouv),yn(1,inouv), & 
                       &   xstruc(1,plaque),ystruc(1,plaque), npstru(plaque)))&
                       & .or. &
                       & .not. onInternalSideOfStructure( xn(2,inouv), yn(2,inouv), struct, plaque )) THEN

                     nn(inouv)=1
                     dir=MOD(dir+1,4) + 1
                     ii=ii - MOD(dir-2,2)
                     jj=jj - MOD(dir-3,2)

                  ELSE

                     ! We end up 

                     PRINT *,'probleme de marche 1 dans mailrg'
                     write (*,*) "Starting point for search: ", xn(1,inouv), yn(1,inouv)
                     call pltend
                     STOP

                  ENDIF

               ELSE

                  IF ((inStruct(xn(2,inouv),yn(2,inouv),xstruc(1,plaque), & 
                       & ystruc(1,plaque),npstru(plaque))) & 
                       & .OR. (cross(ind,xn(1,inouv),yn(1,inouv), & 
                       & xstruc(1,plaque),ystruc(1,plaque), & 
                       & npstru(plaque))) &
                       & .or. &
                       & .not. onInternalSideOfStructure( xn(2,inouv), yn(2,inouv), struct, plaque )&
                       &) THEN

                     PRINT *,'probleme de marche 2 dans mailrg'
                     write (*,*) "Specific reason(2): "

                     if ( inStruct(xn(2,inouv),yn(2,inouv),xstruc(1,plaque), ystruc(1,plaque),npstru(plaque)) ) then
                        write (*,*) "inStruct"
                     end if
                     if ( cross(ind,xn(1,inouv),yn(1,inouv), xstruc(1,plaque),ystruc(1,plaque), npstru(plaque)) ) then
                        write (*,*) "cross"
                     end if
                     if ( .not. onInternalSideOfStructure( xn(2,inouv), yn(2,inouv), struct, plaque ) ) then
                        write (*,*) "not onInternalSideOfStructure"
                     end if

                     call pltend
                     STOP

                  ENDIF
               ENDIF

            end if

         ENDIF

!..Pour les points successifs, on poursuit jusqu'a ce qu'on frappe une
!  structure.
!..Now we have the right direction, so compute the level line again.
         CALL CRBNIV(ii,jj,nn(inouv),dir,nx,ny,x,y,psi, & 
     &            valfct,xn(1,inouv),yn(1,inouv), & 
     &            nstruc,npstru,xstruc,ystruc,indstr,xcrb,ycrb,npcrb,1, & 
     &            plaque,x2,y2)

         end if

         ! FIXME: broken for disconnected double null case?
!!$!.. Calcul de grand psi
!!$         diag%gdpsi(ir,ireg)= (valfct-fctxo)/(fctini-fctxo)
!!$         diag%racpsi(ir,ireg)= sqrt(diag%gdpsi(ir,ireg))
!!$
!!$         if (solregion) then
!!$                 x1 = diag%gdr(ir-1,ireg)
!!$                 y1=ypto
!!$                 sensspe=1 ! drctio(xtt,ytt,ntt,x1,y1,'d')
!!$                 CALL SAUTE(xtt,ytt,ntt,x1,y1,fctanc,x23,y23,fctnew,sensspe, & 
!!$                      & repart,nx,ny,x,y,a00,a10,a01,a11,nxmax,nymax)
!!$
!!$                 !..Calcul des longueurs
!!$                 
!!$                 diag%gdr(ir,ireg)=x23
!!$                 diag%r(ir,ireg)=x23-xpto
!!$                 diag%ra(ir,ireg)=diag%r(ir,ireg)-diag%a(ireg)
!!$                 diag%rho(ir,ireg)=diag%r(ir,ireg)/diag%a(ireg)
!!$         endif

!..Definition de xn2 et yn2 pour le bloc common comort.
! TODO: remove this
         xn2(1:nn(inouv)) = xn(1:nn(inouv), inouv)
         yn2(1:nn(inouv)) = yn(1:nn(inouv), inouv)

         npni2=nn(inouv)

!..On definit maintenant les points de maille de la nouvelle ligne 
!  de niveau a partir de ceux de la precedente.

         dernie=0.0
         !ll=long(xn2,yn2,npni2)
         ll=long(xn(1:nn(inouv),inouv),yn(1:nn(inouv),inouv),nn(inouv))
!---
         if(nrelax.ge.0) then
           d1=0.
           ipol1=2
           ipoln=nppol-1

!  1.   on dispose d'abord les points proportionellement a ceux de la
!       ligne precedente
           l1(1)=zero
           l1(nppol)=ll
           l0(1)=zero
           if(ir.eq.2) l0(nppol)=ll1
           do ipol=ipol1,ipoln
             if(ir.eq.2) then
                 d1=ruban(xn(1:nn(ianc),ianc),yn(1:nn(ianc),ianc),nn(ianc),&
                      & mailx(ipol,ir-1), maily(ipol,ir-1),l0(ipol-1))
                 l0(ipol)=d1
             end if
             l1(ipol)=(l0(ipol)/l0(nppol))*ll
             if (l1(ipol) > ll) then
                 write (*,*) "mailrg: exceeding length of niveau line", l1(ipol)
                 stop
                 !l1(ipol)=ll                 
             end if

             CALL COORD(xn(1:nn(inouv),inouv),yn(1:nn(inouv),inouv),nn(inouv),&
                  & l1(ipol),mailx(ipol,ir),maily(ipol,ir))
           enddo

           ! In case of the inner sol region, we have the outer X-point given
           ! on the last radial flux surface. The position of this point is, obviously, fixed.
           ! We have to enforce this explicitly, of else bad things will happen 
           ! (specifically, points can end up "on the wrong side" of the X-point, and the 
           ! optimization algorithm cannot recover from this).

           IF ((ir .EQ. nprad) .AND. (nuldec)) THEN              
               ! get length to X-point on the last surface
               length_xp=ruban(xn(1:nn(inouv),inouv),yn(1:nn(inouv),inouv),nn(inouv),xpx,xpy,0.0d0)
               
               ! scale points to the left and right of the X-point 
               ! left side
               l1(1:xpind-1) = ( l1(1:xpind-1) / l1(xpind-1) ) * 0.95d0 * length_xp
               ! xpoint itself
               l1(xpind) = length_xp
               ! right side
               l1(xpind+1:ipoln) =  l1(xpind+1:ipoln) - l1(xpind+1)
               l1(xpind+1:ipoln) =  ( l1(xpind+1:ipoln) / l1(ipoln) ) * 0.95d0 + 0.05d0
               l1(xpind+1:ipoln) =  ( l1(xpind+1:ipoln) * (ll - length_xp) ) + length_xp
           end IF

           ! Compute the actual position of the grid points
           do ipol=ipol1,ipoln
             CALL COORD(xn(1:nn(inouv),inouv),yn(1:nn(inouv),inouv),nn(inouv),&
                  & l1(ipol),mailx(ipol,ir),maily(ipol,ir))
           enddo
           mailx(nppol,ir)=xn(nn(inouv),inouv)
           maily(nppol,ir)=yn(nn(inouv),inouv)

           if(nrelax.gt.0) then!
!  2.   on initialise la fonction qui doit s'annuler pour une
!       distribution orthogonale
             call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir), & 
                  & maily(1,ir),ort1,nppol,pasmin,garde1,garde2,l0,l1, & 
                  & ortpur,propo,varr)

             ! collect optimization function for all surfaces
             cort(1:nppol,ir) = ort1(1:nppol)
             cortpur(1:nppol,ir) = ortpur(1:nppol)
             cpropo(1:nppol,ir) = propo(1:nppol)
             cvarr(1:nppol,ir) = varr(1:nppol)

!
!  3.   on procede a un premier deplacement des noeuds
             l2(1)=zero
             l2(nppol)=l1(nppol)
             diag%segt(ir,ireg)=l2(nppol)
             do ipol=ipol1,ipoln
                     if(ort1(ipol).gt.zero) then
                             l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol+1)
                     else
                             l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol-1)
                     endif

                     ! keep the outer X-point fixed if required
                     IF ((ir .EQ. nprad) .AND. nuldec .and. (ipol == xpind)) THEN              
                         l2(ipol) = length_xp
                     end IF

                     call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol), & 
                          & mailx(ipol,ir),maily(ipol,ir))                     

                     diag%somort(ir,ireg)= diag%somort(ir,ireg)+(ort1(ipol)/nppol)
                     diag%somortpur(ir,ireg)= diag%somortpur(ir,ireg)+(ortpur(ipol)/nppol)
                     diag%sompropo(ir,ireg)= diag%sompropo(ir,ireg)+(propo(ipol)/nppol)
                     diag%somvarr(ir,ireg)= diag%somvarr(ir,ireg)+(varr(ipol)/nppol)
             enddo
!
!  4.   on relaxe les points de facon iterative pour realiser la
!       meilleure orthogonalite possible
             do i=1,nrelax
                     call csioSetRelax( i )

                     call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir), & 
                          & maily(1,ir),ort2,nppol,pasmin,garde1,garde2,l0,l2, & 
                          & ortpur,propo,varr)

                     ! collect optimization function for all surfaces
                     cort(1:nppol,ir) = ort2(1:nppol)
                     cortpur(1:nppol,ir) = ortpur(1:nppol)
                     cpropo(1:nppol,ir) = propo(1:nppol)
                     cvarr(1:nppol,ir) = varr(1:nppol)

                     ortmax=zero
                     do ipol=ipol1,ipoln
                             if(abs(ort2(ipol)).gt.rlcept) then
                                     del=-ort2(ipol)*(l2(ipol)-l1(ipol)) & 
                                          & /(ort2(ipol)-ort1(ipol))
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
                                     ! keep the outer X-point fixed if required
                                     IF ((ir .EQ. nprad) .AND. nuldec .and. (ipol == xpind)) THEN              
                                         l2(ipol) = length_xp
                                     end IF                                     
                                     call coord(xn(1,inouv),yn(1,inouv),nn(inouv), & 
                                          & l2(ipol),mailx(ipol,ir),maily(ipol,ir))
                             endif
                             ortmax=max(ortmax,abs(ort2(ipol)))
                     enddo

#ifdef USE_SILO
                     if (DEBUGFILES_MAILRG) then
                     ! TODO: maybe move this into a subroutine in CarreSiloIO
                     ! write out current grid status

                     ! entire grid created so far
                     call csioOpenFile()
                     call siloWriteQuadGrid( csioDbfile, "region", &
                          & nppol, ir, &
                          & mailx(1:nppol, 1:ir), maily(1:nppol, 1:ir) )

                     call siloWriteQuadData( csioDbfile,  "region", "cort", &
                          & cort(1:nppol, 1:ir), DB_NODECENT )
                     call siloWriteQuadData( csioDbfile,  "region", "cortpur", &
                          & cortpur(1:nppol, 1:ir), DB_NODECENT )
                     call siloWriteQuadData( csioDbfile,  "region", "cpropo", &
                          & cpropo(1:nppol, 1:ir), DB_NODECENT )
                     call siloWriteQuadData( csioDbfile,  "region", "cvarr", &
                          & cvarr(1:nppol, 1:ir), DB_NODECENT )

                     ! only current flux surface
                     call siloWriteLineSegmentGridFromPoints( csioDbfile, "currentsurface", mailx(1:nppol,ir), maily(1:nppol,ir) )
                     call siloWriteUMData( csioDbfile,  "currentsurface", "ort", &
                          & siloExpandSegmentData( ort2(1:nppol) ), DB_NODECENT )
                     call siloWriteUMData( csioDbfile,  "currentsurface", "ortpur", &
                          & siloExpandSegmentData( ortpur(1:nppol) ), DB_NODECENT )
                     call siloWriteUMData( csioDbfile,  "currentsurface", "propo", &
                          & siloExpandSegmentData( propo(1:nppol) ), DB_NODECENT )
                     call siloWriteUMData( csioDbfile,  "currentsurface", "varr", &
                          & siloExpandSegmentData( varr(1:nppol) ), DB_NODECENT )
                     end if
#endif

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
              diag%somortp(ir,ireg)= diag%somortp(ir,ireg)+ (ort2(ipol)/nppol)
              diag%somortpurp(ir,ireg) = diag%somortpurp(ir,ireg)+(ortpur(ipol)/nppol)
              diag%sompropop(ir,ireg) = diag%sompropop(ir,ireg)+(propo(ipol)/nppol)
              diag%somvarrp(ir,ireg) = diag%somvarrp(ir,ireg)+(varr(ipol)/nppol)
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
              diag%gdsomortp(ireg)=diag%gdsomortp(ireg)+(diag%somortp(ir,ireg)/(nprad-1))
              diag%gdsomortpurp(ireg)=diag%gdsomortpurp(ireg)+(diag%somortpurp(ir,ireg)/(nprad-1))
              diag%gdsompropop(ireg)=diag%gdsompropop(ireg)+(diag%sompropop(ir,ireg)/(nprad-1))
              diag%gdsomvarrp(ireg)=diag%gdsomvarrp(ireg)+(diag%somvarrp(ir,ireg)/(nprad-1))
      enddo

      RETURN
      END
