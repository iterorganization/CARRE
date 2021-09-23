      SUBROUTINE MARCHE(x0,y0,plaque,fraplq,sens,nivox,nivoy,nvotot, &
     &        nbcrb,xt,yt,nt,stp0,stpmin,nx,ny,x,y,psi, &
     &        nstruc,npstru,xstruc,ystruc,a00,a10, &
     &        a01,a11,indlim)

!======================================================================
!..  Cette sous-routine parametrise des lignes de niveau jusqu'a
!  ce qu'on obtienne la derniere courbe de niveau qui va d'un deflectur
!  a un autre sans rencontrer de structure.
!
!dps - Translation:
! This subroutine parameterises the level lines until one obtains a
! final level line which goes from one target to another without
! hitting a structure.
!======================================================================
      use carre_niveau
      use Logging
      use Helper
      use carre_target

      IMPLICIT NONE

#include <CARREDIM.F>

!  arguments
      INTEGER nx,ny,nstruc,npstru(strumx),indlim, &
     &        nvotot,plaque,fraplq,sens,nbcrb,nt(2)

      REAL(rKind) :: x(nxmax),y(nymax),psi(nxmax,nymax),xstruc(npstmx,strumx), &
     &     ystruc(npstmx,strumx),x0,y0,stp0,stpmin,nivox(npnimx), &
     &     nivoy(npnimx),a00(nxmax,nymax,3),a10(nxmax,nymax,3), &
     &     a01(nxmax,nymax,3),a11(nxmax,nymax,3), &
     &     xt(npnimx,2),yt(npnimx,2)

!  variables locales
      INTEGER ii,jj,dir,k,j,repart,ind1,ind2,ind3,indstr
      REAL(rKind) :: const1
      PARAMETER (const1=1.E-08)
      REAL(rKind) :: pas,x1,y1,x2,y2,psi1,psi2,pastmp,ecart1,ecart2, &
     &       niv1x(npnimx),niv1y(npnimx)
      LOGICAL echec,coin,ouvert, crbnivFailed

!  procedures
      INTEGER ifind
      LOGICAL chgdir, cross
      INTRINSIC MOD,SQRT
      EXTERNAL chgdir,cross,ifind,saute

!=========================
!.. plaque: indice de la structure (plaque) sur laquelle on marche.
!.. fraplq: indice de la structure (plaque) que la ligne de niveau doit
!           frapper.
!.. sens: sens de deplacement sur la plaque
!.. x0,y0: point de depart.
!.. nivox,nivoy: tableaux des coordonnees x et y des points parametrises
!                de la derniere ligne de niveau.
!.. nvotot: nombre de points de cette courbe.
!.. knivtm: nombre de points de cette courbe.
!.. x2,y2: point sur la structure ou debute la nouvelle ligne de niveau.
!.. x1,y1: point de la ligne precedente.
!.. psi1,ps2: valeur de la fonction aux points 1 et 2.
!.. ind1,ind2: indices de segments des points 1 et 2.
!.. ind3: indice d'un point de structure, coin.
!.. pas: valeur du pas auquel on marche.
!.. pastmp: valeur temporaire du pas.
!.. ii,jj: indice d'identification d'un carre.
!.. dir: direction de recherche.
!.. k  : indice du point parametrise de chaque courbe.
!.. indlim: indice de la derniere structure frappee autre que la
!           structure designee fraplq.
!.. indstr: indice de la derniere structure frappee.
!.. repart: variable necessaire pour signifier a "SAUTE" que l'on
!           travaille en distance absolue.
!.. ecart1,ecart2: distance entre un point et chaque extremitee du
!                  segment sur lequel il se trouve.
!.. rien: logique, si oui ou non on n'a pas encore rencontre une
!         structure differente de fraplq.
!.. echec: logique, si on doit considerer le debut de la courbe comme
!          un echec ou non.
!.. coin: logique, si on est a un point de structure ou non.
!.. ouvert: logique=.t. si la structure sur laquelle on marche est
!            ouverte.
!
!dps Translation:
!.. plaque: index of the structure (plate) on which one goes.
!.. fraplq: index of the structure (plate) that the level line must strike.
!.. sens: direction of movement along the target - 1=the same as the
!           target points, -1=opposite.
!.. x0,y0: starting point.
!.. nivox,nivoy: tables of the coordinates X and y of the points parameterising
!                the final level line.
!.. nvotot: number of points on this curve.
!.. knivtm: number of points on this curve.
!.. x2,y2: point on the structure or at the start of the new level line.
!.. x1,y1: point from the preceding line.
!.. psi1,ps2: value of the psi function at points 1 and 2.
!.. ind1,ind2: indices of the segments at points 1 et 2.
!.. ind3: index of a point on the structure, corner.
!.. pas: value of the step we are going to.
!.. pastmp: temporary value of the step.
!.. ii,jj: cell index.
!.. dir: direction of the search.
!.. k  : index of the point parameterising each curve.
!.. indlim: index of the final structure hit other than the designated
!           fraplq.
!.. indstr: index of the final structure hit.
!.. repart: specifies whether "SAUTE" works with distance along the target
!           (repart=1) or the new psi value (repart=2).
!.. ecart1,ecart2: distance between a point and the boundary segment on
!                  which it is.
!.. rien: logical variable indicating whether or not a structure other than
!         fraplq has been hit.
!.. echec: logical variable indicating whether or not a curve fails to
!          start.
!.. coin: logical variable indicating whether or not one has a point on
!          on a structure.
!.. ouvert: logical variable is true if the structure being followed is
!           open.
!=========================

!..Initialisation

      x2 = x0
      y2 = y0

      ouvert=xstruc(1,plaque).ne.xstruc(npstru(plaque),plaque) &
     &  .or. ystruc(1,plaque).ne.ystruc(npstru(plaque),plaque)

      !..Calcul de la valeur de la fonction au premier point / Calculate psi value at first point.

      ii = ifind(x2,x,nx,1)
      jj = ifind(y2,y,ny,1)

      psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + &
     &       a11(ii,jj,1)*x2*y2

      ind2 = indsgm(xstruc(1,plaque),ystruc(1,plaque),npstru(plaque), &
     &              x2,y2)

!..Initialisation.

      pas = stp0
      repart = 1
      pastmp = pas

      DO j=1, nt(nbcrb)
         nivox(j) = xt(j,nbcrb)
         nivoy(j) = yt(j,nbcrb)
     end do

      nvotot = nt(nbcrb)

   10 CONTINUE

      call logmsg(LOGDEBUGBULK, "marche: (x,y)=("//real2str(x2)//", "//real2str(y2)//"), pas="//real2str(pas))

      IF (pastmp .LT. 0.5*stpmin) then
         write (*,*) 'marche: stopping level line search because minimum stepping delta reached'
         RETURN
      end IF

      x1 = x2
      y1 = y2
      psi1 = psi2
      ind1 = ind2
      coin = .FALSE.

!..On recherche le prochain point / Look for the next point.

      CALL SAUTE(xstruc(1,plaque),ystruc(1,plaque),npstru(plaque), &
           & x1,y1,psi1,x2,y2,pas,sens,repart,nx,ny,x,y, &
           & a00,a10,a01,a11,nxmax,nymax)

      ! TODO: check that we are inside the given psi range

      !..On s'assure que l'on ne depasse pas la maille / Make sure we did not go past the end of the mesh.

      IF ((x2 .LT. x(1)) .OR. (x2 .GT. x(nx)) &
           & .OR. (y2 .LT. y(1)) .OR.(y2 .GT. y(ny))) THEN

         pastmp = pastmp/2.
         pas = pastmp
         x2 = x1
         y2 = y1

         psi2 = psi1
         ind2 = ind1
         GO TO 10

      ENDIF

      ! Indice du segment de structure sur lequel se trouve x2,y2 / Index of the segment of the structure containing x2, y2.

      ind2 = indsgm(xstruc(1,plaque),ystruc(1,plaque),npstru(plaque),x2,y2)

      ! If we stepped onto the next structure segment,
      ! set the current stepping point to the node connecting
      ! the two segments.
      IF (ind1 .NE. ind2) THEN

         COIN = .TRUE.

         IF (sens .EQ. 1) THEN
            ind3 = ind2
         ELSE
            ind3 = ind1
         ENDIF

         x2 = xstruc(ind3,plaque)
         y2 = ystruc(ind3,plaque)

         ! FIXME: this is pretty broken. This step modification
         ! can send marche into an infinite loop. Disabled for the moment, seems to
         ! work fine without it

         !pastmp = pas
         !pas = SQRT((x1-x2)**2 + (y1-y2)**2)

      ENDIF

      ! Calcul de la fonction pour le point 2 / Calculate the psi function at point 2.

      ii = ifind(x2,x,nx,1)
      jj = ifind(y2,y,ny,1)

      psi2 = a00(ii,jj,1) + a10(ii,jj,1)*x2 + a01(ii,jj,1)*y2 + a11(ii,jj,1)*x2*y2

      !..Definition du premier point du vecteur de niveau / Define the first point of the level vector.

      niv1x(1) = x2
      niv1y(1) = y2

      !..Recheche du deuxieme point sur le perimetre du carre ii,jj.
      ! Search for the second point on the perimeter of cross section ii,jj.

      ecart1 = SQRT((x2-xstruc(ind2,plaque))**2 + (y2-ystruc(ind2,plaque))**2)
      ecart2 = SQRT((x2-xstruc(ind2+1,plaque))**2 + (y2-ystruc(ind2+1,plaque))**2)

      echec = .FALSE.
      dir = 0
      k = 1
      CALL CRBNIV(ii,jj,k,dir,nx,ny,x,y,psi,psi2, &
     &            niv1x,niv1y, &
     &            nstruc,npstru,xstruc,ystruc, &
     &            indstr,xt,yt,nt,nbcrb,plaque,x2,y2)

!..Si on est rendu a l'extremite du segment, on verifie si ce point est
!  a l'interieur de la structure ou si le sens est change. Si on est pas
!  a l'extremite, on verifie si le point est a l'interieur ou si on a
!  traverse un autre segment de la structure. Pour chaque fois, si c'est
!  le cas, on doit inverser les directions, revenir d'un carre et on
!  oublie le point que l'on vient de prendre. Sinon, on continue en
!  conservant le point obtenu, la direction et les coordonnees du carre.
!dps
!  If one has reached the end of the segment, check to see if the point is
!  inside the structure or if the direction has changed.  If not the latter,
!  verify that the point is in the interior or if there is a cross-piece
!  on another segment of the structure.  If this is the case, the direction
!  must be reversed, to return from a cross section and forget the point
!  just taken.  If not, one continues, preserving the point just obtained,
!  the direction, and the coordinate cross section.

      IF ((ABS(x2-x(nx)).LT.const1) .OR. (ABS(y2-y(ny)).LT.const1) &
     &  .OR. (ABS(x2-x(1)).LT.const1) .OR. (ABS(y2-y(1)).LT.const1)) &
     &                                                         THEN

         IF (((dir .EQ. 1) .AND. (ii .EQ. nx)) &
     &       .OR. ((dir .EQ. 2) .AND. (jj .EQ. ny)) &
     &       .OR. ((dir .EQ. 3) .AND. (ii .EQ. 1)) &
     &       .OR. ((dir .EQ. 4) .AND. (jj .EQ. 1))) THEN

            k = 1
            dir = MOD(dir+1,4) + 1
            ii = ii - MOD(dir-2,2)
            jj = jj - MOD(dir-3,2)

            CALL CRBNIV(ii,jj,k,dir,nx,ny,x,y,psi,psi2, &
     &             niv1x,niv1y,nstruc,npstru, &
     &             xstruc,ystruc,indstr,xt,yt,nt,nbcrb,plaque,x2,y2)

            if(indstr.ne.0 .and. indstr.ne.fraplq) indlim=indstr

            IF (cross(ind2,niv1x,niv1y,xstruc(1,plaque), &
     &                         ystruc(1,plaque),npstru(plaque)))   THEN

               echec = .TRUE.

            ENDIF

         ENDIF

      ELSE IF ((ecart1 .LT. const1) .OR. (ecart2 .LT. const1)) THEN

         IF (chgdir(niv1x,niv1y,nivox,nivoy))     THEN

            k = 1
            dir = MOD(dir+1,4) + 1
            ii = ii - MOD(dir-2,2)
            jj = jj - MOD(dir-3,2)

         ENDIF

      ELSE

         IF (chgdir(niv1x,niv1y,nivox,nivoy))     THEN

            IF ((inStruct(niv1x(2),niv1y(2),xstruc(1,plaque), &
     &                         ystruc(1,plaque),npstru(plaque))) &
     &         .OR. (cross(ind2,niv1x,niv1y,xstruc(1,plaque), &
     &                         ystruc(1,plaque),npstru(plaque))) &
     &         .OR. ouvert)   THEN

               k = 1
               dir = MOD(dir+1,4) + 1
               ii = ii - MOD(dir-2,2)
               jj = jj - MOD(dir-3,2)

            ELSE
               echec = .TRUE.
            ENDIF

         ELSE

           if(.not. ouvert) then
            IF ((inStruct(niv1x(2),niv1y(2),xstruc(1,plaque), &
     &                           ystruc(1,plaque),npstru(plaque))) &
     &        .OR. (cross(ind2,niv1x,niv1y,xstruc(1,plaque), &
     &                          ystruc(1,plaque),npstru(plaque)))) THEN

               echec = .TRUE.
            ENDIF
           endif
         ENDIF
      ENDIF

      !..Pour les points successifs, on poursuit jusqu'a ce qu'on frappe une
      !  structure.
      !  For subsequent points, continue until one strikes a structure.

      IF (.NOT.(echec)) THEN

         CALL CRBNIV(ii,jj,k,dir,nx,ny,x,y,psi,psi2, &
              &          niv1x,niv1y,nstruc,npstru,xstruc, &
              &          ystruc,indstr,xt,yt,nt,nbcrb,plaque,x2,y2, &
              &          allowFail = .true., failed = crbnivFailed)

         if (crbnivFailed) then
            ! finding the level line failed, try error handling
            echec = .true.
         else
            ! ...everything ok, continue normally
            if(indstr.ne.0 .and. indstr.ne.fraplq) indlim=indstr
         end if
      ENDIF

!..Si on a frappe une plaque, on verifie si on est rendu a l'extremite
!  de notre plaque de depart, sinon on avance d'un pas et on recommence.
!  If we do strike a plate, check to see if we have returned to the end of
!  our starting plate.  If not, advance one step and start over.

      IF ((indstr .EQ. fraplq) .AND. (.NOT.(echec))) THEN

          DO j=1, k
              nivox(j) = niv1x(j)
              nivoy(j) = niv1y(j)
          end do

          nvotot = k

          IF (pastmp .LT. stp0) THEN
              pastmp = pastmp/2.
              pas = pastmp
          ELSE
              pas = pastmp
          ENDIF

      ELSE

         IF (coin) THEN
            pastmp = pas
         ENDIF

         pastmp = pastmp/2.
         pas = pastmp

         x2 = x1
         y2 = y1
         psi2 = psi1
         ind2 = ind1

      ENDIF

      GO TO 10

      END
