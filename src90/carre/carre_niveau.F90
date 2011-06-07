module carre_niveau

  implicit none

#include <CARREDIM.F>

  private

  public crbniv

contains


  !*** This sub-routine calculates the positions of the successive points
  !*** on a specified level line. It stops when the line crosses a
  !*** structure or when it reaches the limits of the equilibrium.
  !*** The calculated level line should not cross any of pre-defined
  !*** limiting curves.

  !> The original version of this routine is located in crbniv.F(90).
  !> This version introduces optional arguments and additional features.

  ! Optional arguments
  ! Optional groups: 
  ! -starting point test (avoid adding starting point to level line): x0, y0
  ! -structure intersection: xstruc, ystruc, nstruc, npstru, plaque, indstr
  ! -limiting line intersection: xt, yt, nt, nbcrb
  ! -prescribed end point: stop curve when reaching the given point xEnd, yEnd

!!$      SUBROUTINE CRBNIV(ii,jj,k,idir,nxmax,nymax,nx,ny,x,y,f,niv, & 
!!$     &        crbx,crby,npnimx,strumx,npstmx,nstruc,npstru, & 
!!$     &        xstruc,ystruc,indstr,xt,yt,nt,nbcrb,plaque,x0,y0)


  SUBROUTINE CRBNIV( &
       & ii,jj,k,idir,& 
       & nx,ny,x,y,f,niv, & 
       & crbx,crby,& 
       & nstruc,npstru, & 
       & xstruc,ystruc,indstr,xt,yt,nt,nbcrb,plaque, &
       & x0,y0, &
       & xEnd, yEnd)

    IMPLICIT NONE


    !  arguments
    INTEGER, intent(in) :: nx,ny 
    INTEGER, intent(inout) :: ii, jj, idir, k
    !INTEGER, intent(out) ::

    REAL*8, intent(in) :: x(nxmax),y(nymax),f(nxmax,nymax),niv
    REAL*8, intent(inout) :: crbx(npnimx), crby(npnimx)


    INTEGER, intent(in), optional :: plaque, nstruc,npstru(:),nbcrb
    INTEGER, intent(out), optional :: indstr, nt(2)

    REAL*8, intent(in), optional :: xstruc(npstmx,strumx), ystruc(npstmx,strumx), &
         & xt(npnimx,2),yt(npnimx,2), x0,y0
    REAL*8, intent(in), optional :: xEnd, yEnd

    !  variables locales
    LOGICAL trvers2
    INTEGER i,j,dir,ist,jst,igrace,ngrace, iCrb
    REAL*8 determ,mult1,mult2,dist,proxim
    parameter(proxim=1.e-8,ngrace=3)
    logical :: stepDone

    !  procedures
    LOGICAL milieu, trvers
    REAL*8 interp
    INTRINSIC ABS,SQRT
    EXTERNAL interp,milieu,trvers

    !=========================
    !.. f    :<=> psi.
    !.. ii,jj: x and y indices of the initial reference cell.
    !.. i,j  : x and y indices of the reference cell.
    !.. k    : index of the point to be calculated.
    !.. idir : initial direction.
    !.. dir  : direction of search     2
    !                                3   1
    !                                  4
    !.. indstr: index of intercepting structure, =0 means no intersection.
    !.. niv: the level for which the points should be found
    !.. crbx,crby: x and y coordinates of the points on the level line
    !.. xt,yt: coordinates along the limiting curves
    !..        (point index, curve index)
    !.. nt: number of points on the limiting curves
    !.. nbcrb: number of the limiting curves. Can be zero, in this case no limiting curves are used.
    !.. plaque: index of the target where the line stops.
    !.. ist,jst: indices for the structures.
    !.. determ: matrix determinant for the 2 equations.
    !.. mult1: facteur multiplicatif du segment de courbe.
    !.. mult2: facteur multiplicatif du segment de structure.
    !.. x0 y0: coordinates of the starting point
    !=========================

    !..Copie des arguments en variables locales

    dir = ABS(idir)
    i = ii
    j = jj

    !..Initialisation

    indstr = 0

    !.. 1   Look for the next point.

    !..Inside a cell with the corners are            3.  .4
    !                                                1.  .2
    !  en connaissant la direction initiale, on regarde par quel cote
    !  ressort la ligne de niveau. On ajuste ainsi la rangee, la colonne et
    !  la direction a chaque fois. On verifie egalement que le nouveau
    !  segment forme des 2 derniers points ne traverse pas les courbes t.

    !  by knowing the initial direction, one looks by which side exits the level line. One
    !  adjusts the row, the column and the direction each time. One verifies also that the
    !  new segment formed by the last 2 points does not cross the curves T

    ! Build the level line segment by segment
    do 

       ! We want to do a step along the niveau line
       stepDone = .false.

       !..If the level line passes between 2 and 4,
       IF (.not. stepDone .and. (dir.NE.3)) THEN
          IF (milieu(f(i+1,j), f(i+1,j+1), niv)) THEN
             k=k+1
             crbx(k) = x(i+1)
             crby(k) =interp(y(j),y(j+1),f(i+1,j),f(i+1,j+1),niv)

             trvers2=.false.
             do iCrb = 1, nbcrb             
                trvers2 = trvers2 .or. &
                     & trvers(crbx(k-1),crby(k-1),crbx(k),crby(k),  &
                     &  xt(1,iCrb),yt(1,iCrb),nt(iCrb))
             end do

             IF (trvers2) THEN
                k=k-1
             ELSE
                i=i+1
                dir=1
                stepDone = .true.
             ENDIF

          ENDIF
       ENDIF

       !..If the level line passes between 3 and 4.

       IF (.not. stepDone .and. (dir.NE.4)) THEN
          IF (milieu(f(i,j+1), f(i+1,j+1), niv)) THEN
             k=k+1
             crbx(k) =interp(x(i),x(i+1),f(i,j+1),f(i+1,j+1),niv)
             crby(k) = y(j+1)

             trvers2=.false.
             do iCrb = 1, nbcrb             
                trvers2 = trvers2 .or. &
                     & trvers(crbx(k-1),crby(k-1),crbx(k),crby(k),  &
                     &  xt(1,iCrb),yt(1,iCrb),nt(iCrb))
             end do

             IF (trvers2) THEN
                ! If intersected a limiting curve, undo the last point
                ! and try next face
                k=k-1
             ELSE
                ! Didn't intersect a limiting curve.
                ! Move to next cell, update search direction, jump down to further processing.
                j=j+1
                dir=2
                stepDone = .true.
             ENDIF

          ENDIF
       ENDIF

       !..If the level line passes between 1 and 3.

       IF (.not. stepDone .and. (dir.NE.1)) THEN
          IF (milieu(f(i,j), f(i,j+1), niv)) THEN
             k=k+1
             crbx(k) = x(i)
             crby(k) =interp(y(j),y(j+1),f(i,j),f(i,j+1),niv)

             trvers2=.false.
             do iCrb = 1, nbcrb             
                trvers2 = trvers2 .or. &
                     & trvers(crbx(k-1),crby(k-1),crbx(k),crby(k),  &
                     &  xt(1,iCrb),yt(1,iCrb),nt(iCrb))
             end do

             IF (trvers2) THEN
                k=k-1
             ELSE
                i=i-1
                dir=3
                stepDone = .true.
             ENDIF
          ENDIF
       ENDIF

       !..If the level line passes between 1 et 2.

       IF (.not. stepDone .and. (dir.NE.2)) THEN
          IF (milieu(f(i,j), f(i+1,j), niv)) THEN
             k=k+1
             crbx(k) =interp(x(i),x(i+1),f(i,j),f(i+1,j),niv)
             crby(k) = y(j)

             trvers2=.false.
             do iCrb = 1, nbcrb             
                trvers2 = trvers2 .or. &
                     & trvers(crbx(k-1),crby(k-1),crbx(k),crby(k),  &
                     &  xt(1,iCrb),yt(1,iCrb),nt(iCrb))
             end do

             IF (trvers2) THEN
                k=k-1
                write(6,*)'Erreur dans crbniv, travers.', & 
                     &            ' Pas cense traverser de courbe'
                call pltend
                STOP
             ELSE
                j=j-1
                dir=4
                stepDone = .true.
             ENDIF
          ENDIF
       ENDIF

       if (.not. stepDone) then
          print *,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          print *,'The grid you have specified seems to be too fine in ', & 
               &                                               'radial direction'
          print *,'Try to increase the deltr[1n] values, or'
          print *,'    to reduce the number of the radial grid points, or'
          print *,'    to use equilibrium data with higher resolution'
          print *
          stop 'in crbniv: nothing found'
       end if


       ! we arrive here if a continuation of the level
       ! line that does not intersect one of the limiting curves was found

       !  if the found point coincides with the starting point, it is rejected
       !  and the calculation continues
       dist=sqrt((x0-crbx(k))**2+(y0-crby(k))**2)
       if(dist.lt.proxim) then
          k=k-1
          cycle ! back to beginning of segment loop
       endif

       !..If idir.le.0, then find only one point, change the sign of idir,
       !  and return

       !..Si idir etait inferieur ou egal a 0, alors on ne faisait que chercher
       !  un seul point. On retourne la direction de recherche et les
       !  coordonnees de ce point.

       IF (idir .LE. 0) THEN

          idir = dir
          ii = i
          jj = j

          RETURN

       ENDIF

       !.. 2   Check whether this segment of the level line crosses a structure

       !..Loop over the structures

       DO jst=1, nstruc

          !.. Provided that the number of points in the curve already exceeds 2,
          !   check whether the structure is different from the starting one.

          !..On verifie seulement si la structure est differente de celle de
          !  depart pour un nombre de points superieur a deux.

          IF  ((k .LT. 3) .AND. (plaque .EQ. jst)) then
             cycle
          ENDIF

          DO ist=1, ABS(npstru(jst))-1

             !..Calcul du determinant de la matrice.

             determ = (-(crbx(k) - crbx(k-1))) * & 
                  &               (ystruc(ist+1,jst) - ystruc(ist,jst)) + & 
                  &               (crby(k) - crby(k-1)) * & 
                  &               (xstruc(ist+1,jst) - xstruc(ist,jst))

             !..Si determinant non nul, alors il y a solution.

             IF (determ .NE. 0.) THEN

                !..Facteur multiplicatif du segment de courbe avec la methode de Cramer.

                mult1 = ((-(xstruc(ist,jst)-crbx(k-1))) * & 
                     &                 (ystruc(ist+1,jst)-ystruc(ist,jst)) + & 
                     &                 (ystruc(ist,jst)-crby(k-1)) * & 
                     &                 (xstruc(ist+1,jst)-xstruc(ist,jst)))/determ

                !..mult1 is between 0 and 1 for an intersection

                IF ((mult1.GT.0.).AND.(mult1.LE.1.)) THEN

                   !..Fact. mult. du segment de structure.

                   mult2= ((crbx(k)-crbx(k-1)) * & 
                        &                   (ystruc(ist,jst)-crby(k-1)) - & 
                        &                   (crby(k)-crby(k-1)) * & 
                        &                   (xstruc(ist,jst)-crbx(k-1)))/determ

                   !..Intersection if mult2 between 0 and 1

                   IF ((mult2.GT.0.).AND.(mult2.LT.1.)) THEN

                      !..The point of intersection between the segment of curve and the
                      !  segment of structure becomes the last point of the level line

                      crbx(k) = crbx(k-1) + (crbx(k)-crbx(k-1))*mult1
                      crby(k) = crby(k-1) + (crby(k)-crby(k-1))*mult1
                      indstr = jst

                   ENDIF

                ENDIF

             ENDIF

          end do


          !..Return if the level line was intercepted by a structure.
          IF (indstr .NE. 0) THEN
             RETURN
          ENDIF

       end do ! end loop over structures to check intersection of latest line segment with structures

       !
       !.. 3   Test pour savoir si la ligne de niveau est soit arrive au bord
       !       de la maille, soit revenue a son point de depart.
       !

       if(i.eq.0 .or. i.eq.nx .or. j.eq.0 .or. j.eq.ny) return

       if(k.gt.ngrace) then
          do igrace=1,ngrace
             dist=sqrt((crbx(igrace)-crbx(k))**2+(crby(igrace)-crby(k))**2)
             if(dist.lt.proxim) then
                !www
                if(igrace.gt.2) then
                   write(6,400)(i,crbx(i),crby(i),crbx(k+i-igrace), & 
                        &               crby(k+i-igrace),i=1,igrace)
400                format(i4,1p2e11.3,2x,1p2e11.3)
                endif
                !www
                k=k-igrace+2
                return
             endif
          enddo
       endif
    end do

  END subroutine crbniv


end module carre_niveau
