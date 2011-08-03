module carre_niveau

  use carre_types
  use Helper
  use Logging
#ifdef USE_SILO
  use SiloIO
  use CarreSiloIO
#endif

  implicit none

#include <CARREDIM.F>

  private

  public crbniv, findLevelLineForPoints

  logical, parameter :: DEBUGFILES_CRBNIV = .false.

  integer, save :: CRBNIV_CALL = 0

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
  ! -controlled failure: crbniv can fail to continue a level line, this usually
  !  happens around the X-point. The default behaviour for this case is to stop
  !  the program. If allowFail = .true. is given, in case of failure
  !  the failure status is returned in failed.


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
       & xEnd, yEnd, foundEndPoint, &
       & allowFail, failed)

    IMPLICIT NONE


    !  arguments
    INTEGER, intent(in) :: nx,ny 
    INTEGER, intent(inout) :: ii, jj, idir, k
    !INTEGER, intent(out) ::

    REAL*8, intent(in) :: x(nxmax),y(nymax),f(nxmax,nymax),niv
    REAL*8, intent(inout) :: crbx(npnimx), crby(npnimx)


    ! structure test parameters
    INTEGER, intent(in), optional :: plaque, nstruc,npstru(:)
    INTEGER, intent(out), optional :: indstr
    REAL*8, intent(in), optional :: xstruc(npstmx,strumx), ystruc(npstmx,strumx)

    ! limit curve parameters
    INTEGER, intent(in), optional :: nbcrb
    INTEGER, intent(out), optional ::  nt(2)
    REAL*8, intent(in), optional :: xt(npnimx,2),yt(npnimx,2)

    ! start point
    REAL*8, intent(in), optional :: x0,y0 

    ! end point
    REAL*8, intent(in), optional :: xEnd, yEnd
    logical, intent(out), optional :: foundEndPoint

    ! Fail parameters
    logical, intent(in), optional :: allowFail
    logical, intent(out), optional :: failed

    !  variables locales
    LOGICAL trvers2
    INTEGER i,j,dir,ist,jst,igrace,ngrace, iCrb
    REAL*8 determ,mult1,mult2,dist,proxim
    parameter(proxim=1.e-8,ngrace=3)
    logical :: stepDone
    logical :: testEndPoint, testStartPoint, testStructures, testLimitingCurves
    integer :: iEnd, jEnd
    !  procedures
    LOGICAL milieu, trvers
    REAL*8 interp
    integer ifind
    INTRINSIC ABS,SQRT
    EXTERNAL interp,milieu,trvers,ifind

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

    ! Debug output: set up local filenamespace for crbniv
#ifdef USE_SILO
    if (DEBUGFILES_CRBNIV) then
       call csioSaveCounters()
       CRBNIV_CALL = CRBNIV_CALL + 1
       call csioSetFilenameBase('crbni')
       call csioSetRelax(0)
       call csioSetSurface(CRBNIV_CALL)
       call csioSetRegion(0)
    end if
#endif

    !..Copie des arguments en variables locales

    dir = ABS(idir)
    i = ii
    j = jj

    !..Initialisation

    testStartPoint = present(x0) .and. present(y0)
    testStructures = present(plaque) .and. present(nstruc) .and. present(npstru) &
         & .and. present(indstr) .and. present(xstruc) .and. present(ystruc)
    testLimitingCurves = present(nbcrb) .and. present(nt) .and. present(xt) &
         & .and. present(yt) 
    testEndPoint = present(xEnd) .and. present(yEnd) .and. present(foundEndPoint)

    if ( testEndPoint ) then
       iEnd = ifind(xEnd, x(1:nx), nx, 1)
       jEnd = ifind(yEnd, y(1:ny), ny, 1)
    end if
    if (present(foundEndPoint)) foundEndPoint = .false.

    if (testStructures) then
       indstr = 0
    end if

    ! Failure handling: first assume everything is ok, set failed state later
    if (present(failed)) failed = .false.

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
       
       ! If an end point is given, check whether it is in the current cell.
       ! If yes, close the level line.
       if (testEndPoint) then
          if ( i == iEnd .and. j == jEnd ) then
             k = k + 1
             crbx(k) = xEnd
             crby(k) = yEnd
             foundEndPoint = .true.
#ifdef USE_SILO
             if (DEBUGFILES_CRBNIV) then
                !call csioOpenFile('carreCrbnivEndP')
                call csioOpenFile()
                call siloWriteLineSegmentGridFromPoints( csioDbfile, "brokenline", &
                     & crbx(1:k), crby(1:k) )    
                call csioCloseFile()
                call csioRestoreCounters()
             end if             
#endif
             return
          end if
       end if


       ! We want to do a step along the niveau line
       stepDone = .false.

       !..If the level line passes between 2 and 4,
       IF (.not. stepDone .and. (dir.NE.3)) THEN
          IF (milieu(f(i+1,j), f(i+1,j+1), niv)) THEN
             k=k+1
             crbx(k) = x(i+1)
             crby(k) =interp(y(j),y(j+1),f(i+1,j),f(i+1,j+1),niv)

             trvers2=.false.
             if (testLimitingCurves) then
                do iCrb = 1, nbcrb             
                   trvers2 = trvers2 .or. &
                        & trvers(crbx(k-1),crby(k-1),crbx(k),crby(k),  &
                        &  xt(1,iCrb),yt(1,iCrb),nt(iCrb))
                end do
             end if

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
             if (testLimitingCurves) then
             do iCrb = 1, nbcrb             
                trvers2 = trvers2 .or. &
                     & trvers(crbx(k-1),crby(k-1),crbx(k),crby(k),  &
                     &  xt(1,iCrb),yt(1,iCrb),nt(iCrb))
             end do
             end if

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
             if (testLimitingCurves) then
             do iCrb = 1, nbcrb             
                trvers2 = trvers2 .or. &
                     & trvers(crbx(k-1),crby(k-1),crbx(k),crby(k),  &
                     &  xt(1,iCrb),yt(1,iCrb),nt(iCrb))
             end do
             end if

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
             if (testLimitingCurves) then
             do iCrb = 1, nbcrb             
                trvers2 = trvers2 .or. &
                     & trvers(crbx(k-1),crby(k-1),crbx(k),crby(k),  &
                     &  xt(1,iCrb),yt(1,iCrb),nt(iCrb))
             end do
             end if

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
          
          ! controlled failure handling?
          if (present(allowFail)) then
             if ( allowFail ) then             
                if (present(failed)) failed = .true.
                return
             end if
          end if

          print *,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          print *,'The grid you have specified seems to be too fine in ', & 
               &                                               'radial direction'
          print *,'Try to increase the deltr[1n] values, or'
          print *,'    to reduce the number of the radial grid points, or'
          print *,'    to use equilibrium data with higher resolution'
          print *

          ! debug output
#ifdef USE_SILO
          if (DEBUGFILES_CRBNIV) then
             call csioOpenFile('carreCrbnivStop')
             call csioOpenFile()
             call siloWriteLineSegmentGridFromPoints( csioDbfile, "brokenline", &
                  & crbx(1:k), crby(1:k) )    
             call csioCloseFile()
             call csioRestoreCounters()
          end if
#endif

          stop 'in crbniv: nothing found'
       end if


       ! we arrive here if a continuation of the level
       ! line that does not intersect one of the limiting curves was found

       !  if the found point coincides with the starting point, it is rejected
       !  and the calculation continues
       if (testStartPoint) then
          dist=sqrt((x0-crbx(k))**2+(y0-crby(k))**2)
          if(dist.lt.proxim) then
             k=k-1
             cycle ! back to beginning of segment loop
          endif
       end if

       !..If idir.le.0, then find only one point, change the sign of idir,
       !  and return

       !..Si idir etait inferieur ou egal a 0, alors on ne faisait que chercher
       !  un seul point. On retourne la direction de recherche et les
       !  coordonnees de ce point.

       IF (idir .LE. 0) THEN

          idir = dir
          ii = i
          jj = j

#ifdef USE_SILO
          if (DEBUGFILES_CRBNIV) then
             !call csioOpenFile('carreCrbnivOneP')
             call csioOpenFile()
             call siloWriteLineSegmentGridFromPoints( csioDbfile, "brokenline", &
                  & crbx(1:k), crby(1:k) )    
             call csioCloseFile()
             call csioRestoreCounters()
          end if
#endif
          
          RETURN

       ENDIF

       !.. 2   Check whether this segment of the level line crosses a structure
       if (testStructures) then

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
#ifdef USE_SILO
             if (DEBUGFILES_CRBNIV) then
                !call csioOpenFile('carreCrbnivStru')
                call csioOpenFile()
                call siloWriteLineSegmentGridFromPoints( csioDbfile, "brokenline", &
                     & crbx(1:k), crby(1:k) )    
                call csioCloseFile()
                call csioRestoreCounters()
             end if
#endif
             RETURN
          ENDIF

       end do ! end loop over structures to check intersection of latest line segment with structures
       end if

       !
       !.. 3   Test pour savoir si la ligne de niveau est soit arrive au bord
       !       de la maille, soit revenue a son point de depart.
       !

       if(i.eq.0 .or. i.eq.nx .or. j.eq.0 .or. j.eq.ny) then
#ifdef USE_SILO
          if (DEBUGFILES_CRBNIV) then
             !call csioOpenFile('carreCrbnivOuts')
             call csioOpenFile()
             call siloWriteLineSegmentGridFromPoints( csioDbfile, "brokenline", &
                  & crbx(1:k), crby(1:k) )    
             call csioCloseFile()
             call csioRestoreCounters()
          end if
#endif          
          return
       end if

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


  !> High-level routine to fine a level line connecting two points.
  !> (xFrom, yFrom)->(xTo, yTo). It is assumed that both points
  !> do lie on the same level line. If there are multiple possible 
  !> connection lines between the points, the shortest one is returned.
  subroutine findLevelLineForPoints( equ, &
         & xFrom, yFrom, xTo, yTo, &
         & nivx, nivy, npNiv )
    type(CarreEquilibrium), intent(in) :: equ
    double precision, intent(in) :: xFrom, yFrom, xTo, yTo
    double precision, intent(out) :: nivx(npnimx), nivy(npnimx)
    integer, intent(out) :: npNiv

    ! internal
    integer :: ii, jj, npNivTmp, iDir, iDirStart
    double precision :: valNiv, length, maxLength, lengthFace
    double precision :: nivxTmp(npnimx), nivyTmp(npnimx)
    logical :: foundEndPoint

    ! external
    integer :: ifind
    double precision :: long
    external :: ifind, long
    logical :: foundLevelLine

    ! Find the cell of the starting point    
    ii = ifind(xFrom, equ%x(1:equ%nx), equ%nx, 1)
    jj = ifind(yFrom, equ%y(1:equ%ny), equ%ny, 1)
        
    ! Compute psi value at this point
    valNiv=equ%a00(ii,jj,1) + equ%a10(ii,jj,1)*xFrom + & 
         & equ%a01(ii,jj,1)*yFrom + equ%a11(ii,jj,1)*xFrom*yFrom   

    lengthFace = sqrt( (xTo - xFrom) ** 2 + (yTo - yFrom) ** 2 )

    maxLength = huge(maxLength)
    foundLevelLine = .false.

    ! Now call crbniv in all four directions starting from this point
    do iDirStart = 1, 4

       ! set up start of niveau line
       npNivTmp = 1
       nivxTmp(1) = xFrom
       nivyTmp(1) = yFrom

       iDir = iDirStart ! need to do this because iDir can be changed in crbniv...
       CALL CRBNIV(ii,jj,npNivTmp,iDir,&
            & equ%nx,equ%ny,equ%x,equ%y,equ%psi, & 
            & valNiv,nivxTmp,nivyTmp, &
            & x0=xFrom,y0=yFrom,xEnd=xTo,yEnd=yTo,&
            & foundEndPoint=foundEndPoint)     

       if (foundEndPoint) then
          ! For the resulting level line, compute length
          ! and do bookkeeping to find the shortest
          length = long(nivxTmp(1:npNivTmp), nivyTmp(1:npNivTmp), npNivTmp)
          
          ! Heuristic: if the found level line is too large by an order of magnitude, reject it
          if ( (length/lengthFace) > 10d0 ) then
             call logmsg( LOGKNOWNWARNING, 'findLevelLineForPoints: found level line (length: '&
                 &//real2str(length)//', points: '//int2str(npNivTmp)//')'//&
                 & '- REJECTED because too long compared to face (length: '//real2str(lengthFace)//')' )
             cycle
          end if

          foundLevelLine = .true.
          
          if (length < maxLength) then
             maxLength = length
             npniv = npNivTmp
             nivx = nivxTmp
             nivy = nivyTmp
          end if
       end if

    end do

    if (.not. foundLevelLine) then
       ! This happens if the equilibrium data has issues. 
       ! Usually happens for faces going ino the x-point. Fortunately there the 
       ! level lines are pretty straight anyway, so it's not a problem.
       call logmsg( LOGKNOWNWARNING, 'findLevelLineForPoints: WARNING: unable to find connecting level line.&
            & Substituting direct connection line' )
       npniv = 2
       nivx(1) = xFrom
       nivy(1) = yFrom
       nivx(2) = xTo
       nivy(2) = yTo
   else       
      call logmsg( LOGDEBUG, 'findLevelLineForPoints: distance of points is: '//real2str(lengthFace)//&
           & ', found level line: length='//real2str(maxLength)//&
           & ' with '//int2str(npNiv)//' points' )
   end if

  end subroutine findLevelLineForPoints

end module carre_niveau
