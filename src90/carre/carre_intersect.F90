module carre_intersect

  use carre_types
  use itm_assert
  use Logging

  implicit none

contains

  !> Compute face/structure intersections for all faces in the grid to fill the
  !> grid%faceISec, grid%faceISecPx/y arrays
  subroutine computeFaceStructureIntersections( struct, grid )
    type(CarreStructures), intent(in) :: struct
    type(CarreGrid), intent(inout) :: grid

    ! internal
    integer :: iReg, iPol, iRad, iFace
    double precision :: xx(2), yy(2)

    ! Mark faces intersected by structure elements
    grid%faceISec = .false.

    do iReg = 1, grid%nreg

        do iPol = 1, grid%np1(iReg)
            do iRad = 1, grid%nr(iReg)
                do iFace = 1, 2 ! poloidal, radial

                   ! Skip nonexisting faces
                   select case (iFace)
                   case(FACE_POLOIDAL) ! poloidal
                      if (iPol == grid%np1(iReg)) cycle
                   case(FACE_RADIAL) ! radial
                      if (iRad == grid%nr(iReg)) cycle
                   end select

                    ! fill xx, yy with face start-, endpoint
                    call getFace(iFace)

                    call intersect_all_structures( xx, yy, &
                        & struct, grid%faceISec(iFace, iPol, iRad, iReg), &
                        & ipx = grid%faceISecPx(iFace, iPol, iRad, iReg), &
                        & ipy = grid%faceISecPy(iFace, iPol, iRad, iReg) )                    

                end do
            end do
        end do
    end do

  contains

    subroutine getFace(iFace)
      integer, intent(in) :: iFace

      select case (iFace)
      case(1) ! Poloidal face (iPol, iRad) -> (iPol+1, iRad)
          xx(1) = grid%xmail(iPol, iRad, iReg)
          yy(1) = grid%ymail(iPol, iRad, iReg)
          xx(2) = grid%xmail(iPol + 1, iRad, iReg)
          yy(2) = grid%ymail(iPol + 1, iRad, iReg)              
      case(2) ! Radial face (iPol, iRad) -> (iPol, iRad + 1)
          xx(1) = grid%xmail(iPol, iRad, iReg)
          yy(1) = grid%ymail(iPol, iRad, iReg)
          xx(2) = grid%xmail(iPol, iRad + 1, iReg)
          yy(2) = grid%ymail(iPol, iRad + 1, iReg)
      end select

    end subroutine getFace

  end subroutine computeFaceStructureIntersections


  !> Compute intersection of a line segment curve with the structure elements
  !>
  !> If the optional arguments oldipx, oldipy are not given, the first intersection
  !> found is returned. If the optional point oldipx, oldipy is given, of possible
  !> multiple intersections the one closest to this point is returned.
  subroutine intersect_all_structures(xx, yy, struct, doesIntersect, iStruct, &
      & iSegment, ipx, ipy, oldipx, oldipy)

    REAL*8, intent(in) :: xx(:),yy(:)     
    type(CarreStructures), intent(in) :: struct
    logical, intent(out) :: doesIntersect
    integer, intent(out), optional :: iSegment, iStruct
    REAL*8, intent(out), optional :: ipx, ipy
    REAL*8, intent(in), optional :: oldipx, oldipy

    ! internal      
    integer :: iSeg, is
    integer :: closestISegment, tmpISegment, closestIStruct
    double precision :: closestDist, tmpIpx, tmpIpy, closestIpx, closestIpY
    logical :: tmpDoesIntersect
    

    double precision :: dist
    external :: dist

    closestDist = huge(closestDist)
    closestIStruct = GRID_UNDEFINED
    closestISegment = GRID_UNDEFINED

    ! check for every segment in the line for intersection with every structure
    do iSeg = 1, size(xx) - 1
       do is = 1, struct%rnstruc
          call intersect_structure(xx(iSeg:iSeg+1), yy(iSeg:iSeg+1), &
               & struct%rxstruc(1:abs(struct%rnpstru(is)), is), &
               & struct%rystruc(1:abs(struct%rnpstru(is)), is), &
               & tmpDoesIntersect, tmpISegment, tmpIpx, tmpIpy, &
               & testEndPoints = .true., oldipx=oldipx, oldipy=oldipy)

          if (tmpDoesIntersect) then
              if (present(oldipx)) then
                  if ( dist( tmpIpx, tmpIpy, oldipx, oldipy) < closestDist ) then
                      closestIStruct = is
                      closestISegment = iSeg
                      closestIpx = tmpIpx
                      closestIpy = tmpIpy
                  end if
              else
                  closestIStruct = is
                  closestISegment = iSeg
                  closestIpx = tmpIpx
                  closestIpy = tmpIpy
                  exit
              end if
          end if

       end do       
       
       if (.not. present(oldipx) .and. (closestIStruct /= GRID_UNDEFINED)) then
           ! fast exit if only first found intersection required
           exit
       end if
    end do

    if (closestIStruct == GRID_UNDEFINED) then
        doesIntersect = .false.
        if (present(iStruct)) iStruct = GRID_UNDEFINED
    else
        doesIntersect = .true.
        if (present(iStruct)) iStruct = closestIStruct
        if (present(iSegment)) iSegment = closestISegment
        if (present(ipx)) ipx = closestIpx
        if (present(ipy)) ipy = closestIpy
    end if

  end subroutine intersect_all_structures


  !> Compute intersection of a line segment with one structure
  !> xst,yst: tableaux des coordonnees des points de la structure.
  !> xx,yy: tableaux des coordonnees contenant les 2 points du segment.
  !> doesIntersect: flag indicating whether intersection found
  !> iSegment: index of segment intersected
  !> ipx, ipy: intersection point (if found)
  subroutine intersect_structure(xx,yy,xst,yst,doesIntersect,iSegment,ipx,ipy,&
       & testEndPoints, oldipx, oldipy)

    !  arguments
    REAL*8, intent(in) :: xx(2),yy(2),xst(:),yst(:)

    logical, intent(out) :: doesIntersect
    integer, intent(out), optional :: iSegment
    REAL*8, intent(out), optional :: ipx, ipy
    logical, intent(in), optional :: testEndPoints
    REAL*8, intent(in), optional :: oldipx, oldipy

    !  variables locales
    INTEGER i, j, k
    !.. determ: determinant de la matrice des deux equations.
    !.. mult1: facteur multiplicatif du segment de courbe.
    !.. mult2: facteur multiplicatif du segment de structure.
    REAL*8 mult1,mult2,determ
    double precision :: tmpIpx, tmpIpy, closestIpx, closestIpy, closestDist
    integer :: closestSegment

    double precision :: dist
    external :: dist

    logical :: doesIntersectCheck, tmpDoesIntersect

    closestDist = huge(closestDist)
    closestSegment = GRID_UNDEFINED

    !..Boucle sur la structure.
    DO i=1, size(xst)-1
        call intersection(xx, yy, xst(i:i+1), yst(i:i+1), tmpDoesIntersect, tmpIpx, tmpIpy, testEndPoints)

        if (tmpDoesIntersect) then
            if (present(oldipx)) then
                ! if old intersection point given, do all segments to make sure we ge the closest intersection
                if (dist(tmpIpx, tmpIpy, oldipx, oldipy) < closestDist ) then
                    closestDist = dist(tmpIpx, tmpIpy, oldipx, oldipy)
                    closestIpx = tmpIpx
                    closestIpy = tmpIpy
                    closestSegment = i
                end if
            else
                ! otherwise return first intersection
                closestIpx = tmpIpx
                closestIpy = tmpIpy
                closestSegment = i
                exit
            end if
        end if

    END DO

    if (closestSegment /= GRID_UNDEFINED) then
        doesIntersect = .true.
        if (present(ipx)) ipx = closestIpx
        if (present(ipy)) ipy = closestIpy
        if (present(iSegment)) iSegment = closestSegment
    else
        doesIntersect = .false.
        if (present(iSegment)) iSegment = GRID_UNDEFINED
    end if

  END subroutine intersect_structure


  ! Intersection between two line segments
  subroutine intersection(xx, yy, xst, yst, doesIntersect, ipx, ipy, testEndPoints)

    !  arguments
    REAL*8, intent(in) :: xx(2),yy(2),xst(2),yst(2)

    logical, intent(out) :: doesIntersect
    REAL*8, intent(out), optional :: ipx, ipy
    logical, intent(in), optional :: testEndPoints

    !  variables locales
    INTEGER j, k
    !.. determ: determinant de la matrice des deux equations.
    !.. mult1: facteur multiplicatif du segment de courbe.
    !.. mult2: facteur multiplicatif du segment de structure.
    REAL*8 mult1,mult2,determ

    logical :: doesIntersectCheck

    doesIntersectCheck = segments_intersect( xx(1), yy(1), xx(2), yy(2), &
         & xst(1), yst(1), xst(2), yst(2) )

    if (present(testEndPoints)) then
        if (testEndPoints) then
            do j = 1, 2 ! start point, end point of structure segment
                do k = 1, 2 ! start point, end point of line segment
                    if (pointsIdentical(xx(k), yy(k), xst(j), yst(j), &
                         & absTol=1.0d-6 )) then

                        doesIntersect = .true.
                        if (present(ipx) .and. present(ipy)) then
                            ipx = xx(k)
                            ipy = yy(k)
                        end if
                        call assert( doesIntersect .eqv. doesIntersectCheck, &
                             & "intersect and segments_intersect disagree" )
                        return

                    end if
                end do
            end do
        end if
    end if

    !..Calcul du determinant de la matrice.
    determ = (-(xx(2) - xx(1))) * (yst(2) - yst(1)) + & 
         &                   (yy(2) - yy(1)) * (xst(2) - xst(1))

    !..Si determinant non nul, alors il y a solution.            
    IF (determ .NE. 0.) THEN

        !..Facteur multiplicatif du segment de courbe avec la methode de Cramer.                    
        mult1 = ((-(xst(1)-xx(1))) * (yst(2)-yst(1)) + & 
             &              (yst(1)-yy(1)) * (xst(2)-xst(1)))/determ

        !..Pour avoir intersection, il faut que mult1 soit entre 0 et 1
        IF ((mult1 >= 0.0d0).AND.(mult1 <= 1.0d0)) THEN

            !..Fact. mult. du segment de structure.
            mult2= ((xx(2)-xx(1)) * (yst(1)-yy(1)) - & 
                 &                (yy(2)-yy(1)) * (xst(1)-xx(1)))/determ

            !..Intersection si mult2 entre 0 et 1
            IF ((mult2 >= 0.0d0).AND.(mult2 <= 1.0d0)) THEN
                doesIntersect = .true.
                if (.not. doesIntersectCheck) then 
                    call logmsg(LOGDEBUG, &
                         & "intersect: intersect(true) and segments_intersect(false) disagree, returning false")
                    doesIntersect = .false.
                    return
                end if

                if (present(ipx) .and. present(ipy)) then
!!$                        ipx = xx(1) + mult1 * (xx(2) - xx(1))
!!$                        ipy = yy(1) + mult1 * (yy(2) - yy(1))
                    ipx = xst(1) + mult2 * (xst(2) - xst(1))
                    ipy = yst(1) + mult2 * (yst(2) - yst(1))
                end if
                return
            ENDIF
        ENDIF
    ENDIF

    doesIntersect = .FALSE.
    call assert( doesIntersect .eqv. doesIntersectCheck, &
         & "intersect(false) and segments_intersect(true) disagree" )
  end subroutine intersection



  logical function segments_intersect(p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y)
    double precision, intent(in) :: p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y

    ! internal
    double precision :: d1, d2, d3, d4
    
    d1 = direction(p3x, p3y, p4x, p4y, p1x, p1y)
    d2 = direction(p3x, p3y, p4x, p4y, p2x, p2y)
    d3 = direction(p1x, p1y, p2x, p2y, p3x, p3y)
    d4 = direction(p1x, p1y, p2x, p2y, p4x, p4y)

    segments_intersect = .true.
    
    if (   ((d1 > 0.0d0 .and. d2 < 0.0d0) .or. (d1 < 0.0d0 .and. d2 > 0.0d0)) .and. &
         & ((d3 > 0.0d0 .and. d4 < 0.0d0) .or. (d3 < 0.0d0 .and. d4 > 0.0d0))  ) return

    if ( d1 == 0.0d0 .and. on_segment(p3x, p3y, p4x, p4y, p1x, p1y ) ) return
    if ( d2 == 0.0d0 .and. on_segment(p3x, p3y, p4x, p4y, p2x, p2y ) ) return
    if ( d3 == 0.0d0 .and. on_segment(p1x, p1y, p2x, p2y, p3x, p3y ) ) return
    if ( d4 == 0.0d0 .and. on_segment(p1x, p1y, p2x, p2y, p4x, p4y ) ) return
   
    segments_intersect = .false.

  contains

    double precision function direction( pix, piy, pjx, pjy, pkx, pky )
      double precision, intent(in) :: pix, piy, pjx, pjy, pkx, pky
      
      direction = (pkx - pix) * (pjy - piy) - (pjx - pix) * (pky - piy) 

    end function direction

    logical function on_segment( pix, piy, pjx, pjy, pkx, pky )
      double precision, intent(in) :: pix, piy, pjx, pjy, pkx, pky

      on_segment = (min(pix, pjx) <= pkx) .and. (pkx <= max(pix, pjx)) &
           & .and. (min(piy, pjy) <= pky) .and. (pky <= max(piy, pjy))

    end function on_segment

  END function segments_intersect


  !> Check if points (x1,y1) and (x2,y2) are identical
  !> (i.e, very very close to each other)
  logical function pointsIdentical( x1, y1, x2, y2, absTol )     
    double precision, intent(in) :: x1, y1, x2, y2
    double precision, intent(in), optional :: absTol
    
    ! internal
    double precision :: dist, lAbsTol
    external :: dist
    
    double precision, parameter :: DEFAULTABSTOL = 1e-6

    lAbsTol = DEFAULTABSTOL
    if (present(absTol)) lAbsTol = absTol

    pointsIdentical = ( dist(x1, y1, x2, y2) < lAbsTol )
    
  end function pointsIdentical


end module carre_intersect
