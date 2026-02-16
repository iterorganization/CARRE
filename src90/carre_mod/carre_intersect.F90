module carre_intersect

  use KindDefinitions
  use carre_types
  use itm_assert
  use Logging
  use Helper
  use carre_find

  implicit none

contains

  !> Compute face/structure intersections for all faces in the grid to fill the
  !> grid%faceISec, grid%faceISecPx/y arrays
  !> The finalized flag indicates whether the grid has been finalized, in which case
  !> it is assumed that all face-structure intersections coincide with a grid node.
  !> In this case all faces connected to the boundary node are marked as intersected.
  !> (This is in order to make the final classification robust).
  subroutine computeFaceStructureIntersections( struct, grid, finalized )
    type(CarreStructures), intent(in) :: struct
    type(CarreGrid), intent(inout) :: grid
    logical, intent(in) :: finalized

    ! internal
    integer :: iReg, iPol, iRad, iFace, ix(2), iy(2), istruct, iReg2, ipoint
    integer :: iClosest, pipol(MAX_POINT_OCCUR), pirad(MAX_POINT_OCCUR), npoint
    double precision :: xx(2), yy(2), ipx, ipy
    logical :: doesIntersect

    ! Mark faces intersected by structure elements
    grid%faceISec = .false.
    grid%faceISecIStruct = GRID_UNDEFINED

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
                        & struct, doesIntersect, iStruct, &
                        & ipx=ipx, ipy=ipy )

                    if (.not. doesIntersect) cycle

                    if (.not. finalized) then
                        ! store the intersection for this face
                        grid%faceISec(iFace, iPol, iRad, iReg) = doesIntersect
                        grid%faceISecIStruct(iFace, iPol, iRad, iReg) = istruct
                        grid%faceISecPx(iFace, iPol, iRad, iReg) = ipx
                        grid%faceISecPy(iFace, iPol, iRad, iReg) = ipy
                    else
                        ! find the face end point closest to the intersection, and declare it to be the intersection
                        ! for all faces connected to it
                        iClosest = closestPoint(ipx, ipy, xx, yy)

                        do iReg2 = 1, grid%nreg
                            call findPointInRegion(grid, iReg2, xx(iClosest), yy(iClosest), npoint, pipol, pirad)
                            do ipoint = 1, npoint
                                call markNodeAsIntersection(pipol(ipoint), pirad(ipoint), ireg2, istruct)
                            end do
                        end do
                    end if

                end do
            end do
        end do
    end do

  contains

    subroutine markNodeAsIntersection(ipol, irad, ireg, istruct)
      integer, intent(in) :: ipol, irad, ireg, istruct

      ! internal
      integer :: iFace, iAlign, ipfc, irfc

      ! mark all faces connected to this point that are not yet intersected as intersected

      do iFace = 1, 4 ! left, bottom, right, top

          ipfc = ipol
          irfc = irad
          select case (iFace)
          case(FACE_LEFT)
              if (ipol == 1) cycle
              iAlign = FACE_POLOIDAL
              ipfc = ipol - 1
          case(FACE_BOTTOM)
              if (irad == 1) cycle
              iAlign = FACE_RADIAL
              irfc = irad - 1
          case(FACE_RIGHT)
              if (ipol == grid%np1(ireg)) cycle
              iAlign = FACE_POLOIDAL
          case(FACE_TOP)
              if (irad == grid%nr(ireg)) cycle
              iAlign = FACE_RADIAL
          end select

          if (grid%faceISec(iAlign, ipfc, irfc, iReg)) cycle

          grid%faceISec(iAlign, ipfc, irfc, iReg) = .true.
          grid%faceISecIStruct(iAlign, ipfc, irfc, iReg) = istruct
          grid%faceISecPx(iAlign, ipfc, irfc, iReg) = grid%xmail(ipol, irad, ireg)
          grid%faceISecPy(iAlign, ipfc, irfc, iReg) = grid%ymail(ipol, irad, ireg)
      end do

    end subroutine markNodeAsIntersection

    subroutine getFace(iFace)
      integer, intent(in) :: iFace

      ix(1) = iPol
      iy(1) = iRad
      select case (iFace)
      case(1) ! Poloidal face (iPol, iRad) -> (iPol+1, iRad)
          ix(2) = iPol + 1
          iy(2) = iRad
      case(2) ! Radial face (iPol, iRad) -> (iPol, iRad + 1)
          ix(2) = iPol
          iy(2) = iRad + 1
      end select

      xx(1) = grid%xmail(ix(1), iy(1), iReg)
      yy(1) = grid%ymail(ix(1), iy(1), iReg)
      xx(2) = grid%xmail(ix(2), iy(2), iReg)
      yy(2) = grid%ymail(ix(2), iy(2), iReg)
    end subroutine getFace

  end subroutine computeFaceStructureIntersections

  !> Compute intersection of a line segment curve with the structure elements
  !>
  !> If the optional arguments oldipx, oldipy are not given, the first intersection
  !> found is returned. If the optional point oldipx, oldipy is given, of possible
  !> multiple intersections the one closest to this point is returned.
  subroutine intersect_all_structures(xx, yy, struct, doesIntersect, iStruct, &
      & iSegment, ipx, ipy, oldipx, oldipy)

    REAL(rKind), intent(in) :: xx(:),yy(:)
    type(CarreStructures), intent(in) :: struct
    logical, intent(out) :: doesIntersect
    integer, intent(out), optional :: iSegment, iStruct
    REAL(rKind), intent(out), optional :: ipx, ipy
    REAL(rKind), intent(in), optional :: oldipx, oldipy

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
               & testEndPoints = .false., oldipx=oldipx, oldipy=oldipy)

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
    REAL(rKind), intent(in) :: xx(2),yy(2),xst(:),yst(:)

    logical, intent(out) :: doesIntersect
    integer, intent(out), optional :: iSegment
    REAL(rKind), intent(out), optional :: ipx, ipy
    logical, intent(in), optional :: testEndPoints
    REAL(rKind), intent(in), optional :: oldipx, oldipy

    !  variables locales
    INTEGER i
    double precision :: tmpIpx, tmpIpy, closestIpx, closestIpy, closestDist
    integer :: closestSegment, nIntersections

    double precision :: dist
    external :: dist

    logical :: tmpDoesIntersect

    closestDist = huge(closestDist)
    closestSegment = GRID_UNDEFINED

    nIntersections = 0

    !..Boucle sur la structure.
    DO i=1, size(xst)-1
        call intersection(xx, yy, xst(i:i+1), yst(i:i+1), tmpDoesIntersect, tmpIpx, tmpIpy, testEndPoints)

        if (tmpDoesIntersect) then
            nIntersections = nIntersections + 1
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

    ! Catch the case of multiple intersections
    ! In case of even number of intersections => assume no intersections
    if (closestSegment /= GRID_UNDEFINED .and. mod(nIntersections,2) == 1) then
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
    REAL(rKind), intent(in) :: xx(2),yy(2),xst(2),yst(2)

    logical, intent(out) :: doesIntersect
    REAL(rKind), intent(out), optional :: ipx, ipy
    logical, intent(in), optional :: testEndPoints

    !  variables locales
    INTEGER j, k
    !.. determ: determinant de la matrice des deux equations.
    !.. mult1: facteur multiplicatif du segment de courbe.
    !.. mult2: facteur multiplicatif du segment de structure.
    REAL(rKind) :: mult1,mult2,determ

    logical :: doesIntersectCramer, doesIntersectRobust

    doesIntersectRobust = segments_intersect( xx(1), yy(1), xx(2), yy(2), &
         & xst(1), yst(1), xst(2), yst(2) )

    !if (present(testEndPoints)) then
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
                        call assert( doesIntersect .eqv. doesIntersectRobust, &
                             & "intersect and segments_intersect disagree" )
                        return

                    end if
                end do
            end do
        end if
    !end if

    !..Calcul du determinant de la matrice.
    determ = (-(xx(2) - xx(1))) * (yst(2) - yst(1)) + &
         &                   (yy(2) - yy(1)) * (xst(2) - xst(1))

    !..Si determinant non nul, alors il y a solution.
    IF (determ .NE. 0.) THEN

        !..Facteur multiplicatif du segment de courbe avec la methode de Cramer.
        mult1 = ((-(xst(1)-xx(1))) * (yst(2)-yst(1)) + &
             &              (yst(1)-yy(1)) * (xst(2)-xst(1)))/determ
        !..Fact. mult. du segment de structure.
        mult2= ((xx(2)-xx(1)) * (yst(1)-yy(1)) - &
             &                (yy(2)-yy(1)) * (xst(1)-xx(1)))/determ


        !..Pour avoir intersection, il faut que mult1 soit entre 0 et 1
        IF ((mult1 >= 0.0d0).AND.(mult1 <= 1.0d0)) THEN
            !..Intersection si mult2 entre 0 et 1
            IF ((mult2 >= 0.0d0).AND.(mult2 <= 1.0d0)) THEN
                doesIntersectCramer = .true.
            ENDIF
        ENDIF

        ! We have more trust in the result from the robust intersection test
        doesIntersect = doesIntersectRobust

        ! However, we have to use the coefficients from the non-robust
        ! intersection computation to get an intersection point
        if (doesIntersect .and. present(ipx) .and. present(ipy)) then
                ipx = xst(1) + mult2 * (xst(2) - xst(1))
                ipy = yst(1) + mult2 * (yst(2) - yst(1))
        end if

        return
    ELSE
        ! Zero determinant means colinear. The line segments can still
        ! coincide/overlap. In this case we declare the lines to not intersect.
        doesIntersect = .FALSE.
        ! We also do NOT check against the robust intersection test result
        ! (which will indicate overlapping line segments to intersect).
    ENDIF

  end subroutine intersection


  !> Test whether a point is on one of the real structures
  subroutine isPointOnStructure(x, y, struct, onStructure, iStruct )

    REAL(rKind), intent(in) :: x, y
    type(CarreStructures), intent(in) :: struct
    logical, intent(out) :: onStructure
    integer, intent(out), optional :: iStruct

    ! internal
    integer :: iSeg, is

    ! This tolerance parameter is basically used for floating point
    ! equivalence tests. Increase it will increase the tolerance at
    ! which the point is accepted to be on a structure segment.
    double precision, parameter :: TOLERANCE = 1.0d-10

    do is = 1, struct%rnstruc
        do iSeg = 1, abs(struct%rnpstru(is)) - 1

            if (on_segment( struct%rxstruc(iSeg, is), struct%rystruc(iSeg, is), &
                 & struct%rxstruc(iSeg+1, is), struct%rystruc(iSeg+1, is), &
                 & x, y, TOLERANCE)) then
                if (present(iStruct)) iStruct = is
                onStructure = .true.
                return
            end if

        end do
    end do

    onStructure = .false.
    if (present(iStruct)) iStruct = GRID_UNDEFINED
  end subroutine isPointOnStructure



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

    if ( d1 == 0.0d0 .and. in_rectangle(p3x, p3y, p4x, p4y, p1x, p1y ) ) return
    if ( d2 == 0.0d0 .and. in_rectangle(p3x, p3y, p4x, p4y, p2x, p2y ) ) return
    if ( d3 == 0.0d0 .and. in_rectangle(p1x, p1y, p2x, p2y, p3x, p3y ) ) return
    if ( d4 == 0.0d0 .and. in_rectangle(p1x, p1y, p2x, p2y, p4x, p4y ) ) return

    segments_intersect = .false.
  END function segments_intersect

  ! Compute cross product between vectors i->j and i->k
  double precision function direction( pix, piy, pjx, pjy, pkx, pky )
    double precision, intent(in) :: pix, piy, pjx, pjy, pkx, pky

    direction = (pkx - pix) * (pjy - piy) - (pjx - pix) * (pky - piy)

  end function direction

  ! Check whether point k is in a rectangle with the diagonal
  ! given by the points i, j
  logical function in_rectangle( pix, piy, pjx, pjy, pkx, pky, atol)
    double precision, intent(in) :: pix, piy, pjx, pjy, pkx, pky
    double precision, intent(in), optional :: atol

    ! internal
    double precision :: latol

    latol = 0.0d0
    if (present(atol)) latol = atol

    in_rectangle = (min(pix, pjx)-latol <= pkx) .and. (pkx <= max(pix, pjx)+latol) &
         & .and. (min(piy, pjy)-latol <= pky) .and. (pky <= max(piy, pjy)+latol)

  end function in_rectangle

  ! Check whether point k is on the segment defined by points i, j
  logical function on_segment( pix, piy, pjx, pjy, pkx, pky, atol )
    double precision, intent(in) :: pix, piy, pjx, pjy, pkx, pky
    double precision, intent(in), optional :: atol

    ! internal
    double precision :: d, latol

    latol = 0.0d0
    if (present(atol)) latol = atol

    d = direction( pix, piy, pjx, pjy, pkx, pky )
    on_segment = (abs(d) < latol .and. in_rectangle(pix, piy, pjx, pjy, pkx, pky))
  end function on_segment





end module carre_intersect
