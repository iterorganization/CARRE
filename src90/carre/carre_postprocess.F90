module carre_postprocess

  use carre_types
  use CarreDiagnostics
  use carre_niveau
  use carre_criteria
  use itm_assert
  use Logging
  use carre_intersect

#ifdef USE_SILO
  use SiloIO
#endif
  use CarreSiloIO

  implicit none

#include <CARREDIM.F>
#include <COMRLX.F>

  private

  public carre_postprocess_computation

  ! Number of faces of a cell. 
  ! Bit positions in grid%cellFaceFlag marking that faces of a cell are intersected
  integer, parameter :: INDEX_FACE_LEFT = 1
  integer, parameter :: INDEX_FACE_BOTTOM = 2
  integer, parameter :: INDEX_FACE_RIGHT = 3
  integer, parameter :: INDEX_FACE_TOP = 4

  logical, parameter :: DEBUGFILES_ADDRADIALLINE = .false.
  logical, parameter :: DEBUGFILES_INSERTPOINTS = .false.

contains

  !> Perform postprocessing on the grid as generated in carre_main.
  !> This is only relevant if the cut-cell type extended grid generation is requested.

  subroutine carre_postprocess_computation(par, equ, grid, struct)

    type(CarreParameters), intent(in) :: par
    type(CarreEquilibrium), intent(inout) :: equ
    type(CarreGrid), intent(inout) :: grid
    type(CarreStructures), intent(in) :: struct

    ! internal
    integer :: iReg, iPostProcess, nCellsToRefine, nCellsToCoarsen
    logical :: doesIntersect
    integer :: npReg(nregmx), npDiffRefine, npDiffCoarsen, nIterNoAction
    integer :: ipx, xipol1, xirad1, xipol2, xirad2


    ! Only do postprocessing when doing grid extension
    if (par%gridExtensionMode == GRID_EXTENSION_OFF) return

    call logmsg( LOGINFO, "carre_postprocess_computation: doing cut-cell type grid (grid extension)" )

    ! Initialize grid line flags
    grid%lineFlagRad = GRIDLINE_BASELINE
    do iReg = 1, grid%nreg

        ! first and last radial line is required
        call markRadialLineRequired(grid, iReg, 1)
        call markRadialLineRequired(grid, iReg, grid%np1(iReg))

        ! first and last radial line is required
        !grid%lineFlagRad(1, iReg) = GRIDLINE_REQUIRED
        !grid%lineFlagRad(grid%np1(iReg), iReg) = GRIDLINE_REQUIRED

        ! The grid lines going into the x-point are also required...
        do ipx = 1, equ%npx
            call findPointInRegion(grid, iReg, equ%ptx(ipx), equ%pty(ipx), xipol1, xirad1, xipol2, xirad2)
            if (xipol1 /= GRID_UNDEFINED) then
                call logmsg(LOGINFO,  'carre_postprocess_computation: marking radial line required&
                     & due to x-point #'//int2str(ipx)//" in region "//int2str(iReg))
                !grid%lineFlagRad(xipol1, iReg) = GRIDLINE_REQUIRED
                call markRadialLineRequired(grid, iReg, xipol1)
            end if
            if (xipol2 /= GRID_UNDEFINED) then
                call logmsg(LOGINFO,  'carre_postprocess_computation: marking radial line required&
                     & due to x-point #'//int2str(ipx)//" in region "//int2str(iReg))
                !grid%lineFlagRad(xipol2, iReg) = GRIDLINE_REQUIRED
                call markRadialLineRequired(grid, iReg, xipol2)
            end if
        end do
    end do

    npDiffRefine = 0
    npDiffCoarsen = 0
    nIterNoAction = 0

    do iPostProcess = 1, 21
        if ( mod( iPostProcess, 2 ) == 1 ) then
            call logmsg(LOGINFO,  'carre_postprocess_computation: pass '//int2str(iPostProcess)//": refinement")
        else
            call logmsg(LOGINFO,  'carre_postprocess_computation: pass '//int2str(iPostProcess)//": coarsening")
        end if

        ! Compute face/structure intersections
        call computeFaceStructureIntersections( struct, grid )

        ! Mark points to be inside/outside of vessel
        call labelPointsInsideOutside(equ, grid)

        ! Compute the object categorization
        call categorizeCellsAndFaces()

        ! Compute connection information between regions
        call computeConnectionInformation()

        if (iPostProcess > 9) then
            call writeGridStateToSiloFile('carrePostPrcA'//int2str(iPostProcess), equ, struct, grid)
        else
            call writeGridStateToSiloFile('carrePostPrcA0'//int2str(iPostProcess), equ, struct, grid)
        end if

        if ( mod( iPostProcess, 2 ) == 1 ) then
            ! On odd iterations, do refinement.
            npReg(1:grid%nReg) = grid%np1(1:grid%nReg)
            ! Cells need refinement because of broken geometry?
            nCellsToRefine = count( grid%cellflag == GRID_BOUNDARY_REFINE_FIX )
            if ( nCellsToRefine > 0 ) then
                call logmsg(LOGDEBUG,  'carre_postprocess, iteration '//int2str(iPostProcess)//': '//&
                     &int2str(nCellsToRefine)//' cells to refine because of broken geometry')

                ! Fix cells by modifying the grid accordingly
                call fixCells(equ, struct, grid)

                ! If we had broken cells, skip normal refinement 
            end if

!!$       ! cells need refinement due to low resolution?
!!$       nCellsToRefine = count( grid%cellflag == GRID_REFINE )
!!$       if ( nCellsToRefine > 0 ) then
!!$           call logmsg(LOGDEBUG,  'carre_postprocess, iteration '//int2str(iPostProcess)//': '//&
!!$               &int2str(nCellsToRefine)//' cells to refine')
!!$
!!$           ! Fix cells by modifying the grid accordingly
!!$           !call coarsenCells(equ, struct, grid)
!!$           cycle
!!$       end if

            npDiffRefine = sum(npReg(1:grid%nReg) - grid%np1(1:grid%nReg))
            call logmsg(LOGDEBUG,  'carre_postprocess: iteration '//int2str(iPostProcess)//&
                 &": added "//int2str(npDiffCoarsen)//" radial lines in all regions")
            if (npDiffRefine == 0) then
                nIterNoAction = nIterNoAction + 1
            else
                nIterNoAction = 0
            end if

        else
            ! On even iterations, do coarsening.
            npReg(1:grid%nReg) = grid%np1(1:grid%nReg)
            ! cells need coarsening?
            nCellsToCoarsen = count( grid%cellflag == GRID_INTERNAL_COARSEN ) &
                 & + count( grid%cellflag == GRID_BOUNDARY_COARSEN )
            if ( nCellsToCoarsen > 0 ) then
                call logmsg(LOGDEBUG,  'carre_postprocess, iteration '//int2str(iPostProcess)//': '//&
                     &int2str(nCellsToCoarsen)//' cells to coarsen')

                ! Fix cells by modifying the grid accordingly
                call coarsenCells(grid)
            end if

            npDiffCoarsen = sum(npReg(1:grid%nReg) - grid%np1(1:grid%nReg))
            call logmsg(LOGDEBUG,  'carre_postprocess: iteration '//int2str(iPostProcess)//&
                 &": removed "//int2str(npDiffCoarsen)//" radial lines in all regions")

            if (npDiffRefine == 0) then
                nIterNoAction = nIterNoAction + 1
            else
                nIterNoAction = 0
            end if
        end if

        ! If no changes, exit
        if ( nIterNoAction == 3 ) exit
    end do

    if ( (npDiffRefine == 0) .and. (npDiffCoarsen == 0) ) then
        call logmsg(LOGDEBUG,  'carre_postprocess: grid state converged at iteration '//int2str(iPostProcess))
    else
        call logmsg(LOGDEBUG,  'carre_postprocess: grid state did not converge after iteration '//int2str(iPostProcess))
    end if

    ! write results of postprocessing iteration
    call writeGridStateToSiloFile('carrePostProcC0', equ, struct, grid)

    ! Finalize grid cell fixes by moving external points
    ! of faces onto boundary intersections
    call finalizeCells(grid)

    ! write final postprocessing result
    call writeGridStateToSiloFile('carrePostProcD0', equ, struct, grid)

  contains

    subroutine categorizeCellsAndFaces()

      ! internal
      integer, dimension(npmamx-1,nrmamx-1,nregmx) :: cellIntNodeCount, cellExtNodeCount
      integer :: iReg, iPol, iRad, iFace, i, j

      ! Transfer face intersection information to cells (filling grid%cellFaceFlag)
      grid%cellFaceFlag = 0

      do iReg = 1, grid%nreg

          do iPol = 1, grid%np1(iReg) - 1
              do iRad = 1, grid%nr(iReg) - 1

                  do iFace = 1, 4 ! left, bottom, right, top

                      select case (iFace)
                      case(FACE_LEFT) ! left
                          doesIntersect = grid%faceISec(FACE_RADIAL, iPol, iRad, iReg)
                      case(FACE_BOTTOM) ! bottom
                          doesIntersect = grid%faceISec(FACE_POLOIDAL, iPol, iRad, iReg)
                      case(FACE_RIGHT) ! right
                          doesIntersect = grid%faceISec(FACE_RADIAL, iPol + 1, iRad, iReg)
                      case(FACE_TOP) ! top
                          doesIntersect = grid%faceISec(FACE_POLOIDAL, iPol, iRad + 1, iReg)
                      end select

                      if (doesIntersect) then     
                          grid%cellFaceFlag(iPol, iRad, iReg) = &
                               & ibset(grid%cellFaceFlag(iPol, iRad, iReg), iFace)
                      end if
                  end do

              end do
          end do
      end do

      ! First mark cells internal/external
      grid%cellFlag = GRID_EXTERNAL

      ! Count internal / external corners of cells
      cellIntNodeCount = 0
      cellExtNodeCount = 0

      do iReg = 1, grid%nreg

          do iPol = 1, grid%np1(iReg) - 1
              do iRad = 1, grid%nr(iReg) - 1

                  ! loop over cell corners
                  do i = 0, 1 ! poloidal 
                      do j = 0, 1 ! radial

                          if ( isInternal(grid%pointFlag(iPol+i, iRad+j, iReg)) ) &
                               & cellIntNodeCount(iPol, iRad, iReg) = cellIntNodeCount(iPol, iRad, iReg) + 1

                          if ( isExternal(grid%pointFlag(iPol+i, iRad+j, iReg)) ) &
                               & cellExtNodeCount(iPol, iRad, iReg) = cellExtNodeCount(iPol, iRad, iReg) + 1

                      end do
                  end do

                  ! Cell is internal if at least one point is internal
                  if (cellIntNodeCount(iPol, iRad, iReg) > 0) &
                       & grid%cellFlag(iPol, iRad, iReg) = GRID_INTERNAL
              end do
          end do
      end do

      ! Translate the grid%cellFaceFlag array into cell flags
      ! First assume all boundary cells are unproblematic
      where ( (grid%cellFaceFlag /= GRID_UNDEFINED) .and. (grid%cellFlag == GRID_INTERNAL) ) &
           & grid%cellflag = GRID_BOUNDARY

      ! Then figure out which ones must be refined: cells with more than five edges
      ! Current recipe:
      ! -cells with three internal points and one external point
      where ((cellExtNodeCount == 1) .and. (cellIntNodeCount == 3)) grid%cellflag = GRID_BOUNDARY_REFINE_FIX

      ! Figure out which must be coarsened due to too high resolution
      call computeHxHy(grid)
      where ( (grid%cellflag == GRID_BOUNDARY) &
           & .and. (grid%hx < pasmin) ) grid%cellflag = GRID_BOUNDARY_COARSEN
      where ( (grid%cellflag == GRID_INTERNAL) &
           & .and. (grid%hx < pasmin) ) grid%cellflag = GRID_INTERNAL_COARSEN

      ! Figure out which must be refined due to too low resolution 
      ! Increase resolution at the targets
      where ( (grid%cellflag == GRID_BOUNDARY) &
           & .and. (grid%hx > pasmin) ) grid%cellflag = GRID_BOUNDARY_REFINE


    end subroutine categorizeCellsAndFaces

    ! For use in categorizeCellsAndFaces
    logical function isInternal(pointFlag)
      integer, intent(in) :: pointFlag

      isInternal = (pointFlag == GRID_INTERNAL)
    end function isInternal

    ! For use in categorizeCellsAndFaces
    logical function isExternal(pointFlag)
      integer, intent(in) :: pointFlag

      isExternal = (pointFlag == GRID_EXTERNAL)
    end function isExternal

    ! Compute connection information between regions
    subroutine computeConnectionInformation()

      ! internal
      integer :: iReg, ipol, iRegOther, iOtherFcP, iOtherFcR
      logical :: regionHasFace

      grid%nbFaceReg = GRID_UNDEFINED
      grid%nbFaceIPol = GRID_UNDEFINED
      grid%nbFaceIRad = GRID_UNDEFINED

      do iReg = 1, grid%nreg
          do iPol = 1, grid%np1(iReg) - 1

              do iRegOther = 1, grid%nreg
                  if (iRegOther == iReg) cycle

                  ! Bottom (irad = 1) face of strip in this region
                  call findFaceInRegion( grid, iRegOther, &
                       & grid%xmail(ipol    , 1, iReg), &
                       & grid%ymail(ipol    , 1, iReg), &
                       & grid%xmail(ipol + 1, 1, iReg), &
                       & grid%ymail(ipol + 1, 1, iReg), &
                       & iFcP=iOtherFcP, &
                       & iFcR=iOtherFcR, &
                       & regionHasFace = regionHasFace )
                  if (regionHasFace) then
                      grid%nbFaceIPol(iReg, iPol, 2) = iOtherFcP
                      grid%nbFaceIRad(iReg, iPol, 2) = iOtherFcR
                      call assert(grid%nbFaceIPol(iReg, iPol, 2) /= GRID_UNDEFINED)
                      call assert(grid%nbFaceIRad(iReg, iPol, 2) /= GRID_UNDEFINED)
                      grid%nbFaceReg(iReg, iPol, 2) = iRegOther
                  end if

                  ! Top (irad = grid%nr(iReg)) face of strip in this region
                  call findFaceInRegion( grid, iRegOther, &
                       & grid%xmail(ipol    , grid%nr(iReg), iReg), &
                       & grid%ymail(ipol    , grid%nr(iReg), iReg), &
                       & grid%xmail(ipol + 1, grid%nr(iReg), iReg), &
                       & grid%ymail(ipol + 1, grid%nr(iReg), iReg), &
                       & iFcP=iOtherFcP, &
                       & iFcR=iOtherFcR, &
                       & regionHasFace=regionHasFace )
                  if (regionHasFace) then
                      grid%nbFaceIPol(iReg, iPol, 1) = iOtherFcP
                      grid%nbFaceIRad(iReg, iPol, 1) = iOtherFcR
                      call assert(grid%nbFaceIPol(iReg, iPol, 1) /= GRID_UNDEFINED)
                      call assert(grid%nbFaceIRad(iReg, iPol, 1) /= GRID_UNDEFINED)
                      grid%nbFaceReg(iReg, iPol, 1) = iRegOther
                  end if

              end do

          end do
      end do

    end subroutine computeConnectionInformation

  end subroutine carre_postprocess_computation


  !> Mark the radial grid line at poloidal position iPol in region iReg as required in the grid.
  !> Also follows the grid line in neighbouring regions
  recursive subroutine markRadialLineRequired(grid, iReg, iPol)
    type(CarreGrid), intent(inout) :: grid
    integer, intent(in) :: iReg, iPol
    
    ! internal
    integer :: iRegOther, iBnd, iPolOther, iRadOther
    double precision :: xBnd, yBnd

    if (grid%lineFlagRad(iPol, iReg) == GRIDLINE_REQUIRED ) then
        ! already touched this region
        return
    else
        grid%lineFlagRad(iPol, iReg) = GRIDLINE_REQUIRED    
    end if

    ! Follow into other regions
    do iBnd = 1, 2

        select case (iBnd)            
        case(1)
            xBnd = grid%xmail(iPol, 1, iReg)
            yBnd = grid%ymail(iPol, 1, iReg)
        case(2)
            xBnd = grid%xmail(iPol, grid%nr(iReg), iReg)
            yBnd = grid%ymail(iPol, grid%nr(iReg), iReg)
        end select

        do iRegOther = 1, grid%nreg
            if (iRegOther == iReg) cycle

            call findPointInRegion(grid, iRegOther, xBnd, yBnd, iPolOther, iRadOther)
            if (iPolOther /= GRID_UNDEFINED) then
                call markRadialLineRequired(grid, iRegOther, iPolOther)
            end if
        end do

    end do    

  end subroutine markRadialLineRequired


  ! Compute characteristic cell size in x and y direction
  subroutine computeHxHy( grid )
    type(CarreGrid), intent(inout) :: grid

    ! internal
    integer :: iReg, iPol, iRad
    double precision :: mx1, my1, mx2, my2, dist
    external :: dist


    do iReg = 1, grid%nreg

        do iPol = 1, grid%np1(iReg) - 1
            do iRad = 1, grid%nr(iReg) - 1
                ! hx
                mx1 = ( grid % xmail( iPol, iRad, iReg ) &
                     & + grid % xmail( iPol, iRad + 1, iReg ) ) / 2
                my1 = ( grid % ymail( iPol, iRad, iReg ) &
                     & + grid % ymail( iPol, iRad + 1, iReg ) ) / 2
                mx2 = ( grid % xmail( iPol + 1, iRad, iReg ) &
                     & + grid % xmail( iPol + 1, iRad + 1, iReg ) ) / 2
                my2 = ( grid % ymail( iPol + 1, iRad, iReg ) &
                     & + grid % ymail( iPol + 1, iRad + 1, iReg ) ) / 2

                grid % hx( iPol, iRad, iReg ) = dist( mx1, my1, mx2, my2 )

                ! hy
                mx1 = ( grid % xmail( iPol, iRad, iReg ) &
                     & + grid % xmail( iPol + 1, iRad, iReg ) ) / 2
                my1 = ( grid % ymail( iPol, iRad, iReg ) &
                     & + grid % ymail( iPol + 1, iRad, iReg ) ) / 2
                mx2 = ( grid % xmail( iPol, iRad + 1, iReg ) &
                     & + grid % xmail( iPol + 1, iRad + 1, iReg ) ) / 2
                my2 = ( grid % ymail( iPol, iRad + 1, iReg ) &
                     & + grid % ymail( iPol + 1, iRad + 1, iReg ) ) / 2

                grid % hy( iPol, iRad, iReg ) = dist( mx1, my1, mx2, my2 )
            end do
        end do

    end do

  end subroutine computeHxHy




  !> Label points to be in/outside of the vessel by stepping along faces
  !> and exploiting the intersection information
  subroutine labelPointsInsideOutside( equ, grid )      
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreGrid), intent(inout) :: grid

    ! internal
    integer :: iReg, xip, xir
    integer :: points(npmamx,nrmamx,nregmx)

    points = GRID_UNDEFINED

    do iReg = 1, grid%nreg
        ! For region, find an x-point (which is declared to be on the inside)
        ! TODO: for limiter, just take any point on the core boundary.
        call findXPointInRegion(equ, grid, iReg, xip, xir)

        ! Walk away from there (recursively) until all points are marked.
        ! (this recursion will do transitions between the regions)
        call markInternalPoints(xip, xir, iReg, points)

        call markBoundaryPoints(iReg, points)
    end do


    ! all points that are still undefined are external
    where (points == GRID_UNDEFINED) points = GRID_EXTERNAL

    grid%pointFlag = points

  contains

    !> Mark point (ip,ir) as internal and mark connected points recursively
    recursive subroutine markInternalPoints(ip, ir, iReg, points)
      integer, intent(in) :: ip, ir, iReg
      integer, intent(inout) :: points(npmamx,nrmamx,nregmx)

      ! internal
      logical :: faceIntersect
      integer :: ipOther, irOther, iRegOther

      ! Is this point inside the region?
      if ( (ip < 1) .or. (ir < 1) &
           & .or. (ip > grid%np1(iReg)) .or. (ir > grid%nr(iReg)) ) then
          ! no -> skip
          return
      end if

      ! Have we visited this point before?
      if ( points(ip, ir, iReg) /= GRID_UNDEFINED ) then
          ! Yes -> skip
          return 
      end if

      ! So this point is internal
      points(ip, ir, iReg) = GRID_INTERNAL

      ! Now go to all points connect to this one via a face that
      ! is not intersect by a structure element

      ! radial direction
      ! ...go along top face
      if (ir < grid%nr(iReg)) then
          faceIntersect = grid%faceISec(FACE_RADIAL, ip, ir, iReg)
          if (.not. faceIntersect) then
              ! Internal point! Recursive propagation.
              call markInternalPoints(ip, ir+1, iReg, points)
          else
              ! Boundary point? (non-recursive)
              call checkBoundaryPoint(ip, ir+1, iReg, &
                   & grid%faceISecPx(FACE_RADIAL, ip, ir, iReg), &
                   & grid%faceISecPy(FACE_RADIAL, ip, ir, iReg), points )
          end if
      end if
      ! ...go along bottom face
      if (ir > 1) then
          faceIntersect = grid%faceISec(FACE_RADIAL, ip, ir-1, iReg)
          if (.not. faceIntersect) then
              call markInternalPoints(ip, ir-1, iReg, points)
          else
              call checkBoundaryPoint(ip, ir-1, iReg,&
                   & grid%faceISecPx(FACE_RADIAL, ip, ir-1, iReg), &
                   & grid%faceISecPy(FACE_RADIAL, ip, ir-1, iReg), points )
          end if
      end if

      ! poloidal direction
      ! ...go along right face
      if (ip < grid%np1(iReg)) then
          faceIntersect = grid%faceISec(FACE_POLOIDAL, ip, ir, iReg)
          if (.not. faceIntersect) then
              call markInternalPoints(ip+1, ir, iReg, points)
          else
              call checkBoundaryPoint(ip+1, ir, iReg, &
                   & grid%faceISecPx(FACE_POLOIDAL, ip, ir, iReg), &
                   & grid%faceISecPy(FACE_POLOIDAL, ip, ir, iReg), points )
          end if
      end if
      ! ...go along left face
      if (ip > 1) then
          faceIntersect = grid%faceISec(FACE_POLOIDAL, ip-1, ir, iReg)
          if (.not. faceIntersect) then
              call markInternalPoints(ip-1, ir, iReg, points)
          else
              call checkBoundaryPoint(ip-1, ir, iReg, &
                   & grid%faceISecPx(FACE_POLOIDAL, ip-1, ir, iReg), &
                   & grid%faceISecPy(FACE_POLOIDAL, ip-1, ir, iReg), points )
          end if
      end if

      ! Finally, also propagate the information that this point
      ! is internal to the other grid regions
      do iRegOther = 1, grid%nreg
          if (iRegOther == iReg) cycle
          call findPointInRegion(grid, iRegOther, grid%xmail(ip, ir, iReg), &
               & grid%ymail(ip, ir, iReg), &
               & ipOther, irOther)

          call markInternalPoints(ipOther, irOther, iRegOther, points)
      end do

    end subroutine markInternalPoints

    !> Check whether the given point coincides with the given intersection
    !> point. If yes, mark it as a boundary point.
    subroutine checkBoundaryPoint( ip, ir, iReg, isecPx, isecPy, points )
      integer, intent(in) :: ip, ir, iReg
      double precision, intent(in) :: isecPx, isecPy
      integer, intent(inout) :: points(npmamx,nrmamx,nregmx)

      return
      if ( pointsIdentical( grid%xmail(ip, ir, iReg), grid%ymail(ip, ir, iReg), &
           & isecPx, isecPy ) ) then
          points(ip, ir, iReg) = GRID_BOUNDARY
      end if

    end subroutine checkBoundaryPoint

    !> This a brute-force routine identifying all grid points in the region coinciding
    !> with a face-structure intersection, and marking them to boundary points.
    !> This is supposed to be done by checkBoundaryPoint during the recursion,
    !> but somehow this is unreliable.
    subroutine markBoundaryPoints(iReg, points)
      integer, intent(in) :: iReg
      integer, intent(inout) :: points(npmamx,nrmamx,nregmx)

      ! internal
      integer :: ip, ir
      double precision :: ipx, ipy

      do ip = 1, grid%np1(iReg)
          do ir = 1, grid%nr(iReg)

              ! poloidal face
              if ((ip < grid%np1(iReg)) &
                   & .and. grid%faceISec(FACE_POLOIDAL,ip,ir,iReg)) then
                  ipx = grid%faceISecPx(FACE_POLOIDAL,ip,ir,iReg)
                  ipy = grid%faceISecPy(FACE_POLOIDAL,ip,ir,iReg)                              

                  if ( pointsIdentical(ipx, ipy, &
                       & grid%xmail(ip,ir,iReg), grid%ymail(ip,ir,iReg)) ) points(ip,ir,iReg) = GRID_BOUNDARY
                  if ( pointsIdentical(ipx, ipy, &
                       & grid%xmail(ip+1,ir,iReg), grid%ymail(ip+1,ir,iReg)) ) points(ip+1,ir,iReg) = GRID_BOUNDARY
              end if

              ! radial face
              if ((ir < grid%nr(iReg)) &
                   & .and. grid%faceISec(FACE_RADIAL,ip,ir,iReg)) then
                  ipx = grid%faceISecPx(FACE_RADIAL,ip,ir,iReg)
                  ipy = grid%faceISecPy(FACE_RADIAL,ip,ir,iReg)                              

                  if ( pointsIdentical(ipx, ipy, &
                       & grid%xmail(ip,ir,iReg), grid%ymail(ip,ir,iReg)) ) points(ip,ir,iReg) = GRID_BOUNDARY
                  if ( pointsIdentical(ipx, ipy, &
                       & grid%xmail(ip,ir+1,iReg), grid%ymail(ip,ir+1,iReg)) ) points(ip,ir+1,iReg) = GRID_BOUNDARY
              end if

          end do
      end do

    end subroutine markBoundaryPoints

  end subroutine labelPointsInsideOutside


  !> Fix broken cells by modifying the grid 
  subroutine fixCells(equ, struct, grid)
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(in) :: struct
    type(CarreGrid), intent(inout) :: grid

    ! internal
    integer :: iReg, ip, ir, iBrokenCell
    logical :: isecTopFace, isecBotFace

    logical :: refineFace(2,npmamx,nrmamx,nregmx)
    double precision :: px, py
    
    logical, dimension(nsepsegmx) :: sepSegUpdated ! work array to track which par


    ! Map from original grid indices (1:grid%np(iReg), 1:grid%nr(iReg))
    ! to current grid situation (with added radial lines)
    integer :: irMap(nrmamx, nregmx), ipMap(npmamx, nregmx)
    integer :: npRadOriginal(nregmx), npPolOriginal(nregmx)

    ! flags marking strips of cells in the radial direction as refined
    logical :: cellsRefinedFlag(nregmx, npmamx-1)

    ! Set up identity maps for radial and poloidal point indices
    npRadOriginal = grid%nr
    npPolOriginal = grid%np1
    do iReg = 1, grid%nreg
        ipMap(:, iReg) = (/ (ip, ip = 1, grid%np1(iReg)) /)
        irMap(:, iReg) = (/ (ir, ir = 1, grid%nr(iReg)) /)
    end do

    ! Figure out what faces have to be refined to fix the broken cells
    refineFace = .false.

    do iReg = 1, grid%nreg
        do ip = 1, npPolOriginal(iReg) - 1
            do ir = 1, npRadOriginal(iReg) - 1

                if (grid%cellflag(ip, ir, iReg) == GRID_BOUNDARY_REFINE_FIX) then
                    ! broken cell should have one intersected poloidal face               
                    isecTopFace = btest(grid%cellFaceFlag(ip, ir, iReg), INDEX_FACE_TOP)
                    isecBotFace = btest(grid%cellFaceFlag(ip, ir, iReg), INDEX_FACE_BOTTOM)

                    ! If more than two intersections per cell,  grid/geometry has issues
                    if ( isecTopFace .and. isecBotFace ) then
                        call logmsg(LOGDEBUG,  'fixCells: broken cell has two intersected poloidal faces:'//&
                             & int2str(ip)//', '//int2str(ir)//', '//int2str(iReg)//' corner at '//&
                             & real2str(grid%xmail(ip,ir,iReg))//' '//real2str(grid%ymail(ip,ir,iReg)))
                    end if

                    ! Here, we are only interested in "real" intersections, i.e.
                    ! intersections in the middle of the face, not directly in the face 
                    ! endpoints

                    if (iSecTopFace) then
                        px = grid%faceISecPx(FACE_POLOIDAL, ip, ir+1, iReg)
                        py = grid%faceISecPy(FACE_POLOIDAL, ip, ir+1, iReg)

                        if (.not. ( &
                             & pointsIdentical(px, py, grid%xmail(ip,ir+1,iReg), grid%ymail(ip,ir+1,iReg)) &
                             & .or. &
                             & pointsIdentical(px, py, grid%xmail(ip+1,ir+1,iReg), grid%ymail(ip+1,ir+1,iReg)) &
                             & ) ) &
                             & then
                            refineFace(FACE_POLOIDAL, ip, ir+1, iReg) = .true.
                        end if
                    end if

                    if (iSecBotFace) then
                        px = grid%faceISecPx(FACE_POLOIDAL, ip, ir, iReg)
                        py = grid%faceISecPy(FACE_POLOIDAL, ip, ir, iReg)

                        if (.not. ( &
                             & pointsIdentical(px, py, grid%xmail(ip,ir,iReg), grid%ymail(ip,ir,iReg)) &
                             & .or. &
                             & pointsIdentical(px, py, grid%xmail(ip+1,ir,iReg), grid%ymail(ip+1,ir,iReg)) &
                             & ) ) &
                             & then
                            refineFace(FACE_POLOIDAL, ip, ir, iReg) = .true.
                        end if

                    end if
                end if

            end do

        end do
    end do


    ! Loop over all poloidal faces and fix problem cells by inserting radial grid lines where requested.
    ! This modifies the mailx and maily arrays, especially changing the number of grid points.
    ! The grid node indices thus become inconstent with the other arrays. This 
    ! is accounted for by using mapping arrays between the original grid and the modified/fixed 
    ! one. The maps are kept in ipMap and irMap:
    ! ipMap(ip, iReg) is the new poloidal index originally corresponding with poloidal index ip
    ! in region iReg. Same for irMap.

    iBrokenCell = 0
    cellsRefinedFlag = .false.    

    do iReg = 1, grid%nreg
        do ip = 1, npPolOriginal(iReg) - 1
            do ir = 1, npRadOriginal(iReg)

                if (refineFace(FACE_POLOIDAL, ip, ir, iReg)) then        

                    if (cellsRefinedFlag(iReg, ip)) then
                        call logmsg( LOGDEBUG, "fixCells: skipping refinement of radial cell strip in &
                             & region "//int2str(iReg)//", ipol="//int2str(ip)//" - was already refined in&
                             & this iteration")
                        cycle
                    end if

                    ! Debug plotting: abuse region number for current cell
                    iBrokenCell = iBrokenCell + 1
                    call csioSetRegion(100 + iBrokenCell)

                    ! Add a radial line through the intersection point
                    ! of this face with the structure. Note that the
                    ! intersection point known at this point 
                    ! (grid%faceISecPx(INDEX_FACE_TOP, ip, ir, iReg),
                    !   grid%faceISecPy(INDEX_FACE_TOP, ip, ir, iReg)) 
                    ! is inaccurate and will be recomputed 
                    sepSegUpdated(:) = .false.
                    call addRadialLine( equ, struct, grid, &
                         & iReg, sepSegUpdated, &
                         & grid%faceISecPx(FACE_POLOIDAL, ip, ir, iReg), &
                         & grid%faceISecPy(FACE_POLOIDAL, ip, ir, iReg), &
                         & recomputeIntersection = .true., &
                         & iFcR = irMap(ir, iReg) )

                    call markRefined(iReg, ip)

                    ! update grid index map to account for radial line
                    ! We added a radial grid line between poloidal points ip and ip + 1
                    ! Shift all indices bigger than that up by one
                    ! TODO: check we don't run out of space...
                    ipMap(ip + 1 : npPolOriginal(iReg), iReg) = ipMap(ip + 1 : npPolOriginal(iReg), iReg) + 1
                end if

            end do
        end do
    end do

  contains

    subroutine markRefined(iReg, iPol)
      integer, intent(in) :: iReg, iPol

      ! internal
      integer :: iDir

      cellsRefinedFlag(iReg, iPol) = .true.

      do iDir = 1, 2
          if (grid%nbFaceReg(iReg, iPol, iDir) /= GRID_UNDEFINED) then
              cellsRefinedFlag(grid%nbFaceReg(iReg, iPol, iDir), grid%nbFaceIPol(iReg, iPol, iDir)) = .true.              
          end if
      end do

    end subroutine markRefined

  end subroutine fixCells

  !> Add a radial grid line to a grid region, starting at the face
  !> given by the points (fcFromX,fcFromY) and (fcToX,fcToY).
  !> The new radial grid line should go through the the point (px, py).
  !> (This point has to be positioned exactly on a  poloidally/x-aligned face).
  !> If recomputeIntersection is true, (px,py) will be recomputed as the intersection
  !> of the face with a structure. 
  !> The region index iReg in which the face is located has to be given. 
  !> Optionally, the indices of the face (ip,ir) can be given, where
  !> (iFcP,iFcR) is the left point (start point) of the face. 
  recursive subroutine addRadialLine( equ, struct, grid, &
       & iReg, sepSegUpdated, &
       & px, py, &
       & recomputeIntersection, &
       & iFcR, iRegOrigin )
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(in) :: struct
    type(CarreGrid), intent(inout) :: grid
    integer, intent(in) :: iReg
    logical, dimension(nsepsegmx), intent(inout) :: sepSegUpdated
    double precision, intent(in) :: px, py
    logical, intent(in), optional :: recomputeIntersection
    integer, intent(in), optional :: iFcR, iRegOrigin

    ! internal
    integer :: ir
    integer :: liFcP, liFcR  ! local copies of dummy arguments iFcP, iFcR
    integer :: iOtherFcP, iOtherFcR

    double precision :: llx(npnimx), lly(npnimx)
    integer :: llNp, iRegOther, i

    logical :: doesIntersect, alignment, regionHasFace
    double precision :: newPx, newPy

    integer :: iSurface

    ! Find face to split
    call findFaceForPoint(grid, px, py, iReg, liFcP, liFcR, alignment, &
         & doPoloidal = .true., doRadial = .false., iRad = iFcR )
    ! we expect a poloidal face
    call assert( alignment )
    ! ...and if requested, on the given radial line
    if (present(iFcR)) call assert( liFcR == iFcR )

    ! Find poloidal level line going through the poloidally aligned face
    call findLevelLineForPoints( equ, &
         & grid%xmail(liFcP, liFcR, iReg), grid%ymail(liFcP, liFcR, iReg), &
         & grid%xmail(liFcP+1, liFcR, iReg), grid%ymail(liFcP+1, liFcR, iReg), &
         & llX, llY, llNp )

    ! If required, recompute intersection of the poloidal level line with the structure
    ! to guarantee the new point lies on the level line (this is only required for
    ! the face where the new radial line was emitted from).
    if (recomputeIntersection) then
        call intersect_all_structures(llX(1:llNp), llY(1:llNp), struct, &
             & doesIntersect, &
             & ipx=newPx, ipy=newPy, &
             & oldipx=px, oldipy=py)
        if (.not. doesIntersect) then
            ! This can happen if multiple radial lines are added in one cell of the original grid,
            ! and the geometry changed such that the intersection vanishes.
            call logmsg(LOGDEBUG,  'addRadialLine: did not find intersection of face with a structure!&
                 & Skipping this radial line.')
            return
        end if
        call logmsg(LOGDEBUGBULK,  'addRadialLine: old intersection '//real2str(px)//', '//real2str(py)//&
             &', new intersection '//real2str(newPx)//', '//real2str(newPy))
    else
        newPx = px
        newPy = py
    end if

    ! Add space for new grid points. Shift liFcP + 1 : end up by one
    ! TODO: test that we don't run out of space   
    grid%xmail(liFcP + 2 : grid%np1(iReg) + 1, :, iReg) = &
         & grid%xmail(liFcP + 1 : grid%np1(iReg), :, iReg)
    grid%ymail(liFcP + 2 : grid%np1(iReg) + 1, :, iReg) = &
         & grid%ymail(liFcP + 1 : grid%np1(iReg), :, iReg)
    grid%np1(iReg) = grid%np1(iReg) + 1

    ! Same for radial line flags
    grid%lineFlagRad(liFcP + 2 : grid%np1(iReg) + 1, iReg) = &
         & grid%lineFlagRad(liFcP + 1 : grid%np1(iReg), iReg)
    ! Flag for new radial line is then set to required below

    ! copy liFcP notes to liFcP + 1 nodes to have something sensible for plotting
    grid%xmail(liFcP + 1, :, iReg) = grid%xmail(liFcP, :, iReg)
    grid%ymail(liFcP + 1, :, iReg) = grid%ymail(liFcP, :, iReg)

    ! Place given point on reference grid line
    grid%xmail(liFcP + 1, liFcR, iReg) = newPx
    grid%ymail(liFcP + 1, liFcR, iReg) = newPy

    ! Update the number of points on the separatrix segment associated with the radial
    ! cell strip we are refining.
    if ( .not. sepSegUpdated(grid%radLineSepSeg(liFcP,iReg)) ) then
        grid%nptseg(grid%radLineSepSeg(liFcP,iReg)) = grid%nptseg(grid%radLineSepSeg(liFcP,iReg)) + 1
        sepSegUpdated(grid%radLineSepSeg(liFcP,iReg)) = .true.
    end if

    ! ...and set pointer to separatrix segment this line emanates from
    grid%radLineSepSeg(liFcP + 2 : grid%np1(iReg) + 1, iReg) = &
         & grid%radLineSepSeg(liFcP + 1 : grid%np1(iReg), iReg)
    grid%radLineSepSeg(liFcP + 1, iReg) = grid%radLineSepSeg(liFcP, iReg)

#ifdef USE_SILO
    ! Debug output
    if (DEBUGFILES_ADDRADIALLINE .and. recomputeIntersection) then
        call csioSetSurface(0)
        call csioSetRelax(0)
        call csioOpenFile()
        call siloWriteLineSegmentGridFromPoints( csioDbfile, "refLine", llX(1:llNp), lly(1:llNp) )    
        call siloWriteQuadGrid( csioDbfile, "region", &
             & 3, grid%nr(iReg), &
             & grid%xmail(liFcP:liFcP+2, 1:grid%nr(iReg), iReg), &
             & grid%ymail(liFcP:liFcP+2, 1:grid%nr(iReg), iReg) )
        call siloWriteQuadGrid( csioDbfile, "oldIntersection", &
             & 3, 3, siloFakeGridForPointX(px), siloFakeGridForPointY(py) )
        call siloWriteQuadGrid( csioDbfile, "newIntersection", &
             & 3, 3, siloFakeGridForPointX(newPx), siloFakeGridForPointY(newPy) )
        call csioCloseFile()
    end if
#endif

    ! For this region iReg: compute new radial points by moving
    ! away from the given point in both directions

    ! Positive direction
    iSurface = 0
    do ir = lIFcR + 1, grid%nr(iReg)       
        iSurface = iSurface + 1
        call csioSetSurface(delta=1)
        call insertPoints( equ, &
             & grid%xmail(liFcP:liFcP+2, ir-1, iReg), &
             & grid%ymail(liFcP:liFcP+2, ir-1, iReg), &
             & grid%xmail(liFcP:liFcP+2, ir, iReg), &
             & grid%ymail(liFcP:liFcP+2, ir, iReg) )
    end do

    ! Negative direction
    do ir = lIFcR - 1, 1, -1 
        iSurface = iSurface + 1
        call csioSetSurface(delta=1)
        call insertPoints( equ, &
             & grid%xmail(liFcP:liFcP+2, ir + 1, iReg), &
             & grid%ymail(liFcP:liFcP+2, ir + 1, iReg), &
             & grid%xmail(liFcP:liFcP+2, ir, iReg), &
             & grid%ymail(liFcP:liFcP+2, ir, iReg) )
    end do

#ifdef USE_SILO
    ! Debug output: entire grid
    if (DEBUGFILES_ADDRADIALLINE) then
        call csioSetSurface(delta = 1)
        call csioSetRelax(0)
        call csioOpenFile()
        do i = 1, grid%nreg
            call siloWriteQuadGrid( csioDbfile, 'region'//int2str(i), &
                 & grid%np1(i), grid%nr(i), &
                 & grid%xmail(1:grid%np1(i), 1:grid%nr(i), i), &
                 & grid%ymail(1:grid%np1(i), 1:grid%nr(i), i) )
        end do
        call csioCloseFile()    
    end if
#endif   

    ! For points on boundary of region, insert
    ! radial lines starting at this point in all other regions
    ! -> recursive calls to addRadialLine, without recomputing structure intersection
    do iRegOther = 1, grid%nreg
        if (iRegOther == iReg) cycle
        if (present(iRegOrigin)) then
            if (iRegOther == iRegOrigin) cycle
        end if

        ! Boundary face (liFcP,1) -> (liFcP+2,1)
        ! Is this face in the other region?
        call findFaceInRegion( grid, iRegOther, &
             & grid%xmail(liFcP, 1, iReg), &
             & grid%ymail(liFcP, 1, iReg), &
             & grid%xmail(liFcP + 2, 1, iReg), &
             & grid%ymail(liFcP + 2, 1, iReg), &
             & iFcP=iOtherFcP, iFcR=iOtherFcR, regionHasFace = regionHasFace )

        if (regionHasFace) then
            call logmsg(LOGDEBUGBULK,  'addRadialLine: region '//int2str(iReg)&
                 &//' extending into region '//int2str(iRegOther) )
            call addRadialLine( equ, struct, grid, &
                 & iRegOther, sepSegUpdated, &
                 & grid%xmail(liFcP+1, 1, iReg), &
                 & grid%ymail(liFcP+1, 1, iReg), &
                 & recomputeIntersection = .false., &
                 & iFcR = iOtherFcR, iRegOrigin = iReg ) 
        end if

        ! Boundary face (liFcP,nrMax) -> (liFcP+2,nrMax)
        ! Is this face in the other region?
        call findFaceInRegion( grid, iRegOther, &
             & grid%xmail(liFcP, grid%nr(iReg), iReg), &
             & grid%ymail(liFcP, grid%nr(iReg), iReg), &
             & grid%xmail(liFcP + 2, grid%nr(iReg), iReg), &
             & grid%ymail(liFcP + 2, grid%nr(iReg), iReg), &
             & iFcP=iOtherFcP, iFcR=iOtherFcR, regionHasFace = regionHasFace )

        if (regionHasFace) then
            call logmsg(LOGDEBUGBULK,  'addRadialLine: region '//int2str(iReg)&
                 &//' extending into region '//int2str(iRegOther) )
            call addRadialLine( equ, struct, grid, &
                 & iRegOther, sepSegUpdated, &
                 & grid%xmail(liFcP+1, grid%nr(iReg), iReg), &
                 & grid%ymail(liFcP+1, grid%nr(iReg), iReg), &
                 & recomputeIntersection = .false., &
                 & iFcR = iOtherFcR, iRegOrigin = iReg ) 
        end if
    end do

#ifdef USE_SILO
    ! Debug output: entire region grid
    if (DEBUGFILES_ADDRADIALLINE) then
        call csioSetSurface(delta = 1)
        call csioSetRelax(0)
        call csioOpenFile()
        do i = 1, grid%nreg
            call siloWriteQuadGrid( csioDbfile, 'region'//int2str(i), &
                 & grid%np1(i), grid%nr(i), &
                 & grid%xmail(1:grid%np1(i), 1:grid%nr(i), i), &
                 & grid%ymail(1:grid%np1(i), 1:grid%nr(i), i) )
        end do
        call csioCloseFile()    
    end if
#endif

    ! Mark the new radial line segment as required
    grid%lineFlagRad(liFcP + 1, iReg) = GRIDLINE_REQUIRED

  end subroutine addRadialLine


  !> Starting from a given (reference) poloidal grid line, place points on the
  !> adjacent grid line. On the reference grid line, all points must be given.
  !> On the adjacent grid line, the first and last point must be given
  !> The points are distributed using the relaxation scheme for the criterion set.
  subroutine insertPoints(equ, refx, refy, newx, newy)
    type(CarreEquilibrium), intent(in) :: equ
    double precision, intent(in) :: refx(:), refy(:)
    double precision, intent(out) :: newx(size(refx)), newy(size(refy))

    ! internal
    double precision :: nivRefX(npnimx), nivRefY(npnimx)
    double precision :: nivNewX(npnimx), nivNewY(npnimx)
    integer :: npNivRef, npNivNew

    ! coordinates of points on reference and new grid line
    ! (in distance from beginning of niveau line)
    double precision :: lRef(npnimx), lNew(npnimx), lNewModified(npnimx)    

    double precision :: lengthRef, lengthNew, curDist
    double precision :: lPasmin, lTgarde
    integer :: ipol, i

    ! Criteria values
    double precision, dimension(size(refx)) :: critNew, critModified

    ! debug plotting
    double precision :: tmpMailx(size(refx),2), tmpMaily(size(refy),2)

    ! externals
    double precision :: long, ruban
    external :: long, ruban

    ! some sanity checks
    call assert(size(refx) == size(refy))

    ! We need the level lines for both sections of poloidal grid lines
    call findLevelLineForPoints( equ, &
         & refx(1), refy(1), refx(size(refx)), refy(size(refx)), &
         & nivRefX, nivRefY, npNivRef )
    call findLevelLineForPoints( equ, &
         & newx(1), newy(1), newx(size(newx)), newy(size(newx)), &
         & nivNewX, nivNewY, npNivNew )

    !..On definit maintenant les points de maille de la nouvelle ligne
    !  de niveau a partir de ceux de la precedente.
    !..We now place the grid points of the new grid line according
    !  to the spacing of the grid points on the reference grid line

    lengthRef=long(nivRefX(1:npNivRef),nivRefY(1:npNivRef),npNivRef)
    lengthNew=long(nivNewX(1:npNivNew),nivNewY(1:npNivNew),npNivNew)

    ! Compute coordinates (distance to first point) of given points on reference line
    ! First point is at zero
    lRef(1) = 0d0
    ! for all other points we compute the distance
    curDist = 0d0
    do ipol = 2, size(refx) - 1
        ! Giving curDist as a parameter is a safety check that we don't get a 
        ! point postioned "before", i.e. closer to the start point than the previous one
        lRef(iPol) = ruban(nivRefX(1:npNivRef), nivRefY(1:npNivRef), npNivRef,&
             & refx(ipol), refy(ipol), curDist)       
        curDist = lRef(iPol)
        call assert(lRef(iPol) <= lengthRef, "insertPoints: length computation broken" )
    end do
    ! Last point is at end of reference line
    lRef(size(newx)) = lengthRef 

    ! Place an initial distribution of points on the new grid line
    lNew(1) = 0d0
    do ipol = 2, size(newx) - 1
        lNew(ipol)=(lRef(ipol)/lengthRef)*lengthNew
        CALL COORD( nivNewX(1:npNivNew), nivNewY(1:npNivNew), npNivNew, &
             & lNew(ipol), newx(ipol), newy(ipol) )
    enddo
    lNew(size(newx)) = lengthNew
    ! (first and last point in nivNewX must already be set correctly...)

#ifdef USE_SILO
    ! Write out intermediate grid stage
    if (DEBUGFILES_INSERTPOINTS) then
        call csioSetRelax( 0 )

        call csioOpenFile()
        tmpMailx(:,1) = refx
        tmpMailx(:,2) = newx
        tmpMaily(:,1) = refy
        tmpMaily(:,2) = newy
        call siloWriteLineSegmentGridFromPoints( csioDbfile, "refLine", nivRefX(1:npNivRef), nivRefY(1:npNivRef) )
        call siloWriteLineSegmentGridFromPoints( csioDbfile, "newLine", nivNewX(1:npNivNew), nivNewY(1:npNivNew) )
        call siloWriteQuadGrid( csioDbfile, "region", &
             & size(newx), 2, &
             & tmpMailx, tmpMaily )            
        call csioCloseFile()
    end if
#endif

    ! Optimize distribution of grid points according to the criteria,
    ! using the signed relaxation method 

    !nrelax = 0
    ! nrelax is coming from COMRLX.F. It's not a constant, can be modified by user I/O.
    if(nrelax.gt.0) then
        ! 2.   on initialise la fonction qui doit s'annuler pour une
        !      distribution orthogonale
        ! We compute the grid quality criterion for the initiali point distribution.
        ! Like nrelax, pasmin, l0 and l1 come from the COMRLX common block
        ! The two zeros in the clort call are the guard lengths. We effectively 
        ! disable the "proportional distribution" criterion here.
        !lPasmin = pasmin * 1e-1
        !lPasmin = 1e-4
        lPasmin = 5e-4
        lTgarde = 1e-1
        call clort( refx, refy,&
             & newx, newy, &
             & critNew, size(refx), &
             & lPasmin, lTgarde, lTgarde, lRef, lNew )

        !  3. on procede a un premier deplacement des noeuds
        !     We relax the node position one time by moving them slightly in the right direction
        lNewModified(1)=lNew(1)
        lNewModified(size(newx))=lNew(size(newx))
        !diag%segt(ir,ireg)=l2(nppol)
        do ipol=2, size(newx) - 1
            if(critNew(ipol).gt.0d0) then
                lNewModified(ipol)=0.9*lNew(ipol)+0.1*lNew(ipol+1)
            else
                lNewModified(ipol)=0.9*lNew(ipol)+0.1*lNew(ipol-1)
            endif
            call coord(nivNewX(1:npNivNew), nivNewY(1:npNivNew),&
                 & npNivNew,lNewModified(iPol), & 
                 & newx(ipol),newy(ipol) )

            !diag%somort(ir,ireg)= diag%somort(ir,ireg)+(critNew(ipol)/nppol)
            !diag%somortpur(ir,ireg)= diag%somortpur(ir,ireg)+(ortpur(ipol)/nppol)
            !diag%sompropo(ir,ireg)= diag%sompropo(ir,ireg)+(propo(ipol)/nppol)
            !diag%somvarr(ir,ireg)= diag%somvarr(ir,ireg)+(varr(ipol)/nppol)
        enddo

        ! 4. on relaxe les points de facon iterative pour realiser la
        !    meilleure orthogonalite possible
        !    We relax the node positions to get the best criteria match possible 
        do i=1,nrelax

            call clort( refx, refy,&
                 & newx, newy, &
                 & critModified, size(refx), &
                 & lPasmin, lTgarde, lTgarde, lRef, lNewModified )


            do ipol=2, size(newx) - 1
                if(abs(critModified(ipol)).gt.rlcept) then
                    ! criterion for current node too big: move node
                    del=-critModified(ipol)*(lNewModified(ipol)-lNew(ipol)) & 
                         & /(critModified(ipol)-critNew(ipol))
                    if(del.gt.0d0) then
                        del=min(del,relax*(lNewModified(ipol+1)-lNewModified(ipol)))
                    else
                        del=max(del,relax*(lNewModified(ipol-1)-lNewModified(ipol)))
                    endif
                    if(del.ne.0d0) then
                        lNew(ipol)=lNewModified(ipol)
                        critNew(ipol)=critModified(ipol)
                        lNewModified(ipol)=lNewModified(ipol)+del
                    endif
                    call coord(nivNewX(1:npNivNew), nivNewY(1:npNivNew),&
                         & npNivNew,lNewModified(iPol), & 
                         & newx(ipol),newy(ipol) )

                endif
            enddo

#ifdef USE_SILO
            if (DEBUGFILES_INSERTPOINTS) then
                call csioSetRelax( i )
                call csioOpenFile()
                tmpMailx(:,1) = refx
                tmpMailx(:,2) = newx
                tmpMaily(:,1) = refy
                tmpMaily(:,2) = newy
                call siloWriteLineSegmentGridFromPoints( csioDbfile, "refLine", nivRefX(1:npNivRef), nivRefY(1:npNivRef) )
                call siloWriteLineSegmentGridFromPoints( csioDbfile, "newLine", nivNewX(1:npNivNew), nivNewY(1:npNivNew) )
                call siloWriteQuadGrid( csioDbfile, "region", &
                     & size(newx), 2, &
                     & tmpMailx, tmpMaily )            
                call csioCloseFile()
            end if
#endif

            if(maxval(abs(critNew)) <= rlcept) exit
        enddo

        if(ortmax > rlcept) then
            ! The relaxation failed to produce good results with the
            ! given number of iterations
        end if

    endif
  end subroutine insertPoints

  !> Coarsen grid by removing excessively fine rows of grid cells
  subroutine coarsenCells(grid)
    type(CarreGrid), intent(inout) :: grid  

    ! internal
    integer :: iReg, ip, ir, np
    logical :: removeRadialLine(npmamx,nregmx), removeable
    logical :: sepSegsDelta(nsepsegmx)
    

    ! Figure out what radial grid lines have to be removed
    ! to remove excessively fine cells
    ! Also compute how many points are removed from every separatrix segment by this
    removeRadialLine = .false.
    sepSegsDelta = 0
    do iReg = 1, grid%nreg
        do ip = 1, grid%np1(iReg) - 1
            do ir = 1, grid%nr(iReg) - 1

                if ( ( grid%cellflag(ip, ir, iReg) == GRID_BOUNDARY_COARSEN ) .or. &
                     & ( grid%cellflag(ip, ir, iReg) == GRID_INTERNAL_COARSEN ) ) then

                    call markRadialLine(ip, iReg, removeable)
                    if (.not. removeable) call markRadialLine(ip + 1, iReg, removeable)
                    if (.not. removeable) then
                        ! Found no radial line that can be removed to coarsen this radial cell strip
                        !call logmsg(LOGWARNING, "coarsenCells: cannot coarsen cell")
                    end if
                end if

            end do
        end do
        call logmsg(LOGDEBUG, "coarsenCells: region "//int2str(iReg)//": can remove " &
             & //int2str(count(removeRadialLine(:, iReg) == .true.))//" lines.")
    end do

    ! Remove lines region by region
    do iReg = 1, grid%nreg
        np = 0
        do ip = 1, grid%np1(iReg)
            if (.not. removeRadialLine(ip, iReg)) then
                np = np + 1
                grid%xmail(np, :, iReg) = grid%xmail(ip, :, iReg)
                grid%ymail(np, :, iReg) = grid%ymail(ip, :, iReg)
                grid%lineFlagRad(np, iReg) = grid%lineFlagRad(ip, iReg)
                grid%radLineSepSeg(np, iReg) = grid%radLineSepSeg(ip, iReg)
            end if
        end do
        grid%np1(iReg) = np
    end do

    ! update number of points on separatrix segments
    grid%nptseg = grid%nptseg - sepSegsDelta

  contains

    ! Check whether the radial line going through ir, ireg can be removed, and 
    ! if yes, mark it to be removed in array removeRadialLine for all regions.
    ! Also note the indices of the separatrix segments to update
    subroutine markRadialLine(ip, iReg, removeable) 
      integer, intent(in) :: ip, ireg
      logical, intent(out) :: removeable

      ! internal
      integer :: iContinue, iRegOther, ipOther, irOther, i
      integer :: ipRegions(grid%nreg)
      double precision :: xBnd, yBnd
      logical :: updateSegSegs(nsepsegmx)


      logical, dimension(grid%nreg) :: inRegion
      integer, dimension(grid%nreg) :: iPolRegion


      removeable = .false.
      ipRegions = GRID_UNDEFINED

      ! already marked for removal?
      if ( removeRadialLine(ip, ireg) ) return

      ! trace radial line through regions
      call followRadialLine(grid, iReg, ip, inRegion, iPolRegion) 
     
      ! is it removeable?
      removeable = .true.
      do i = 1, grid%nreg
          if (.not. inRegion(i)) cycle
          if ( grid%lineFlagRad(iPolRegion(i), i) == GRIDLINE_REQUIRED ) then
              removeable = .false.
              exit
          end if
      end do

      ! if removeable, mark for removal and do bookkeeping
      if (removeable) then

          updateSegSegs = .false.
          do i = 1, grid%nreg
              if (inRegion(i)) then 
                  removeRadialLine(iPolRegion(i), i) = .true.
                  updateSegSegs( grid%radLineSepSeg(iPolRegion(i), i) ) = .true.
              end if
          end do
          
          ! Update point deltas for separatrix segments touched by removing this radial line
          do i = 1, nsepsegmx
              if ( updateSegSegs(i) ) sepSegsDelta(i) = sepSegsDelta(i) + 1
          end do
      end if

    end subroutine markRadialLine

  end subroutine coarsenCells

  ! Follow a radial line, note through which regions it passes 
  ! and note the poloidal positions for every region
  recursive subroutine followRadialLine(grid, iReg, iPol, inRegion, iPolRegion, regionVisited) 
    type(CarreGrid), intent(in) :: grid
    integer, intent(in) :: iReg, iPol
    logical, intent(inout), dimension(grid%nreg) :: inRegion
    integer, intent(inout), dimension(grid%nreg) :: iPolRegion
    logical, intent(inout), dimension(grid%nreg), optional :: regionVisited

    ! internal
    integer :: iRegOther, iContinue, iPolOther, iRadOther
    logical :: lRegionVisited(grid%nreg)
    double precision :: xBnd, yBnd

    ! Set up tracking for visited regions at top of recursion
    if (present(regionVisited)) then 
        lRegionVisited = regionVisited
    else
        lRegionVisited = .false.
        inRegion = .false.
        iPolRegion = GRID_UNDEFINED
    end if

    if (lregionVisited(iReg)) return

    inRegion(iReg) = .true.
    iPolRegion(iReg) = iPol
    lregionVisited(iReg) = .true.

    ! continuation in other regions
    do iContinue = 1, 2
        select case (iContinue)
        case(1)
            xBnd = grid%xmail(iPol, 1, iReg)
            yBnd = grid%ymail(iPol, 1, iReg)
        case(2)
            xBnd = grid%xmail(iPol, grid%nr(iReg), iReg)
            yBnd = grid%ymail(iPol, grid%nr(iReg), iReg)
        end select
        
        do iRegOther = 1, grid%nreg
            if (iRegOther == iReg) cycle
            call findPointInRegion(grid, iRegOther, xBnd, yBnd, iPolOther, iRadOther)
            if (iPolOther /= GRID_UNDEFINED) then
                call followRadialLine(grid, iRegOther, iPolOther, inRegion, iPolRegion, lRegionVisited)
            end if
        end do
    end do

    if (present(regionVisited)) then 
        regionVisited = lRegionVisited
    end if

  end subroutine followRadialLine
    




  subroutine finalizeCells(grid)
    type(CarreGrid), intent(inout) :: grid

    ! internal
    integer :: iReg, ir, ip, iFace, ip2, ir2
    integer :: nExt

    integer :: iPass, dx, dy, ipFace, irFace, nInt, ipl, irl, ipFix, irFix, ipNb, irNb
    logical :: pointMoved, pointOk

    ! For all intersected faces with an internal point on one and
    ! an external point on the other side, move the external point
    ! to the structure intersection

    ! Consider every external point. Look at all faces this point is part of,
    ! and consider the other end point.
    ! First pass: if the other endpoint is an internal point, they are connect
    ! by an intersected face. Move point onto the intersection.
    ! Second pass: if the other endpoint is a boundary point, move the point onto it.
    ! Only move every point once.

    ! TODO: make sure we remove north/south boundary condition if possible

    do iReg = 1, grid%nReg
        do ip = 1, grid%np1(iReg)
            do ir = 1, grid%nr(iReg)

                if (grid%pointFlag(ip, ir, iReg) /= GRID_EXTERNAL) cycle

                ! apply correction strategies one after another
                pointMoved = .false.
                do iPass = 1, 2

                    ! loop over all neighbour points = connected faces
                    do dx = -1, 1
                        do dy = -1, 1
                            ! make sure the offset we use makes sense
                            if ( (abs(dx) + abs(dy)) /= 1) cycle

                            ! Compute indices of neighbour point
                            ip2 = ip + dx
                            ir2 = ir + dy                    

                            ! make sure neighbour point is in region
                            if ( (ip2 < 1) .or. (ip2 > grid%np1(iReg)) ) cycle
                            if ( (ir2 < 1) .or. (ir2 > grid%nr(iReg)) ) cycle

                            ! figure out indices and type of face this point is on
                            ipFace = ip + min(dx, 0)
                            irFace = ir + min(dy, 0)
                            if (dx /= 0) iFace = FACE_POLOIDAL
                            if (dy /= 0) iFace = FACE_RADIAL

                            ! First pass: move onto intersection
                            if ( (iPass==1) .and. &
                                 & (grid%pointFlag(ip2, ir2, iReg) == GRID_INTERNAL) ) then

                                ! external and internal point: 
                                ! move external point onto intersection (in all regions!)                         
                                call movePoint( grid%xmail(ip,ir,iReg), grid%ymail(ip,ir,iReg), &
                                     & grid%faceISecPx(iFace,ipFace,irFace,iReg), grid%faceISecPy(iFace,ipFace,irFace,iReg) )
                                pointMoved = .true.
                                exit
                            end if

                            ! Second pass: move onto boundary point
                            if ( (iPass==2) .and. &
                                 & (grid%pointFlag(ip2, ir2, iReg) == GRID_BOUNDARY) ) then

                                ! external and boundary point: 
                                ! move external point onto boundary point (-> triangle cell)
                                call movePoint( grid%xmail(ip,ir,iReg), grid%ymail(ip,ir,iReg), &
                                     & grid%xmail(ip2,ir2,iReg), grid%ymail(ip2,ir2,iReg) )
                                pointMoved = .true.
                                exit
                            end if
                        end do ! dy loop

                        if (pointMoved) exit
                    end do ! dx loop

                    if (pointMoved) exit
                end do ! strategy loop

            end do
        end do
    end do

    ! Now catch special case of internal cells with three external points and one internal point
    ! (no boundary point). Make sure the external point not connected to the internal 
    ! point via a face is placed on one of the other external points. If this is not 
    ! the case, move it to the external neighbour along the radial face.

    do iReg = 1, grid%nReg
        do ip = 1, grid%np1(iReg) - 1
            do ir = 1, grid%nr(iReg) - 1

                ! we are interested in internal cells
                if (grid%cellflag(ip, ir, iReg) == GRID_EXTERNAL) cycle

                ! ...with 1 internal and 3 external points
                nInt = count( grid%pointflag(ip:ip+1, ir:ir+1, iReg) == GRID_INTERNAL )
                nExt = count( grid%pointflag(ip:ip+1, ir:ir+1, iReg) == GRID_EXTERNAL )
                if (.not. ((nInt == 1) .and. (nExt == 3)) ) cycle

                call logmsg(LOGDEBUG,  'finalizeCells: candidate cell '//int2str(ip)&
                     &//', '//int2str(ir)//', '//int2str(iReg) )

                ! find the internal point
                ipFix = GRID_UNDEFINED
                irFix = GRID_UNDEFINED
                do ipL = 0, 1
                    do irL = 0, 1
                        if ( grid%pointflag(ip + ipL, ir + irL, iReg) == GRID_INTERNAL ) then
                            ipFix = ip + ipL
                            irFix = ir + irL
                        end if
                    end do
                end do
                call assert( ipFix /= GRID_UNDEFINED )

                ! the external point we are interested in is on the opposite corner
                ipFix = ipFix + 1
                irFix = irFix + 1
                if (ipFix > ip+1) ipFix = ip
                if (irFix > ir+1) irFix = ir

                ! Check whether the point is already positioned on another corner
                pointOk = .false.

                ! compare with neighbour in poloidal direction
                ipNb = ipFix + 1
                irNb = irFix
                if (ipNb > ip+1) ipNb = ip
                pointOk = pointOk .or. pointsIdentical( &
                     & grid%xmail(ipFix,irFix,iReg), grid%ymail(ipFix,irFix,iReg), &
                     & grid%xmail(ipNb,irNb,iReg), grid%ymail(ipNb,irNb,iReg) )

                ! compare with neighbour in radial direction
                ipNb = ipFix
                irNb = irFix + 1
                if (irNb > ir+1) irNb = ir
                pointOk = pointOk .or. pointsIdentical( &
                     & grid%xmail(ipFix,irFix,iReg), grid%ymail(ipFix,irFix,iReg), &
                     & grid%xmail(ipNb,irNb,iReg), grid%ymail(ipNb,irNb,iReg) )

                ! if not ok, set it to neighbour in radial direction
                if (.not. pointOk) then
                    call logmsg(LOGDEBUG,  'finalizeCells: cell '//int2str(ip)//' '//int2str(ir)&
                         &//' '//int2str(iReg)//', fixing node '//int2str(ipFix)//' '&
                         &//int2str(irFix)//' with node '//int2str(ipNb)//' '//int2str(irNb) )
                    call movePoint( grid%xmail(ipFix,irFix,iReg), grid%ymail(ipFix,irFix,iReg), &
                         & grid%xmail(ipNb,irNb,iReg), grid%ymail(ipNb,irNb,iReg) )
                end if

            end do
        end do
    end do

  contains

    subroutine movePoint( xFrom, yFrom, xTo, yTo )
      double precision, intent(in) :: xFrom, yFrom, xTo, yTo

      ! internal
      integer :: iReg, ip, ir

      do iReg = 1, grid%nreg
          ip = GRID_UNDEFINED
          call findPointInRegion(grid, iReg, xFrom, yFrom, ip, ir)
          ! if no point found, go to next region
          if (ip == GRID_UNDEFINED) cycle
          grid%xmail(ip, ir, iReg) = xTo
          grid%ymail(ip, ir, iReg) = yTo
      end do

    end subroutine movePoint


  end subroutine finalizeCells



  !> Find the face in region iReg to which the given point (px, py) is closest.
  !> The starting point of the face (lowest in dex in poloidal and radial direction)
  !> is returned as (liFcP, liFcR). The alignment of the face is returned as the
  !> flag alignment, where .true. means the poloidal face and .false. the radial face.
  !> 
  !> By default, both radial and poloidal faces are considered. One can override
  !> this using the optional flags doPoloidal and doRadial.
  !> 
  !> The optional parameter iRad can be used to constrain the search to one radial coordinate.
  subroutine findFaceForPoint(grid, px, py, iReg, liFcP, liFcR, alignment, doPoloidal, doRadial, iRad)
    type(CarreGrid), intent(in) :: grid
    double precision, intent(in) :: px, py
    integer, intent(in) :: iReg
    integer, intent(out) :: liFcP, liFcR
    logical, intent(out) :: alignment
    logical, intent(in), optional :: doPoloidal, doRadial
    integer, intent(in), optional :: iRad

    ! internal
    logical :: lDoPoloidal = .true., lDoRadial = .true.   
    integer :: ip, ir, irFrom, irTo
    double precision :: dist, minDist

    if (present(doPoloidal)) lDoPoloidal = doPoloidal
    if (present(doRadial)) lDoRadial = doRadial

    liFcP = GRID_UNDEFINED
    liFcR = GRID_UNDEFINED
    alignment = .true.

    minDist = huge(minDist)


    irFrom = 1
    irTo = grid%nr(iReg)
    if (present(iRad)) then
        irFrom = iRad
        irTo = iRad
    end if

    do ip = 1, grid%np1(iReg) 
        do ir = irFrom, irTo

            ! compute distance to poloidal face from (ip,ir) -> (ip+1,ir)
            if (lDoPoloidal .and. (ip < grid%np1(iReg))) then            
                dist = normFaceDist( grid%xmail(ip, ir,iReg), grid%ymail(ip,ir,iReg), &
                     & grid%xmail(ip+1,ir,iReg), grid%ymail(ip+1,ir,iReg), px, py )
                if ( dist < minDist ) then
                    minDist = dist
                    liFcP = ip
                    liFcR = ir
                    alignment = .true. ! poloidal face
                end if
            end if

            ! compute distance to radial face from (ip,ir) -> (ip,ir+1)
            if (lDoRadial .and. (ir < grid%nr(iReg))) then
                dist = normFaceDist( grid%xmail(ip, ir,iReg), grid%ymail(ip,ir,iReg), &
                     & grid%xmail(ip,ir+1,iReg), grid%ymail(ip,ir+1,iReg), px, py )
                if ( dist < minDist ) then
                    minDist = dist
                    liFcP = ip
                    liFcR = ir
                    alignment = .false. ! radial face
                end if
            end if

        end do
    end do

    call assert( (liFcP /= GRID_UNDEFINED) .and. (liFcR /= GRID_UNDEFINED), &
         & "findFaceForPoint: did not find a face close to the given point - something is broken." )

  contains

    ! Compute a normalized measure of point-face distance for the purpose
    ! of finding the face closest to a point. This is not a proper distance
    ! measure! Just a simple thing that seems to work, at least for the
    ! assumption that the (px,py) is really close to the face we are looking
    ! for, compared to all other faces.
    double precision function normFaceDist(fx1, fy1, fx2, fy2, px, py)      
      double precision, intent(in) :: fx1, fy1, fx2, fy2, px, py

      double precision :: dist
      external :: dist

      normFaceDist = ( dist(fx1, fy1, px, py) + dist(px, py, fx2, fy2) ) &
           & / dist( fx1, fy1, fx2, fy2 )

    end function normFaceDist

  end subroutine findFaceForPoint

  !> Check whether a given face is part of a region, and return information
  !> about its location and alignment.
  !> Alignment, as usual: .true. = poloidal, .false. = radial
  subroutine findFaceInRegion( grid, iReg, &
       & xFrom, yFrom, xTo, yTo, &
       & iFcP, iFcR, regionHasFace, alignment )

    type(CarreGrid), intent(in) :: grid
    integer, intent(in) :: iReg
    double precision, intent(in) :: xFrom, yFrom, xTo, yTo
    integer, intent(out), optional :: iFcP, iFcR
    logical, intent(out), optional :: regionHasFace, alignment    

    ! internal
    integer :: ip, ir, ipTo, irTo, iAlign
    logical :: match, lAlignment

    do ip = 1, grid%np1(iReg)
        do ir = 1, grid%nr(iReg)

            do iAlign = 1, 2

                select case (iAlign)
                case(1) ! poloidal direction                
                    if (ip == grid%np1(iReg)) cycle
                    ipTo = ip + 1
                    irTo = ir
                    lAlignment = .true.
                case(2) ! radial direction
                    if (ir == grid%nr(iReg)) cycle
                    ipTo = ip
                    irTo = ir + 1
                    lAlignment = .false.
                end select

                ! test face identity, accounting for switched points
                match = &
                     & ( pointsIdentical(grid%xmail(ip, ir, iReg), &
                     &             grid%ymail(ip, ir, iReg), xFrom, yFrom)&
                     & .and. &
                     &   pointsIdentical(grid%xmail(ipTo, irTo, iReg), &
                     &             grid%ymail(ipTo, irTo, iReg), xTo, yTo) ) &
                     & .or. &
                     &  ( pointsIdentical(grid%xmail(ip, ir, iReg), &
                     &              grid%ymail(ip, ir, iReg), xTo, yTo)&
                     & .and. &
                     &    pointsIdentical(grid%xmail(ipTo, irTo, iReg), &
                     &              grid%ymail(ipTo, irTo, iReg), xFrom, yFrom) )

                if (match) then
                    ! return results
                    if (present(alignment)) alignment = lAlignment
                    if (present(iFcP)) iFcP = ip
                    if (present(iFcR)) iFcR = ir
                    if (present(regionHasFace)) regionHasFace = .true.
                    return
                end if

            end do
        end do
    end do

    ! if we arrive here, nothing was found
    if (present(iFcP)) iFcP = GRID_UNDEFINED
    if (present(iFcR)) iFcR = GRID_UNDEFINED
    if (present(regionHasFace)) regionHasFace = .false.    
  end subroutine findFaceInRegion


  !> Find the indices (xipol, xirad) of a given point (x,y) in the region iReg of the grid.
  !> If no point is found, GRID_UNDEFINED is returned for the indices.
  !> If the optional return arguments xipol2, xirad2 are given, a possible second occurrence of the point
  !> in the region is also returned.
  subroutine findPointInRegion(grid, iReg, x, y, xipol, xirad, xipol2, xirad2)
    type(CarreGrid), intent(in) :: grid
    integer, intent(in) :: iReg
    double precision, intent(in) :: x, y
    integer, intent(out) :: xipol, xirad
    integer, intent(out), optional :: xipol2, xirad2

    ! internal
    integer :: ipol, irad, nfound
    double precision, parameter :: POINT_TOL = 1e-4
    real*8 :: dist
    external dist

    xipol = GRID_UNDEFINED
    xirad = GRID_UNDEFINED
    if (present(xipol2)) xipol2 = GRID_UNDEFINED
    if (present(xirad2)) xirad2 = GRID_UNDEFINED

    ! search point in this region
    do iPol = 1, grid%np1(iReg)
        do iRad = 1, grid%nr(iReg)

            if ( pointsIdentical( x, y, &
                 & grid%xmail(iPol, iRad, iReg), &
                 & grid%ymail(iPol, iRad, iReg) ) ) then

                ! found point, return indices
                if (xipol == GRID_UNDEFINED) then                     
                    xipol = ipol
                    xirad = irad                     
                    if (.not. present(xipol2)) return
                    if (.not. present(xirad2)) return
                else
                    xipol2 = ipol
                    xirad2 = irad
                    return
                end if
            end if

        end do
    end do

    ! Nothing found, return GRID_UNDEFINED
  end subroutine findPointInRegion


  ! Find index of of an x-point in this region
  ! Note it's possible an x-point occurs multiple time, or multiple x-points occur.
  ! The first one found will be returned.
  subroutine findXPointInRegion(equ, grid, iReg, xipol, xirad)
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreGrid), intent(in) :: grid
    integer, intent(in) :: iReg
    integer, intent(out) :: xipol, xirad

    ! internal
    integer :: ipx
    double precision, parameter :: XPOINT_TOL = 1e-6
    real*8 :: dist
    external dist

    ! check all x-points
    do ipx = 1, equ%npx
        call findPointInRegion(grid, iReg, equ%ptx(ipx), equ%pty(ipx), &
             & xipol, xirad )

        if ( ( xipol /= GRID_UNDEFINED ) .and. ( xirad /= GRID_UNDEFINED ) ) then         
            call logmsg(LOGDEBUGBULK,  "findXPointInRegion: region "//int2str(iReg)//", found x-point&
                 & at "//int2str(xipol)//' '//int2str(xirad)//", position "//real2str(equ%ptx(ipx))//&
                 & ' '//real2str(equ%pty(ipx)) )
            return
        end if
    end do

    ! if we arrive here, no cell next to an x-point was found
    xipol = GRID_UNDEFINED
    xirad = GRID_UNDEFINED

  end subroutine findXPointInRegion


  !> Compute minimum distance of a point to a curve, by 
  !> computing the minimum distance to all points of the curve.
  double precision FUNCTION distanceToCurve(vx,vy,x,y) result(dmin)
    double precision, intent(in) :: vx(:), vy(:), x, y

    ! internal
    integer :: i
    double precision :: d

    ! external
    double precision :: dist
    external :: dist

    dmin = huge(dmin)

    do i = 1, size(vx)
        d = dist(x, y, vx(i), vy(i))
        dmin = min(d, dmin)
    end do

  end FUNCTION distanceToCurve


end module carre_postprocess
