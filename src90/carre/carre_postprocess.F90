module carre_postprocess

  use carre_types
  use CarreDiagnostics
  use itm_string
  use carre_niveau
  use carre_criteria
  use itm_assert

#ifdef USE_SILO
  use SiloIO
  use CarreSiloIO
#endif

  implicit none

#include <CARREDIM.F>
#include <COMRLX.F>

  private

  public carre_postprocess_computation

  ! Number of faces of a cell. 
  ! Bit positions in grid%faceflag marking that faces of a cell are intersected
  integer, parameter :: INDEX_FACE_LEFT = 1
  integer, parameter :: INDEX_FACE_BOTTOM = 2
  integer, parameter :: INDEX_FACE_RIGHT = 3
  integer, parameter :: INDEX_FACE_TOP = 4

contains

  !> Perform postprocessing on the grid as generated in carre_main.
  !> This is only relevant if the cut-cell type extended grid generation is requested.

  subroutine carre_postprocess_computation(par, equ, grid, struct)

    type(CarreParameters), intent(in) :: par
    type(CarreEquilibrium), intent(inout) :: equ
    type(CarreGrid), intent(inout) :: grid
    type(CarreStructures), intent(in) :: struct

    ! internal
    integer :: iReg, iPol, iRad, iFace
    double precision :: xx(2), yy(2)

    ! Initialize grid%nr (TODO: move this into the actual gridding routines,
    ! maille.F90. Currently not possible because they are not fully converted
    ! to the new derived types yet)
    grid % nr = par % npr

    ! Identify cells that are intersected by vessel structure

    grid%faceflag = 0
    grid%cellflag = GRID_UNDEFINED

    do iReg = 1, grid%nreg

        do iPol = 1, grid%np1(iReg) - 1
            do iRad = 1, grid%nr(iReg) - 1

                do iFace = 1, 4

                    ! fill xx, yy
                    call getFace(iFace)

                    call intersect_structure( xx, yy, &
                        & struct, grid%faceISec(iFace, iPol, iRad, iReg), &
                        & ipx = grid%faceISecPx(iFace, iPol, iRad, iReg), &
                        & ipy = grid%faceISecPy(iFace, iPol, iRad, iReg) )

                    if (grid%faceISec(iFace, iPol, iRad, iReg)) then     
                        grid%faceflag(iPol, iRad, iReg) = &
                            & ibset(grid%faceflag(iPol, iRad, iReg), iFace)
                    end if
                end do

            end do
        end do
    end do

    ! Translate the grid%faceflag array into cell flags
    ! First assume all boundary cells are problematic
    where (grid%faceflag /= GRID_UNDEFINED) grid%cellflag = GRID_BOUNDARY_REFINE
    ! Then figure out which ones are ok (the ones where opposite faces
    ! are intersected)
    where (btest(grid%faceflag, INDEX_FACE_LEFT) .and. btest(grid%faceflag, INDEX_FACE_RIGHT)) grid%cellflag = GRID_BOUNDARY
    where (btest(grid%faceflag, INDEX_FACE_TOP) .and. btest(grid%faceflag, INDEX_FACE_BOTTOM)) grid%cellflag = GRID_BOUNDARY

    ! Mark cells to be inside/outside of vessel
    call labelCells(equ, grid)

#ifdef USE_SILO
    ! write results
    call csioOpenFile('carrePostProces')
    do iReg = 1, grid%nreg
        call siloWriteQuadGrid( csioDbfile, 'region'//int2str(iReg), &
            & grid%np1(iReg), grid%nr(iReg), &
            & grid%xmail(1:grid%np1(iReg), 1:grid%nr(iReg), iReg), &
            & grid%ymail(1:grid%np1(iReg), 1:grid%nr(iReg), iReg) )

        call siloWriteQuadData( csioDbfile, 'region'//int2str(iReg), &
            & 'faceflag'//int2str(iReg), &
            & real(grid%faceflag(1:grid%np1(iReg)-1, 1:grid%nr(iReg)-1, iReg),rKind), &
            & DB_ZONECENT )
        call siloWriteQuadData( csioDbfile, 'region'//int2str(iReg), &
            & 'cellflag'//int2str(iReg), &
            & real(grid%cellflag(1:grid%np1(iReg)-1, 1:grid%nr(iReg)-1, iReg),rKind), &
            & DB_ZONECENT )
    end do
#endif

    ! Fix broken cells by modifying the grid accordingly
    call fixCells(equ, struct, grid)


  contains

    subroutine getFace(iFace)
      integer, intent(in) :: iFace

      select case (iFace)
      case(1) ! Left face of cell
          xx(1) = grid%xmail(iPol, iRad, iReg)
          yy(1) = grid%ymail(iPol, iRad, iReg)
          xx(2) = grid%xmail(iPol, iRad + 1, iReg)
          yy(2) = grid%ymail(iPol, iRad + 1, iReg)
      case(2) ! Bottom face of cell
          xx(1) = grid%xmail(iPol, iRad, iReg)
          yy(1) = grid%ymail(iPol, iRad, iReg)
          xx(2) = grid%xmail(iPol + 1, iRad, iReg)
          yy(2) = grid%ymail(iPol + 1, iRad, iReg)              
      case(3) ! Right face of cell
          xx(1) = grid%xmail(iPol + 1, iRad, iReg)
          yy(1) = grid%ymail(iPol + 1, iRad, iReg)
          xx(2) = grid%xmail(iPol + 1, iRad + 1, iReg)
          yy(2) = grid%ymail(iPol + 1, iRad + 1, iReg)
      case(4) ! Top face of cell
          xx(1) = grid%xmail(iPol, iRad + 1, iReg)
          yy(1) = grid%ymail(iPol, iRad + 1, iReg)
          xx(2) = grid%xmail(iPol + 1, iRad + 1, iReg)
          yy(2) = grid%ymail(iPol + 1, iRad + 1, iReg)
      end select

    end subroutine getFace

  end subroutine carre_postprocess_computation

  !> Compute intersection of a line segment curve with the structure elements
  subroutine intersect_structure(xx, yy, struct, doesIntersect, iStruct, &
      & iSegment, ipx, ipy)

    REAL*8, intent(in) :: xx(:),yy(:)     
    type(CarreStructures), intent(in) :: struct
    logical, intent(out) :: doesIntersect
    integer, intent(out), optional :: iSegment, iStruct
    REAL*8, intent(out), optional :: ipx, ipy

    ! internal      
    integer :: iSeg, is

    ! check for every segment in the line for intersection with every structure
    do iSeg = 1, size(xx) - 1
       do is = 1, struct%rnstruc
          call intersect(xx(iSeg:iSeg+1), yy(iSeg:iSeg+1), &
               & struct%rxstruc(1:abs(struct%rnpstru(is)), is), &
               & struct%rystruc(1:abs(struct%rnpstru(is)), is), &
               & doesIntersect, iSegment, ipx, ipy)
          if (present(iStruct)) iStruct = is
          if (doesIntersect) return
       end do
       
    end do

    doesIntersect = .false.
    if (present(iStruct)) iStruct = GRID_UNDEFINED

  end subroutine intersect_structure


  !> Label cells to be in/outside of the vessel
  subroutine labelCells(equ, grid )      
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreGrid), intent(inout) :: grid

    ! internal
    integer :: iReg, xip, xir
    integer :: cells(npmamx-1,nrmamx-1,nregmx)

    cells = GRID_UNDEFINED

    do iReg = 1, grid%nreg
        ! For region, find a cell next to x-point (which will be on the inside)
        call findXPointCell(xip, xir)

        ! Walk away from there (recursively?) until all cells are marked
        ! For limiter, just take a cell on the core boundary.
        call markInternalCells(xip, xir, cells)
    end do

    ! all cells that are still undefined are external
    where (cells == GRID_UNDEFINED) cells = GRID_EXTERNAL

    grid%cellflag = cells

  contains

    !> Mark internal cells 
    recursive subroutine markInternalCells(ip, ir, cells)
      integer, intent(in) :: ip, ir
      integer :: cells(npmamx-1,nrmamx-1,nregmx)

      ! internal
      integer :: iNb
      integer, parameter :: NNEIGHBOUR = 8
      integer, parameter :: &
          & dp(NNEIGHBOUR) = (/ -1, -1,  0, +1, +1, +1,  0, -1 /), &
          & dr(NNEIGHBOUR) = (/  0, -1, -1, -1,  0, +1, +1, +1 /)

      ! Is this cell inside the region?
      if ( (ip <= 0) .or. (ir <= 0) &
          & .or. (ip >= grid%np1(iReg)) .or. (ir >= grid%nr(iReg)) ) then
          ! no -> skip
          return
      end if

      ! Have we visited this cell before?
      if ( cells(ip, ir, iReg) /= GRID_UNDEFINED ) then
          ! Yes -> skip
          return 
      end if

      ! We are coming from an internal cell. 
      ! Figure out situation of current cell from intersection test.
      select case (grid%cellflag(ip, ir, iReg))
      case(GRID_BOUNDARY, GRID_BOUNDARY_REFINE)
          ! hit a boundary cell - transfer flag, return
          cells(ip, ir, iReg) = grid%cellflag(ip, ir, iReg)
          return
      end select

      ! This is an internal cell
      cells(ip, ir, iReg) = GRID_INTERNAL

      ! proceed into all eight neighbour cells
      do iNb = 1, NNEIGHBOUR 
          call markInternalCells(ip + dp(iNb), ir + dr(iNb), cells)
      end do
    end subroutine markInternalCells

    ! Find cell in current region which is next to an x-point
    subroutine findXPointCell(xipol,xirad)
      integer, intent(out) :: xipol, xirad

      ! internal
      integer :: ipol, irad, ipx
      double precision :: dx
      double precision, parameter :: XPOINT_TOL = 1e-6
      real*8 :: dist
      external dist

      ! search for x-point in this region
      do iPol = 1, grid%np1(iReg)
          do iRad = 1, grid%nr(iReg)

              ! check distance for all x-points
              do ipx = 1, equ%npx
                  dx = dist( equ%ptx(ipx), equ%pty(ipx), &
                      & grid%xmail(iPol, iRad, iReg), &
                      & grid%ymail(iPol, iRad, iReg) )

                  if (dx < XPOINT_TOL) then
                      ! found an x-point, return cell indices
                      xipol = ipol
                      xirad = irad
                      ! if we are at the upper boundary line,
                      ! move back into cell 
                      if (xipol == grid%np1(iReg)) &
                          & xipol = xipol - 1
                      if (xirad == grid%nr(iReg)) &
                          & xirad = xirad - 1

                      write (*,*) "findXPointCell: region ", iReg, ", found x-point&
                          & at ", xipol, xirad, ", position ", equ%ptx(ipx), &
                          & equ%pty(ipx)
                      return
                  end if

              end do

          end do
      end do

      ! if we arrive here, no cell next to an x-point was found
      xipol = GRID_UNDEFINED
      xirad = GRID_UNDEFINED

    end subroutine findXPointCell

  end subroutine labelCells


  !> Fix broken cells by modifying the grid 
  subroutine fixCells(equ, struct, grid)
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(in) :: struct
    type(CarreGrid), intent(inout) :: grid


    ! internal
    integer :: iReg, ip, ir, iBrokenCell
    logical :: isecTopFace, isecBotFace

    ! Map from original grid indices (1:grid%np(iReg), 1:grid%nr(iReg))
    ! to current grid situation (with added radial lines)
    integer :: irMap(nrmamx, nregmx), ipMap(npmamx, nregmx)
    integer :: npRadOriginal(nregmx), npPolOriginal(nregmx)

    ! Set up identity maps
    npRadOriginal = grid%nr
    npPolOriginal = grid%np1
    do iReg = 1, grid%nreg
       ipMap(:, iReg) = (/ (ip, ip = 1, grid%np1(iReg)) /)
       irMap(:, iReg) = (/ (ir, ir = 1, grid%nr(iReg)) /)
    end do

    iBrokenCell = 0

    ! Loop over all cells and fix problem cells by inserting radial grid lines.
    ! This modifies the mailx and maily arrays, especially changing the number of grid points.
    ! The grid node indices thus become inconstent with the other arrays. This 
    ! is accounted for by using mapping arrays between the original grid and the modified/fixed 
    ! one. The maps are kept in ipMap and irMap:
    ! ipMap(ip, iReg) is the new poloidal index originally corresponding with poloidal index ip
    ! in region iReg. Same for irMap.

    do iReg = 1, grid%nreg
        do ip = 1, npPolOriginal(iReg) - 1
            do ir = 1, npRadOriginal(iReg) - 1

                if (grid%cellflag(ip, ir, iReg) == GRID_BOUNDARY_REFINE) then

                    isecTopFace = btest(grid%faceflag(ip, ir, iReg), INDEX_FACE_TOP)
                    isecBotFace = btest(grid%faceflag(ip, ir, iReg), INDEX_FACE_BOTTOM)

                    if (isecTopFace .and. isecBotFace) then
                        ! cell to refine, but more than one refinement point per cell - help!
                        write (*,*) "fixCells: your grid/geometry has issues in region ", iReg,&
                            & " cell (ipol, irad) ", ip, ir
                    end if

                    if (isecTopFace) then
                       ! Debug plotting: abuse region number for current cell
                       iBrokenCell = iBrokenCell + 1
                       call csioSetRegion(1000 + iBrokenCell)

                       ! Add a radial line through the intersection point
                       ! of this face with the structure. Note that the
                       ! intersection point known at this point 
                       ! (grid%faceISecPx(INDEX_FACE_TOP, ip, ir, iReg),
                       !   grid%faceISecPy(INDEX_FACE_TOP, ip, ir, iReg)) 
                       ! is inaccurate and will be recomputed 
                        call addRadialLine( equ, struct, grid, &
                             & iReg, &
                             & grid%faceISecPx(INDEX_FACE_TOP, ip, ir, iReg), &
                             & grid%faceISecPy(INDEX_FACE_TOP, ip, ir, iReg), &
                             & iFcR = irMap(ir, iReg) + 1 )

                        ! update grid index map to account for radial line
                        ! We added a radial grid line between poloidal points ip and ip + 1
                        ! Shift all indices bigger than that up by one
                        ! TODO: check we don't run out of space...
                        ipMap(ip + 1 : npPolOriginal(iReg), iReg) = ipMap(ip + 1 : npPolOriginal(iReg), iReg) + 1
                    end if

!!$                    if (isecBotFace) then
!!$                       ! Debug plotting: abuse region number for current cell
!!$                       iBrokenCell = iBrokenCell + 1
!!$                       call csioSetRegion(1000 + iBrokenCell)
!!$
!!$                       ! Add a radial line through the intersection point
!!$                       ! of this face with the structure. Note that the
!!$                       ! intersection point known at this point 
!!$                       ! (grid%faceISecPx(INDEX_FACE_TOP, ip, ir, iReg),
!!$                       !   grid%faceISecPy(INDEX_FACE_TOP, ip, ir, iReg)) 
!!$                       ! is inaccurate and will be recomputed 
!!$                        call addRadialLine( par, equ, struct, grid, &
!!$                             & iReg, &
!!$                             & grid%xmail(ipMap(ip, iReg), irMap(ir, iReg), iReg), grid%ymail(ipMap(ip, iReg), irMap(ir, iReg), iReg), &
!!$                             & grid%xmail(ipMap(ip+1, iReg), irMap(ir, iReg), iReg), grid%ymail(ipMap(ip+1, iReg), irMap(ir, iReg), iReg), &
!!$                             & grid%faceISecPx(INDEX_FACE_BOTTOM, ip, ir, iReg), &
!!$                             & grid%faceISecPy(INDEX_FACE_BOTTOM, ip, ir, iReg), &
!!$                             & recomputeIntersection = .true., & 
!!$                             & iFcP = ipMap(ip, iReg), iFcR = irMap(ir+1, iReg) )
!!$
!!$                        ! update grid index map to account for radial line
!!$                        ! We added a radial grid line between poloidal points ip and ip + 1
!!$                        ! TODO: check we don't run out of space...
!!$                        ipMap(ip + 1 : npPolOriginal(iReg), iReg) = ipMap(ip + 1 : npPolOriginal(iReg), iReg) + 1
!!$                    end if

                end if

            end do
        end do
    end do

  end subroutine fixCells


  !> Add a radial grid line to a grid region, starting at the face
  !> given by the points (fcFromX,fcFromY) and (fcToX,fcToY).
  !> The new radial grid line should go through the the point (px, py).
  !> (This point has to be positioned exactly on a  poloidally/x-aligned face).
  !> If recomputeIntersection is true, (px,py) will be recomputed as the intersection
  !> of the face with a structure. 
  !> The region index iReg in which the face is located has to be given. 
  !> Optionally, the indices of the face (ip,ir) can be given, where
  !> (ip,ir) is the left point (start point) of the face. 
  recursive subroutine addRadialLine( equ, struct, grid, &
       & iReg, &
       & px, py, &
       & iFcP, iFcR )
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(in) :: struct
    type(CarreGrid), intent(inout) :: grid
    integer, intent(in) :: iReg
!!$    double precision, intent(in) :: fcFromX, fcFromY, fcToX, fcToY
    double precision, intent(in) :: px, py
!!$    logical, intent(in), optional :: recomputeIntersection
    integer, intent(in), optional :: iFcP, iFcR

    ! internal
    integer :: ip, ir
    integer :: liFcP, liFcR  ! local copies of dummy arguments iFcP, iFcR
    double precision :: npx(grid%nr(iReg)), npy(grid%nr(iReg))

    double precision :: llx(npnimx), lly(npnimx)
    integer :: llNp

    logical :: doesIntersect, alignment
    double precision :: newPx, newPy

    integer :: iSurface

    ! Coordinates of starting face
!!$    if (present(iFcP) .and. present(iFcR)) then
!!$        ! If given: use directly
!!$        liFcP = iFcP
!!$        liFcR = iFcR
!!$    else
!!$        ! If not given: find in current region        
!!$        ! call findFaceForPoint(grid, px, py, liFcP, liFcR)
!!$
!!$    end if

    call findFaceForPoint(grid, px, py, iReg, liFcP, liFcR, alignment, &
         & doPoloidal = .true., doRadial = .false., iRad = iFcR )

    ! we expect a poloidal face
    call assert( alignment == .true. )
    ! ...on the given radial line
    call assert( liFcR == iFcR )

    ! Find poloidal level line going through the poloidally aligned face
!!$    call findLevelLineForPoints( equ, &
!!$         & fcFromX, fcFromY, fcToX, fcToY, &
!!$         & llX, llY, llNp )
    call findLevelLineForPoints( equ, &
         & grid%xmail(liFcP, liFcR, iReg), grid%ymail(liFcP, liFcR, iReg), &
         & grid%xmail(liFcP+1, liFcR, iReg), grid%ymail(liFcP+1, liFcR, iReg), &
         & llX, llY, llNp )

    ! Recompute intersection of the poloidal level line with the structure
    ! to guarantee the new point lies on the level line.
    call intersect_structure(llX, llY, struct, doesIntersect, ipx=newPx, ipy=newPy)
    
    if (.not. doesIntersect) stop 'addRadialLine: did not find intersection of face with a structure!'
    write (*,*) 'addRadialLine: old intersection ', px, py, ', new intersection ', newPx, newPy

    ! Add space for new grid points. Shift liFcP + 1 : end up by one
    ! TODO: test that we don't run out of space   
    grid%xmail(liFcP + 2 : grid%np1(iReg) + 1, :, iReg) = &
        & grid%xmail(liFcP + 1 : grid%np1(iReg), :, iReg)
    grid%ymail(liFcP + 2 : grid%np1(iReg) + 1, :, iReg) = &
        & grid%ymail(liFcP + 1 : grid%np1(iReg), :, iReg)
    grid%np1(iReg) = grid%np1(iReg) + 1

    ! copy liFcP notes to liFcP + 1 nodes to have something sensible for plotting
    grid%xmail(liFcP + 1, :, iReg) = grid%xmail(liFcP, :, iReg)
    grid%ymail(liFcP + 1, :, iReg) = grid%ymail(liFcP, :, iReg)

    ! Place given point on reference grid line
    grid%xmail(liFcP + 1, liFcR, iReg) = newPx
    grid%ymail(liFcP + 1, liFcR, iReg) = newPy
    
    ! Debug output
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

    ! For this region iReg: compute new radial points by moving
    ! away from the given point in both directions

    ! DEBUG: no relaxation inside insertPoints for now...
    !nrelax = 0    

    ! Positive direction
    iSurface = 0
    do ir = lIFcR + 1, grid%nr(iReg)       
       iSurface = iSurface + 1
       call csioSetSurface(iSurface)
       call insertPoints( equ, &
            & grid%xmail(liFcP:liFcP+2, ir-1, iReg), &
            & grid%ymail(liFcP:liFcP+2, ir-1, iReg), &
            & grid%xmail(liFcP:liFcP+2, ir, iReg), &
            & grid%ymail(liFcP:liFcP+2, ir, iReg) )
    end do

    ! Negative direction
    do ir = lIFcR - 1, 1, -1 
       iSurface = iSurface + 1
       call csioSetSurface(iSurface)
       call insertPoints( equ, &
            & grid%xmail(liFcP:liFcP+2, ir + 1, iReg), &
            & grid%ymail(liFcP:liFcP+2, ir + 1, iReg), &
            & grid%xmail(liFcP:liFcP+2, ir, iReg), &
            & grid%ymail(liFcP:liFcP+2, ir, iReg) )
    end do

    ! For points on boundary of region, insert
    ! radial line starting at this point in all other regions
    ! -> recursive call to addRadialLine

    ! Debug output: entire region grid
    call csioSetSurface(iSurface + 1)
    call csioSetRelax(0)
    call csioOpenFile()
    call siloWriteQuadGrid( csioDbfile, "region", &
         & grid%np1(iReg), grid%nr(iReg), &
         & grid%xmail(1:grid%np1(iReg), 1:grid%nr(iReg), iReg), &
         & grid%ymail(1:grid%np1(iReg), 1:grid%nr(iReg), iReg) )
    call csioCloseFile()    

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

    ! Write out intermediate grid stage
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

    ! Optimize distribution of grid points according to the criteria,
    ! using the signed relaxation method 

    nrelax = 0
    ! nrelax is coming from COMRLX.F. It's not a constant, can be modified by user I/O.
    if(nrelax.gt.0) then
       ! 2.   on initialise la fonction qui doit s'annuler pour une
       !      distribution orthogonale
       ! We compute the grid quality criterion for the initiali point distribution.
       ! Like nrelax, pasmin, l0 and l1 come from the COMRLX common block
       ! The two zeros in the clort call are the guard lengths. We effectively 
       ! disable the "proportional distribution" criterion here.
       pasmin = 0d0
       call clort( refx, refy,&
            & newx, newy, &
            & critNew, size(refx), &
            & pasmin, 0d0, 0d0, lRef, lNew )
       
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
               & pasmin, 0d0, 0d0, lRef, lNewModified )

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
          
          if(maxval(abs(critNew)) <= rlcept) exit
       enddo
       
       if(ortmax > rlcept) then
          ! The relaxation failed to produce good results with the
          ! given number of iterations
       end if
       
    endif
  end subroutine insertPoints


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

    call assert( (liFcP /= GRID_UNDEFINED) .and. (liFcR /= GRID_UNDEFINED) )

  contains
    
    ! Compute a normalized measure of point-face distance for the purpose
    ! of finding the face closest to a point. This is not a real distance
    ! measure!
    double precision function normFaceDist(fx1, fy1, fx2, fy2, px, py)      
      double precision, intent(in) :: fx1, fy1, fx2, fy2, px, py

      double precision :: dist
      external :: dist

      normFaceDist = ( dist(fx1, fy1, px, py) + dist(px, py, fx2, fy2) ) &
           & / dist( fx1, fy1, fx2, fy2 )

    end function normFaceDist

  end subroutine findFaceForPoint

  
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


  !> Compute intersection of a line segment with one structure
  !> xst,yst: tableaux des coordonnees des points de la structure.
  !> xx,yy: tableaux des coordonnees contenant les 2 points du segment.
  !> doesIntersect: flag indicating whether intersection found
  !> iSegment: index of segment intersected
  !> ipx, ipy: intersection point (if found)
  subroutine intersect(xx,yy,xst,yst,doesIntersect,iSegment,ipx,ipy)

    !  arguments
    REAL*8, intent(in) :: xx(2),yy(2),xst(:),yst(:)

    logical, intent(out) :: doesIntersect
    integer, intent(out), optional :: iSegment
    REAL*8, intent(out), optional :: ipx, ipy
    !  variables locales
    INTEGER i
    !.. determ: determinant de la matrice des deux equations.
    !.. mult1: facteur multiplicatif du segment de courbe.
    !.. mult2: facteur multiplicatif du segment de structure.
    REAL*8 mult1,mult2,determ


    !..Boucle sur la structure.
    DO i=1, size(xst)-1
        !..Calcul du determinant de la matrice.
        determ = (-(xx(2) - xx(1))) * (yst(i+1) - yst(i)) + & 
            &                   (yy(2) - yy(1)) * (xst(i+1) - xst(i))

        !..Si determinant non nul, alors il y a solution.            
        IF (determ .NE. 0.) THEN

            !..Facteur multiplicatif du segment de courbe avec la methode de Cramer.                    
            mult1 = ((-(xst(i)-xx(1))) * (yst(i+1)-yst(i)) + & 
                &              (yst(i)-yy(1)) * (xst(i+1)-xst(i)))/determ

            !..Pour avoir intersection, il faut que mult1 soit entre 0 et 1
            IF ((mult1.GT.0.).AND.(mult1.LT.1.)) THEN

                !..Fact. mult. du segment de structure.
                mult2= ((xx(2)-xx(1)) * (yst(i)-yy(1)) - & 
                    &                (yy(2)-yy(1)) * (xst(i)-xx(1)))/determ

                !..Intersection si mult2 entre 0 et 1
                IF ((mult2.GT.0.).AND.(mult2.LT.1.)) THEN
                    doesIntersect = .true.
                    if (present(iSegment)) iSegment = i
                    if (present(ipx) .and. present(ipy)) then
                        ipx = xx(1) + mult1 * (xx(2) - xx(1))
                        ipy = yy(1) + mult1 * (yy(2) - yy(1))
                    end if
                    return
                ENDIF
            ENDIF
        ENDIF
    END DO

    doesIntersect = .FALSE.
    if (present(iSegment)) iSegment = GRID_UNDEFINED
  END subroutine intersect


end module carre_postprocess
