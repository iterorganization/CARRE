module carre_postprocess

  use carre_types
  use CarreDiagnostics
  use itm_string
  use carre_niveau

#ifdef USE_SILO
  use SiloIO
  use CarreSiloIO
#endif

  implicit none

#include <CARREDIM.F>

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

    ! Identify cells that are intersected by vessel structure

    grid%faceflag = 0
    grid%cellflag = GRID_UNDEFINED

    do iReg = 1, grid%nreg

        do iPol = 1, grid%np1(iReg) - 1
            do iRad = 1, par%npr(iReg) - 1

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
    call labelCells(par, equ, grid)

    ! Fix broken cells by modifying the grid accordingly
    call fixCells(par, equ, struct, grid)

#ifdef USE_SILO
    ! write results
    call csioOpenFile('carrePostProces')

    do iReg = 1, grid%nreg
        call siloWriteQuadGrid( csioDbfile, 'region'//int2str(iReg), &
            & grid%np1(iReg), par%npr(iReg), &
            & grid%xmail(1:grid%np1(iReg), 1:par%npr(iReg), iReg), &
            & grid%ymail(1:grid%np1(iReg), 1:par%npr(iReg), iReg) )

        call siloWriteQuadData( csioDbfile, 'region'//int2str(iReg), &
            & 'faceflag'//int2str(iReg), &
            & real(grid%faceflag(1:grid%np1(iReg)-1, 1:par%npr(iReg)-1, iReg),rKind), &
            & DB_ZONECENT )
        call siloWriteQuadData( csioDbfile, 'region'//int2str(iReg), &
            & 'cellflag'//int2str(iReg), &
            & real(grid%cellflag(1:grid%np1(iReg)-1, 1:par%npr(iReg)-1, iReg),rKind), &
            & DB_ZONECENT )
    end do
#endif

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
  subroutine labelCells( par, equ, grid )      
    type(CarreParameters), intent(in) :: par
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
          & .or. (ip >= grid%np1(iReg)) .or. (ir >= par%npr(iReg)) ) then
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
          do iRad = 1, par%npr(iReg)

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
                      if (xirad == par%npr(iReg)) &
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
  subroutine fixCells(par, equ, struct, grid)
    type(CarreParameters), intent(in) :: par
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(in) :: struct
    type(CarreGrid), intent(inout) :: grid


    ! internal
    integer :: iReg, ip, ir
    logical :: isecTopFace, isecBotFace

    do iReg = 1, grid%nreg
        do ip = 1, grid%np1(iReg) - 1
            do ir = 1, par%npr(iReg) - 1

                if (grid%cellflag(ip, ir, iReg) == GRID_BOUNDARY_REFINE) then

                    isecTopFace = btest(grid%faceflag(ip, ir, iReg), INDEX_FACE_TOP)
                    isecBotFace = btest(grid%faceflag(ip, ir, iReg), INDEX_FACE_BOTTOM)

                    if (isecTopFace .and. isecBotFace) then
                        ! cell to refine, but more than one refinement point per cell - help!
                        write (*,*) "fixCells: your grid/geometry has issues in region ", iReg,&
                            & " cell (ipol, irad) ", ip, ir
                    end if

                    if (isecTopFace) then
                       ! Add a radial line through the intersection point
                       ! of this face with the structure. Note that the
                       ! intersection point known at this point 
                       ! (grid%faceISecPx(INDEX_FACE_TOP, ip, ir, iReg),
                       !   grid%faceISecPy(INDEX_FACE_TOP, ip, ir, iReg)) 
                       ! is inaccurate and will be recomputed 
                        call addRadialLine( par, equ, struct, grid, &
                             & iReg, &
                             & grid%xmail(ip, ir+1, iReg), grid%ymail(ip, ir+1, iReg), &
                             & grid%xmail(ip+1, ir+1, iReg), grid%ymail(ip+1, ir+1, iReg), &
                             & grid%faceISecPx(INDEX_FACE_TOP, ip, ir, iReg), &
                             & grid%faceISecPy(INDEX_FACE_TOP, ip, ir, iReg), &
                             & recomputeIntersection = .true., & 
                             & iFcP = ip, iFcR = ir + 1 )

                    end if
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
  recursive subroutine addRadialLine( par, equ, struct, grid, &
       & iReg, &
       & fcFromX, fcFromY, fcToX, fcToY,&
       & px, py, &
       & recomputeIntersection, &
       & iFcP, iFcR )
    type(CarreParameters), intent(in) :: par
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(in) :: struct
    type(CarreGrid), intent(inout) :: grid
    integer, intent(in) :: iReg
    double precision, intent(in) :: fcFromX, fcFromY, fcToX, fcToY
    double precision, intent(in) :: px, py
    logical, intent(in), optional :: recomputeIntersection
    integer, intent(in), optional :: iFcP, iFcR

    ! internal
    integer :: ip, ir
    integer :: liFcP, liFcR  ! local copies of dummy arguments iFcP, iFcR
    double precision :: npx(par%npr(iReg)), npy(par%npr(iReg))

    double precision :: llx(npnimx), lly(npnimx)
    integer :: llNp

    logical :: doesIntersect
    double precision :: newPx, newPy

    ! Coordinates of starting face
    if (present(iFcP) .and. present(iFcR)) then
        ! If given: use directly
        liFcP = iFcP
        liFcR = iFcR
    else
        ! If not given: find in current region        
        ! call findFaceForPoint(grid, px, py, liFcP, liFcP)
    end if

    ! Find poloidal level line going through the poloidally aligned face
    call findLevelLineForPoints( equ, &
         & fcFromX, fcFromY, fcToX, fcToY, &
         & llX, llY, llNp )

    ! Recompute intersection of the poloidal level line with the structure
    ! to guarantee the new point lies on the level line.
    call intersect_structure(llX, llY, struct, doesIntersect, ipx=newPx, ipy=newPy)
    
    if (.not. doesIntersect) stop 'addRadialLine: did not find intersection point!'
    write (*,*) 'addRadialLine: old intersection', px, py
    write (*,*) 'addRadialLine: new intersection', newPx, newPy

    ! Add space for new grid points. Shift liFcP + 1 : end up by one
    ! TODO: test that we don't run out of space   
    grid%xmail(liFcP + 2 : grid%np1(iReg) + 1, :, iReg) = &
        & grid%xmail(liFcP + 1 : grid%np1(iReg), :, iReg)
    grid%ymail(liFcP + 2 : grid%np1(iReg) + 1, :, iReg) = &
        & grid%ymail(liFcP + 1 : grid%np1(iReg), :, iReg)
    grid%np1(iReg) = grid%np1(iReg) + 1

    ! Place given point    
    grid%xmail(liFcP + 1, liFcR, iReg) = px
    grid%ymail(liFcP + 1, liFcR, iReg) = py

    ! For this region iReg: compute new radial points by moving
    ! away from the given point in both directions

    ! Positive direction
    do ir = iFcR + 1, par%npr(iReg)       
        call insertPoints( equ, struct, &
            & grid%xmail(liFcP:liFcP+2, iFcR-1, iReg), &
            & grid%ymail(liFcP:liFcP+2, iFcR-1, iReg), &
            & grid%xmail(liFcP:liFcP+2, iFcR, iReg), &
            & grid%ymail(liFcP:liFcP+2, iFcR, iReg) )
    end do

    ! Negative direction
    do ir = iFcR - 1, 1, -1 

    end do

    ! For points on boundary of region, insert
    ! radial line starting at this point in all other regions
    ! -> recursive call to addRadialLine


  end subroutine addRadialLine


  !> Starting from a given (reference) poloidal grid line, place points on the
  !> adjacent grid line. Of the adjacent grid line, the first
  !> and last point should be given
  subroutine insertPoints(equ, struct, refx, refy, newx, newy)
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(in) :: struct
    double precision, intent(in) :: refx(:), refy(:)
    double precision, intent(out) :: newx(size(refx)), newy(size(refy))

    double precision :: nivRefX(npnimx), nivRefY(npnimx)
    double precision :: nivNewX(npnimx), nivNewY(npnimx)
    integer :: npNivRef, npNivNew

!!$    ! We need the level lines for both poloidal grid lines
!!$    call findLevelLine(equ, struct, refx(1), refy(1), &
!!$        & refx(size(refx)), refy(size(refy)), &
!!$        & nivRefX, nivRefY, npNivRef )
!!$
    newx = 0.0
    newy = 0.0

!!$    !..On definit maintenant les points de maille de la nouvelle ligne
!!$    !  de niveau a partir de ceux de la precedente.
!!$
!!$    dernie=0.0
!!$    ll=long(xn2,yn2,npni2)
!!$
!!$    d1=0.
!!$    ipol1=2
!!$    ipoln=nppol-1
!!$    !
!!$    !  1.   on dispose d'abord les points proportionellement a ceux de la
!!$    !       ligne precedente
!!$    l1(1)=zero
!!$    l1(nppol)=ll
!!$    l0(1)=zero
!!$    if(ir.eq.2) l0(nppol)=ll1
!!$    do ipol=ipol1,ipoln
!!$        d1=ruban(xn(1,ianc),yn(1,ianc),nn(ianc),mailx(ipol,ir-1), & 
!!$            &          maily(ipol,ir-1),d1)
!!$        if(ir.eq.2) l0(ipol)=d1
!!$        !            l1(ipol)=(d1/ll1)*ll
!!$        l1(ipol)=(l0(ipol)/l0(nppol))*ll
!!$        CALL COORD(xn(1,inouv),yn(1,inouv),nn(inouv),l1(ipol), & 
!!$            &               mailx(ipol,ir),maily(ipol,ir))
!!$    enddo
!!$    !
!!$    mailx(nppol,ir)=xn(nn(inouv),inouv)
!!$    maily(nppol,ir)=yn(nn(inouv),inouv)
!!$    !
!!$    if(nrelax.gt.0) then
!!$        !
!!$        !  2.   on initialise la fonction qui doit s'annuler pour une
!!$        !       distribution orthogonale
!!$        call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir), & 
!!$            & maily(1,ir),ort1,nppol,pasmin,garde1,garde2,l0,l1, & 
!!$            & ortpur,propo,varr)
!!$
!!$        !
!!$        !  3.   on procede a un premier deplacement des noeuds
!!$        l2(1)=zero
!!$        l2(nppol)=l1(nppol)
!!$        diag%segt(ir,ireg)=l2(nppol)
!!$        do ipol=ipol1,ipoln
!!$            if(ort1(ipol).gt.zero) then
!!$                l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol+1)
!!$            else
!!$                l2(ipol)=0.9*l1(ipol)+0.1*l1(ipol-1)
!!$            endif
!!$            call coord(xn(1,inouv),yn(1,inouv),nn(inouv),l2(ipol), & 
!!$                & mailx(ipol,ir),maily(ipol,ir))
!!$
!!$            diag%somort(ir,ireg)= diag%somort(ir,ireg)+(ort1(ipol)/nppol)
!!$            diag%somortpur(ir,ireg)= diag%somortpur(ir,ireg)+(ortpur(ipol)/nppol)
!!$            diag%sompropo(ir,ireg)= diag%sompropo(ir,ireg)+(propo(ipol)/nppol)
!!$            diag%somvarr(ir,ireg)= diag%somvarr(ir,ireg)+(varr(ipol)/nppol)
!!$        enddo
!!$        !
!!$        !  4.   on relaxe les points de facon iterative pour realiser la
!!$        !       meilleure orthogonalite possible
!!$        do i=1,nrelax
!!$            call csioSetRelax( i )
!!$
!!$            call clort(mailx(1,ir-1),maily(1,ir-1),mailx(1,ir), & 
!!$                & maily(1,ir),ort2,nppol,pasmin,garde1,garde2,l0,l2, & 
!!$                & ortpur,propo,varr)
!!$
!!$            ortmax=zero
!!$            do ipol=ipol1,ipoln
!!$                if(abs(ort2(ipol)).gt.rlcept) then
!!$                    del=-ort2(ipol)*(l2(ipol)-l1(ipol)) & 
!!$                        & /(ort2(ipol)-ort1(ipol))
!!$                    if(del.gt.zero) then
!!$                        del=min(del,relax*(l2(ipol+1)-l2(ipol)))
!!$                    else
!!$                        del=max(del,relax*(l2(ipol-1)-l2(ipol)))
!!$                    endif
!!$                    if(del.ne.zero) then
!!$                        l1(ipol)=l2(ipol)
!!$                        ort1(ipol)=ort2(ipol)
!!$                        l2(ipol)=l1(ipol)+del
!!$                    endif
!!$                    call coord(xn(1,inouv),yn(1,inouv),nn(inouv), & 
!!$                        & l2(ipol),mailx(ipol,ir),maily(ipol,ir))
!!$                endif
!!$                ortmax=max(ortmax,abs(ort2(ipol)))
!!$            enddo
!!$
!!$            if(ortmax <= rlcept) exit
!!$        enddo
!!$        
!!$        if(ortmax > rlcept) then
!!$            ! The relaxation failed to produce good results with the
!!$            ! given number of iterations
!!$        end if
!!$
!!$    endif
!!$
!!$    RETURN
  end subroutine insertPoints

  
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
