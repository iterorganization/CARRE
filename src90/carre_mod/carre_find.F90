module carre_find

  use carre_types
  use Logging
  use Helper
  use itm_assert

  ! maximum allowed occurrence count for a point in a region
  integer, parameter, public :: MAX_POINT_OCCUR = 3

contains

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
  subroutine findPointInRegion(grid, iReg, x, y, npoint, xipol, xirad, findAll)
    type(CarreGrid), intent(in) :: grid
    integer, intent(in) :: iReg
    double precision, intent(in) :: x, y
    integer, intent(out) :: npoint, xipol(MAX_POINT_OCCUR), xirad(MAX_POINT_OCCUR)
    logical, intent(in), optional :: findAll

    ! internal
    integer :: ipol, irad
    logical :: lFindAll

    lFindAll = .false.
    if (present(findAll)) lFindAll = findAll

    xipol = GRID_UNDEFINED
    xirad = GRID_UNDEFINED
    npoint = 0

    ! search point in this region
    do iPol = 1, grid%np1(iReg)
        do iRad = 1, grid%nr(iReg)

            if ( pointsIdentical( x, y, &
                 & grid%xmail(iPol, iRad, iReg), &
                 & grid%ymail(iPol, iRad, iReg) ) ) then

                ! found point, return indices
                npoint = npoint + 1
                if (npoint > MAX_POINT_OCCUR) stop "findPointInRegion: found more instances than anticipated"

                xipol(npoint) = ipol
                xirad(npoint) = irad

                if (.not. lFindAll) return
            end if

        end do
    end do

    ! Nothing found, return GRID_UNDEFINED, npoint=0
  end subroutine findPointInRegion


  ! Find index of of an X-point in this region
  ! Note it is possible an X-point occurs multiple times, or multiple X-points occur.
  ! The first one found will be returned.
  subroutine findXPointInRegion(equ, grid, iReg, xipol, xirad)
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreGrid), intent(in) :: grid
    integer, intent(in) :: iReg
    integer, intent(out) :: xipol, xirad

    ! internal
    integer :: ipx
    integer :: xipols(MAX_POINT_OCCUR), xirads(MAX_POINT_OCCUR), npoint

    ! check all X-points
    do ipx = 1, equ%npx
        call findPointInRegion(grid, iReg, equ%ptx(ipx), equ%pty(ipx), &
             & npoint, xipols, xirads )

        if (npoint > 0) then
            call logmsg(LOGDEBUGBULK,  "findXPointInRegion: region "//int2str(iReg)//", found X-point&
                 & at "//int2str(xipols(1))//' '//int2str(xirads(1))//", position "//real2str(equ%ptx(ipx))//&
                 & ' '//real2str(equ%pty(ipx)) )
            xipol = xipols(1)
            xirad = xirads(1)
            return
        end if
    end do

    ! if we arrive here, no cell next to an X-point was found
    xipol = GRID_UNDEFINED
    xirad = GRID_UNDEFINED

  end subroutine findXPointInRegion


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


  !> In a list of points (xx(i),yy(i)), find the point closest to the point (x,y)
  !> and return its index in the list
  integer function closestPoint( x, y, xx, yy ) 
    double precision, intent(in) :: x, y, xx(:), yy(:)

    ! internal
    integer :: i
    double precision :: dmin, d
    double precision :: dist
    external :: dist

    
    closestPoint = GRID_UNDEFINED
    dmin = huge(dmin)
    
    do i = 1, size(xx)
        d = dist(x, y, xx(i), yy(i))
        if (d < dmin) then
            dmin = d
            closestPoint = i
        end if
    end do

  end function closestPoint


end module carre_find
