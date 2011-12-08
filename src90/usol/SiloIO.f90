module SiloIO

  ! General Silo IO routine wrappers

  ! FIXME: error handling

  use Logging
  use Helper
  use KindDefinitions , only: iKind, rKind

  implicit none
 
  !private
  !public siloSetDisabled, siloGetDisabled, siloOpen, siloClose, siloWriteLineSegmentGrid

  include 'silo.inc'

  logical :: SILOIO_DISABLED = .false.


contains

  subroutine siloSetDisabled( isDisabled )
    logical, intent(in) :: isDisabled

    SILOIO_DISABLED = isDisabled
  end subroutine siloSetDisabled

  logical function siloGetDisabled()
    siloGetDisabled = SILOIO_DISABLED
  end function siloGetDisabled


  subroutine siloOpen( filename, dbfile )
    character(*), intent(in) :: filename
    integer, intent(out) :: dbfile

    integer ierr

    if ( SILOIO_DISABLED ) return

    ierr = dbcreate( filename, len(filename), DB_CLOBBER, DB_LOCAL,&
         & 'Comment about the data', 22, DB_HDF5, dbfile)

    if(dbfile.eq.-1) then
            call logmsg(LOGFATAL, 'Could not create Silo file '//filename//'\n')
            stop
    endif

  end subroutine siloOpen


  subroutine siloClose( dbfile )
    integer, intent(in) :: dbfile

    integer ierr

    if ( SILOIO_DISABLED ) return

    ierr = dbclose(dbfile)

  end subroutine siloClose


  !> Write a line segment grid
  !> Segments: dim. 1: segment index, dim. 2: 1 = start point, 2 = end point, dim. 3: 1 = x coordinate, 2 = y coordinate  
  subroutine siloWriteLineSegmentGrid( dbfile, name, nSeg, segments )
    integer, intent(in) :: dbfile
    character(*), intent(in) :: name
    integer(iKind), intent(in) :: nSeg
    real(rKind), intent(in), dimension(nSeg,2,2) :: segments

    ! internal
    integer :: err, ierr

    integer, dimension(1) :: shapetype, shapesize, shapecount
    integer, dimension(nSeg*2) :: zonelist
    integer :: i

    real(rKind), dimension(nSeg*2) :: x, y

    if ( siloGetDisabled() ) return

    ! build shapelists
    shapetype(1) = DB_ZONETYPE_BEAM
    shapesize(1) = 2
    shapecount(1) = nSeg
    
    zonelist = (/ (i, i = 1, nSeg * 2) /)

    ! Write out connectivity information.
    err = dbputzl2(dbfile, name//"_zonelist", &
         & len(name) + len("_zonelist"), & ! length of name 
         & nSeg, & ! nzones
         & 2, & ! ndims
         & zonelist, &
         & nseg * 2, & ! LNODELIST 
         & 1, & ! base of nodelist (zero or one)
         & 0, & ! no. of ghost zones at beginning of nodelist
         & 0, & ! no. of ghost zones at end of nodelist
         & shapetype, & ! array, all DB_ZONETYPE_BEAM
         & shapesize, & ! shapesize - number of nodes for the shape types
         & shapecount, & ! shapecounts - how many shapes of each type
         & 1, & ! nshapetypes - only one type
         & DB_F77NULL, & ! Options
         & ierr)
    
    if ( err == -1 ) then
            call logmsg( LOGFATAL, "siloWriteLineSegmentGrid: error at dbputzl: ", err )
            stop "siloWriteLineSegmentGrid: error at dbputzl"
    end if

    do i = 1, nSeg
       x( (i - 1) * 2 + 1 ) = segments( i, 1, 1 )
       x( (i - 1) * 2 + 2 ) = segments( i, 1, 2 )
       y( (i - 1) * 2 + 1 ) = segments( i, 2, 1 )
       y( (i - 1) * 2 + 2 ) = segments( i, 2, 2 )
    end do

    ! Write an unstructured mesh
    err = dbputum(dbfile, name, len(name), &
         & 2, & ! ndims
!!$         & reshape( segments(:,1,:), (/ nSeg * 2 /) ), reshape( segments(:,2,:), (/ nSeg * 2 /) ), &
         & x, y, &
         & DB_F77NULL, "X", 1, "Y", 1, DB_F77NULL, 0, &
         & DB_DOUBLE, & !example puts datatype here...
         & nSeg * 2, nSeg, & ! nnodes, nzones
         & name//"_zonelist", len(name//"_zonelist"), &
         & DB_F77NULL, 0, &
         !& DB_DOUBLE, &  ! according to silo.book, belongs here?
         & DB_F77NULL, & ! optlist_id
         & ierr)
    
    if ( err == -1 ) then
            call logmsg( LOGFATAL, "siloWriteLineSegmentGrid: error at dbputum: ", err )
            stop "siloWriteLineSegmentGrid: error at dbputum"
    end if                    

  end subroutine siloWriteLineSegmentGrid

  
  !> Write a line segment grid, starting with a list of points (x,y)
  subroutine siloWriteLineSegmentGridFromPoints( dbfile, name, x, y )
    integer, intent(in) :: dbfile
    character(*), intent(in) :: name
    real(rKind), intent(in), dimension(:) :: x, y

    ! internal
    integer :: is
    real(rKind), dimension(size(x)-1,2,2) :: segments

    ! check consistency
    if ( size(x) /= size(y) ) then
            call logmsg( LOGFATAL, "siloWriteLineSegmentGridFromPoints: size mismatch in point arrays" )
            stop "siloWriteLineSegmentGridFromPoints:1"
    end if

    ! convert a list of points into individual segments
    do is = 1, size(x) - 1
            segments(is,1,1) = x(is)
            segments(is,2,1) = y(is)
            segments(is,1,2) = x(is + 1)
            segments(is,2,2) = y(is + 1)
    end do

    call siloWriteLineSegmentGrid( dbfile, name, size(x) - 1, segments )

  end subroutine siloWriteLineSegmentGridFromPoints
  
  
  !> Expand data given on individual points to be plotted 
  !> on an unstructured grid where these points have been 
  !> assembled into segments.
  function siloExpandSegmentData( data ) result (segdata) 
    real(rKind), intent(in), dimension(:) :: data
    real(rKind),  dimension( ( size(data) - 1 ) * 2 ) :: segdata
    
    ! internal
    integer :: ip

    do ip = 1, size(data) - 1 
            segdata( (ip-1)*2 + 1 ) = data(ip)
            segdata( (ip-1)*2 + 2 ) = data(ip + 1)
    end do

  end function siloExpandSegmentData


  !> Write data to a unstructured grid
  subroutine siloWriteUMData( dbfile, gridname, name, data, mode )
    integer, intent(in) :: dbfile
    character(*), intent(in) :: gridname, name
    real(rKind), dimension(:), intent(in) :: data
    integer, intent(in) :: mode

    ! internal
    integer :: err, ierr

    if ( siloGetDisabled() ) return

    select case (mode)
    case (DB_ZONECENT, DB_NODECENT)
            ! all is good
    case default
            call logmsg( LOGFATAL, "siloWriteUMData: given mode not supported:", mode )
            stop "'siloWriteUMData:1"
    end select

    err = dbputuv1(dbfile, name, len(name), gridname, len(gridname), &
         & data, & ! data
         & size(data), & ! number of elements
         & DB_F77NULL, &
         & 0, &
         & DB_DOUBLE, &
         & mode, &
         & DB_F77NULL, &
         & ierr)

    if ( err == -1 ) stop "g2deSilo_writeCvScalar: error at dbputuv1"

  end subroutine siloWriteUMData


  !> Write a point mesh
  subroutine siloWritePointGrid( dbfile, name, x, y )
    integer, intent(in) :: dbfile
    character(*), intent(in) :: name
    real(rKind), intent(in), dimension(:) :: x, y

    ! internal
    integer :: err, ierr

    if ( siloGetDisabled() ) return

    if (size(x) == 0) then
       call logmsg(LOGDEBUG, "siloWritePointGrid: not writing empty grid, name "//name)
       return
    end if

    err = dbputpm(dbfile, name, len(name), 2, x, y, DB_F77NULL, &
         & size(x), DB_DOUBLE, DB_F77NULL, ierr)
      
  end subroutine siloWritePointGrid



  !> Write a structured 2d grid.
  !>
  !> Uses siloWriteQuadGrid.
  subroutine siloWriteStructured2dGrid( dbfile, name, nx, ny, x, y )
    integer, intent(in) :: dbfile
    character(*), intent(in) :: name
    integer, intent(in) :: nx, ny
    real(rKind), intent(in) :: x(nx), y(ny)

    ! internal
    real(rKind) :: fx(nx, ny), fy(nx, ny)
    integer :: i

    ! Set up full coordinate arrays (TODO: fortran intrinsic for this?)
    do i = 1, ny
       fx(:, i) = x
    end do
    
    do i = 1, nx
       fy(i, :) = y
    end do

    call siloWriteQuadGrid( dbfile, name, nx, ny, fx, fy )

  end subroutine siloWriteStructured2dGrid


  !> Write a logically rectangular, structured quadrilateral (non-rectilinear) grid
  !> Segments: dim. 1: segment index, dim. 2: 1 = start point, 2 = end point, dim. 3: 1 = x coordinate, 2 = y coordinate
  !> 
  !> If logicalPlot == .true., the node positions given in x,y are ignored and substituted with the node indices
  !> (i.e., the cartesian geometry of the grid is used)

  subroutine siloWriteQuadGrid( dbfile, name, nx, ny, x, y, logicalPlot )
    integer, intent(in) :: dbfile
    character(*), intent(in) :: name
    integer, intent(in) :: nx, ny
    real(rKind), intent(in), dimension(nx,ny) :: x, y
    logical, intent(in), optional :: logicalPlot

    ! internal
    integer :: err, ierr, dims(2), i
    real(rKind) :: lx(nx, ny), ly(nx, ny)

    if ( siloGetDisabled() ) return

    lx = x
    ly = y

    if (present(logicalPlot)) then
       if (logicalPlot) then
          ! Substitute coordinate data          
          do i = 1, nx
             lx(i,:) = i
          end do
          do i = 1, ny
             ly(:,i) = i
          end do
       end if
    end if

    dims(1) = nx
    dims(2) = ny

!!$    err = dbputqm ( dbfile, &
!!$         & name, len(name), &
!!$         & "x", 1, &
!!$         & "y", 1, &
!!$         & DB_F77NULL, 0, &
!!$         & real(x), real(y), DB_F77NULL, &
!!$         & dims, 2, & ! dims, ndims
!!$         & DB_FLOAT, DB_NONCOLLINEAR, &
!!$         & DB_F77NULL, ierr )

    err = dbputqm ( dbfile, &
         & name, len(name), &
         & "x", 1, &
         & "y", 1, &
         & DB_F77NULL, 0, &
         & lx, ly, DB_F77NULL, &
         & dims, 2, & ! dims, ndims
         & DB_DOUBLE, DB_NONCOLLINEAR, &
         & DB_F77NULL, ierr )

!!$    ndims = 2
!!$    dims(1) = NNX
!!$    dims(2) = NNY
!!$    err = dbputqm (dbfile, "quadmesh", 8, "xc", 2, "yc", 2, "zc", 2, xx, yy, DB_F77NULL, &
!!$         & dims, ndims, DB_FLOAT, DB_NONCOLLINEAR, DB_F77NULL, ierr)

  end subroutine siloWriteQuadGrid

  !> Write data on a quadrilateral grid
  subroutine siloWriteQuadData( dbfile, gridname, name, data, mode )
    integer, intent(in) :: dbfile
    character(*), intent(in) :: gridname, name
    real(rKind), dimension(:,:), intent(in) :: data
    integer, intent(in) :: mode

    ! internal
    integer :: err, ierr

    if ( siloGetDisabled() ) return

    select case (mode)
    case (DB_ZONECENT, DB_NODECENT)
            ! all is good
    case default
            call logmsg( LOGFATAL, "siloWriteQuadData: given mode not supported:", mode )
            stop "'siloWriteQuadData:1"
    end select
     
    err = dbputqv1(dbfile, name, len(name), gridname, len(gridname), &
         data, shape(data), 2, DB_F77NULL, 0, DB_DOUBLE, mode, DB_F77NULL, ierr)

    if ( err == -1 ) then
            call logmsg( LOGFATAL, "siloWriteQuadData: error writing data" )
            stop "'siloWriteQuadData:2"
    end if

  end subroutine siloWriteQuadData


  function siloFakeGridForPointX(x) result(gridX)
    real(rKind), intent(in) :: x
    real(rKind), dimension(3,3) :: gridX

    real(rKind), parameter :: DX = 1e-3_rKind

    gridX(1,:) = x - DX
    gridX(2,:) = x
    gridX(3,:) = x + DX
  end function siloFakeGridForPointX

  function siloFakeGridForPointY(x) result(gridY)
    real(rKind), intent(in) :: x
    real(rKind), dimension(3,3) :: gridY

    real(rKind), parameter :: DX = 1e-3_rKind

    gridY(:,1) = x - DX
    gridY(:,2) = x
    gridY(:,3) = x + DX
  end function siloFakeGridForPointY


end module SiloIO

