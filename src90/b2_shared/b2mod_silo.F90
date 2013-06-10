module b2mod_silo

  !use Logging
  use b2ITMMapping , REMOVED_B2_R8 => R8
  use b2mod_types

  implicit none

#include "silo.inc"

  !character(*), parameter :: FILENAME_BASE = 'usol'
  integer, parameter :: FILENAMEBASELENGTH = 10
  character(FILENAMEBASELENGTH), save :: filenameBase = 'usols00000'
  character(*), parameter :: FILENAME_EXT = '' ! UPDATE filename LENGTH IF CHANGING THIS

  integer, private :: dbfile, timecount  
  character( FILENAMEBASELENGTH + 5), private :: filename

  logical, private :: DISABLED = .false.

!!$  interface g2deSilo_writeCvScalar
!!$          module procedure g2deSilo_writeCvScalarReal, g2deSilo_writeCvScalarInteger
!!$  end interface

contains

  subroutine b2silo_setDisabled( isDisabled )
    logical, intent(in) :: isDisabled

    DISABLED = isDisabled
  end subroutine b2silo_setDisabled

  logical function b2silo_getDisabled()
    b2silo_getDisabled = DISABLED
  end function b2silo_getDisabled


  subroutine b2silo_setTimeCount( count )
    integer, intent(in) :: count
    
    timecount = count    
    call b2silo_updateFilename()
  end subroutine b2silo_setTimeCount


  subroutine b2silo_setFilenameBase( name )
    character(FILENAMEBASELENGTH), intent(in) :: name

    ! internal
    integer :: i

    filenameBase = name    
    do i = 1, len(filenameBase)
        if ( filenameBase(i:i) == ' ' ) filenameBase(i:i) = 'x'
    end do
    call b2silo_updateFilename()
  end subroutine b2silo_setFilenameBase


  subroutine b2silo_updateFilename()
    !write ( *, '(a,i5.5,a)' ) filenameBase, timecount, FILENAME_EXT
    write ( filename, '(a,i5.5,a)' ) filenameBase, timecount
    !write(*,*) filename
  end subroutine b2silo_updateFilename

  function b2silo_getFilename() result (fn)
    character(len(filename)) :: fn
    
    fn = filename
  end function b2silo_getFilename


  subroutine b2silo_open(name)
    character(FILENAMEBASELENGTH), intent(in), optional :: name

    ! internal
    integer ierr

    if ( DISABLED ) return

    if (present(name)) call b2silo_setFilenameBase( name )
    call b2silo_updateFilename()

    ierr = dbcreate( filename, len(filename), DB_CLOBBER, DB_LOCAL,&
         & 'Comment about the data', 22, DB_HDF5, dbfile)
    
    if(dbfile.eq.-1) then
       write (*,*) 'Could not create Silo file!\n'
       stop
    endif

  end subroutine b2silo_open


  subroutine b2silo_close()

    integer ierr

    if ( DISABLED ) return

    ierr = dbclose(dbfile)

  end subroutine b2silo_close


  subroutine b2silo_writeGrid( name, gmap, nx, ny, cflags, crx, cry )
    character(*), intent(in) :: name
    type(B2ITMGridMap), intent(in) :: gmap
    integer, intent(in) :: nx, ny
    integer, intent(in) :: cflags(-1:nx,-1:ny,CARREOUT_NCELLFLAGS)
    real (kind=R8), intent(in) :: & 
         & crx(-1:nx,-1:ny,0:3), cry(-1:nx,-1:ny,0:3)
    
    ! internal
    integer, dimension(gmap%ncv * 4) :: nodelistp, nodelistc
    real(R8), dimension(gmap%nvx) :: xp, yp
    real(R8), dimension(gmap%ncv * 4) :: xc, yc
    integer :: iCv, iNode, nZones, err, ierr, ix, iy, iVx

    if ( DISABLED ) return

    nZones = gmap%nCv 
    
    iNode = 0
    do iCv = 1, gmap % nCv
        ix = gmap%mapCvix(iCv)
        iy = gmap%mapCviy(iCv)

        nodelistp( iNode + 1 ) = gmap%mapVxI(ix,iy,0)
        nodelistp( iNode + 2 ) = gmap%mapVxI(ix,iy,1)
        nodelistp( iNode + 3 ) = gmap%mapVxI(ix,iy,3)
        nodelistp( iNode + 4 ) = gmap%mapVxI(ix,iy,2)

        nodelistc( iNode + 1 ) = iNode + 1
        nodelistc( iNode + 2 ) = iNode + 2
        nodelistc( iNode + 3 ) = iNode + 4
        nodelistc( iNode + 4 ) = iNode + 3

        iNode = iNode + 4
    end do

    ! Build list of nodes in physical space
    do iVx = 1, gmap%nvx
        ix = gmap%mapVxix(iVx)
        iy = gmap%mapVxiy(iVx)
        xp(iVx) = crx(ix,iy,gmap%mapVxIVx(iVx))
        yp(iVx) = cry(ix,iy,gmap%mapVxIVx(iVx))        
    end do

    ! Build list of nodes in computational space
    iNode = 0
    do iCv = 1, gmap % nCv
        ix = gmap%mapCvix(iCv)
        iy = gmap%mapCviy(iCv)

        xc(iNode+1:iNode+4) = ix
        yc(iNode+1:iNode+4) = iy

        xc(iNode+2) = xc(iNode+2) + 1
        yc(iNode+3) = yc(iNode+3) + 1
        xc(iNode+4) = xc(iNode+4) + 1
        yc(iNode+4) = yc(iNode+4) + 1

        iNode = iNode + 4
    end do


    ! Write out connectivity information.
    err = dbputzl(dbfile, name//"_zl_p", &
         & len(name) + len("_zl_p"), & ! length of name 
         & nZones, & ! nzones
         & 2, & ! ndims
         & nodelistp, &
         & iNode, & ! LNODELIST 
         & 1, & ! base of nodelist (zero or one)
         & 4, &! shapesize - number of nodes for the shape types (was (/4/)
         & nZones, & ! shapecounts - how many shapes of each type (was (/nZones/)
         & 1, & ! nshapetypes - only quads
         & ierr)
    err = dbputzl(dbfile, name//"_zl_c", &
         & len(name) + len("_zl_c"), & ! length of name 
         & nZones, & ! nzones
         & 2, & ! ndims
         & nodelistc, &
         & iNode, & ! LNODELIST 
         & 1, & ! base of nodelist (zero or one)
         & 4, &! shapesize - number of nodes for the shape types (was (/4/)
         & nZones, & ! shapecounts - how many shapes of each type (was (/nZones/)
         & 1, & ! nshapetypes - only quads
         & ierr)

    if ( err == -1 ) stop "b2silo_writeGrid: error at dbputzl"
       
    ! Write the unstructured meshes
    err = dbputum(dbfile, name//"_p", len(name)+2, &
         & 2, & ! ndims
         & xp, yp, DB_F77NULL, "X", 1, "Y", 1, DB_F77NULL, 0, &
         & DB_DOUBLE, & !example puts datatype here...
         & gmap%nVx, nZones, & ! nnodes, nzones
         & name//"_zl_p", len(name//"_zl_p"), DB_F77NULL, 0, &
         !& DB_DOUBLE, &  ! according to silo.book, belongs here?
         & DB_F77NULL, & ! optlist_id
         & ierr)
    err = dbputum(dbfile, name//"_c", len(name)+2, &
         & 2, & ! ndims
         & xc, yc, DB_F77NULL, "X", 1, "Y", 1, DB_F77NULL, 0, &
         & DB_DOUBLE, & !example puts datatype here...
         & gmap%nCv*4, nZones, & ! nnodes, nzones
         & name//"_zl_c", len(name//"_zl_c"), DB_F77NULL, 0, &
         !& DB_DOUBLE, &  ! according to silo.book, belongs here?
         & DB_F77NULL, & ! optlist_id
         & ierr)

    if ( err == -1 ) stop "b2silo_writeGrid: error at dbputum"

  end subroutine b2silo_writeGrid

  subroutine b2silo_writeCvScalarReal( gridname, gmap, name, nx, ny, phi )
    character(*), intent(in) :: gridname
    type(B2ITMGridMap), intent(in) :: gmap
    character(*), intent(in) :: name
    integer, intent(in) :: nx, ny
    real(R8), intent(in) :: phi(-1:nx, -1:ny) 

    ! internal
    integer :: err, ierr, nZones, iCv, ix, iy, i
    real(R8) :: data(gmap%nCv)
    character(len(name)) :: modname

    if ( DISABLED ) return

    ! change dots to _
    modname = name
    do i = 1, len(modname)
        if ( modname(i:i) == '.' ) modname(i:i) = '_'
    end do
    !if (loglvl( LOGDEBUG )) write (*,*) 'b2silo: writing ' // name // ' to file ' // filename

    nZones = gmap % nCv

    do iCv = 1, gmap%nCv
        ix = gmap%mapCvix(iCv)
        iy = gmap%mapCviy(iCv)
        data(iCv) = phi(ix, iy)        
    end do

    err = dbputuv1(dbfile, modname//"_p", len(modname)+2, gridname//"_p", len(gridname)+2, &
         & data, & ! data
         & nZones, & ! number of elements
         & DB_F77NULL, &
         & 0, &
         & DB_DOUBLE, &
         & DB_ZONECENT, &
         & DB_F77NULL, &
         & ierr)
    err = dbputuv1(dbfile, modname//"_c", len(modname)+2, gridname//"_c", len(gridname)+2, &
         & data, & ! data
         & nZones, & ! number of elements
         & DB_F77NULL, &
         & 0, &
         & DB_DOUBLE, &
         & DB_ZONECENT, &
         & DB_F77NULL, &
         & ierr)

    if ( err == -1 ) stop "b2silo_writeCvScalar: error at dbputuv1"

  end subroutine b2silo_writeCvScalarReal
!!$
!!$
!!$  subroutine b2silo_writeFcScalarReal(  gridname, grid, name, phiFc, writeGhosts )
!!$    character(*), intent(in) :: gridname
!!$    type(Grid2dEdge), intent(in) :: grid
!!$    character(*), intent(in) :: name
!!$    real(rKind), intent(in) :: phiFc( grid % nFace ) 
!!$    logical, intent(in), optional :: writeGhosts
!!$
!!$    ! internal
!!$    logical :: wgl
!!$
!!$    wgl = .true.
!!$    if ( present( writeGhosts ) ) wgl = writeGhosts
!!$
!!$    call b2silo_writeCvScalarReal( gridname, grid, gridname // '_' // name // '_top', &
!!$         & fcToCv( G2DE_CV_IFCTOP ), writeGhosts = wgl )
!!$    call b2silo_writeCvScalarReal( gridname, grid, gridname // '_' // name // '_right', &
!!$         & fcToCv( G2DE_CV_IFCRIGHT ), writeGhosts = wgl )
!!$    call b2silo_writeCvScalarReal( gridname, grid, gridname // '_' // name // '_bottom', &
!!$         & fcToCv( G2DE_CV_IFCBOTTOM ), writeGhosts = wgl )
!!$    call b2silo_writeCvScalarReal( gridname, grid, gridname // '_' // name // '_left', &
!!$         & fcToCv( G2DE_CV_IFCLEFT ), writeGhosts = wgl )
!!$
!!$  contains
!!$
!!$    function fcToCv( iFcL ) result ( cv )
!!$      real(rKind), dimension( grid % nCv ) :: cv
!!$      integer, intent(in) :: iFcL
!!$
!!$      ! internal
!!$      integer :: iCv
!!$
!!$      cv = 0.0_rKind
!!$
!!$      do iCv = 1, grid % nCv
!!$              if ( grid % cv( iCv ) % iFace( iFcL ) /= GRID_UNDEFINED ) &
!!$                   & cv( iCv ) = phiFc( grid % cv( iCv ) % iFace( iFcL ) )
!!$      end do
!!$
!!$    end function fcToCv
!!$
!!$  end subroutine b2silo_writeFcScalarReal


!!$  subroutine b2silo_writeCvScalarInteger( gridname, grid, name, phi )
!!$    character(*), intent(in) :: gridname
!!$    type(Grid2dEdge), intent(in) :: grid
!!$    character(*), intent(in) :: name
!!$    integer, intent(in) :: phi( grid % nCv ) 
!!$
!!$    ! internal
!!$    integer :: err, ierr, nZones
!!$    real(rKind) :: data( grid % nCv - grid % nGhostCv )
!!$
!!$    if ( DISABLED ) return
!!$
!!$    nZones = grid % nCv - grid % nGhostCv
!!$
!!$    data = pack( phi, grid % cv % type /= CVTYPE_GHOST )
!!$
!!$    err = dbputuv1(dbfile, name, len(name), gridname, len(gridname), &
!!$         & real( data, rKind ), & ! data
!!$         & nZones, & ! number of elements
!!$         & DB_F77NULL, &
!!$         & 0, &
!!$         & DB_INT, &
!!$         & DB_ZONECENT, &
!!$         & DB_F77NULL, &
!!$         & ierr)
!!$
!!$    if ( err == -1 ) stop "b2silo_writeCvScalar: error at dbputuv1"
!!$
!!$
!!$  end subroutine b2silo_writeCvScalarInteger

!!$  subroutine b2silo_writeCvScalarInteger( gridname, grid, name, phi, writeGhosts )
!!$    character(*), intent(in) :: gridname
!!$    type(Grid2dEdge), intent(in) :: grid
!!$    character(*), intent(in) :: name
!!$    integer, intent(in) :: phi( grid % nCv ) 
!!$    logical, optional, intent(in) :: writeGhosts
!!$
!!$    call b2silo_writeCvScalarReal( gridname, grid, name, real( phi, rKind ), writeGhosts )
!!$
!!$  end subroutine b2silo_writeCvScalarInteger



end module b2mod_silo
