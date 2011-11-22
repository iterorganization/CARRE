module CarreSiloIO

  ! Carre-specific Silo IO routines

  use Logging 
  use Helper
  use carre_types
  use KindDefinitions , only: iKind, rKind
#ifdef USE_SILO           
  use SiloIO
#endif
  implicit none

#include <CARREDIM.F>
  
  integer :: iReg = 0, iSurf = 0, iRelax = 0
  integer :: iRegSave = 0, iSurfSave = 0, iRelaxSave = 0
  integer :: csioDbfile
  logical :: fileopen = .false.

  character(5), parameter :: FILENAMEBASE_DEFAULT = 'carre'
  character(5), save :: filenameBase = FILENAMEBASE_DEFAULT, filenameBaseSave

  character(5+3+4+3), save :: filename = 'carre0010001001'
  character(*), parameter :: FILENAME_EXTENSION = '.silo'
  character(*), parameter :: FILENAME_FORMAT = '(a5,i3.3,i4.4,i3.3)' ! ireg, isurf, irelax

  integer :: csioStrucNSeg
  real(rKind), allocatable, dimension(:,:,:) :: csioStrucSegments
  integer :: csioVirtualStrucNSeg
  real(rKind), allocatable, dimension(:,:,:) :: csioVirtualStrucSegments

  private
  public csioOpenFile, csioCloseFile
  public csioSetRegion, csioSetSurface, csioSetRelax, csioSetFilename, csioSetFilenameBase, csioGetStructureSegments
  public csioSaveCounters, csioRestoreCounters
  public csioStrucNSeg, csioStrucSegments
  public csioVirtualStrucNSeg, csioVirtualStrucSegments
  public writeGridStateToSiloFile
  public csioDbfile

contains
 
  subroutine csioOpenFile(name)
    character(*), intent(in), optional :: name

    call csioCloseFile()

    call csioSetFilename(name)
#ifdef USE_SILO
    call logmsg(LOGDEBUG, "CarreSiloIO.csioOpenFile: opening file "//filename//FILENAME_EXTENSION)
    call siloOpen( filename//FILENAME_EXTENSION, csioDbfile )
#endif
    fileopen = .true.

#ifdef USE_SILO           
    if ( allocated( csioStrucSegments ) ) then
            call siloWriteLineSegmentGrid( csioDbfile, "structure", csioStrucNSeg, csioStrucSegments )            
    end if
    if ( allocated( csioVirtualStrucSegments ) ) then
            call siloWriteLineSegmentGrid( csioDbfile, "virtualstructure", csioVirtualStrucNSeg, csioVirtualStrucSegments )
    end if
#endif

  end subroutine csioOpenFile

  subroutine csioCloseFile()
    if ( fileopen ) then        
#ifdef USE_SILO       
       call logmsg(LOGDEBUG, "CarreSiloIO.csioCloseFile: closing file "//filename)    
       call siloClose( csioDbfile )
#endif
       fileopen = .false.
    end if
  end subroutine csioCloseFile

  subroutine csioSetFilenameBase( base )
    character(5), intent(in), optional :: base

    if (present(base)) then
       filenameBase = base
    else
       filenameBase = FILENAMEBASE_DEFAULT
    end if

  end subroutine csioSetFilenameBase

  subroutine csioSetRegion( diReg )
    integer, intent(in) :: diReg
   
    call logmsg(LOGDEBUGBULK, "CarreSiloIO.csioSetRegion: region "//int2str(diReg))    
    iReg = diReg
    call csioCloseFile()
  end subroutine csioSetRegion
 
  subroutine csioSetSurface( diSurf, delta )
    integer, intent(in), optional :: diSurf, delta

    if (present(diSurf)) iSurf = diSurf       
    if (present(delta)) iSurf = iSurf + delta

    call logmsg(LOGDEBUGBULK, "CarreSiloIO.csioSetSurface: surface "//int2str(iSurf))    
    call csioCloseFile()
  end subroutine csioSetSurface

  subroutine csioSetRelax( diRelax, delta )
    integer, intent(in), optional :: diRelax, delta

    if (present(diRelax)) iRelax = diRelax
    if (present(delta)) iRelax = iRelax + delta

    call logmsg(LOGDEBUGBULK, "CarreSiloIO.csioSetRelax: relaxation "//int2str(iRelax))    
    call csioCloseFile()
  end subroutine csioSetRelax


  subroutine csioSetFilename(name)
    character(len(filename)), intent(in), optional :: name

    if (present(name)) then 
       call csioCloseFile()
       filename = name
    else
       write ( filename, FILENAME_FORMAT ) filenameBase, ireg, isurf, irelax
    end if
  end subroutine csioSetFilename

  !> Save counter variables into save* variables for restoring them later.
  subroutine csioSaveCounters()
    filenameBaseSave = filenameBase
    iRegSave = iReg
    iSurfSave = iSurf
    iRelaxSave = iRelax
  end subroutine csioSaveCounters

  !> Restore counter variables from save* variables
  subroutine csioRestoreCounters()
    filenameBase = filenameBaseSave
    iReg = iRegSave
    iSurf = iSurfSave
    iRelax = iRelaxSave
  end subroutine csioRestoreCounters

  !> Get a description of a line segment grid as expected by siloWriteLineSegmentGrid
  !> Segments: dim. 1: segment index, dim. 2: 1 = x coordinate, 2 = y coordinate, dim. 3: 1 = start point, 2 = end point,
  
  subroutine csioGetStructureSegments( nstruc, npstru, xstruc, ystruc, nSeg, segments )
    integer, intent(in) :: nstruc
    integer, intent(in), dimension(strumx) :: npstru
    real(rKind), intent(in), dimension(npstmx,strumx) :: xstruc, ystruc

    integer(iKind), intent(out) :: nSeg
    real(rKind), intent(out), dimension(:,:,:), allocatable :: segments

    ! internal
    integer :: is, ip, ic

    ! A structure is a collection of ordered vertices. Each structure has npstru(istruc) - 1 segments
    ! Note: npstru can be negative (iirc for closed structures)
    is =  sum( abs(npstru(1:nstruc)) - 1 )
    allocate( segments( is, 2, 2 ) )

    ic = 0
    do is = 1, nstruc
            do ip = 1, abs(npstru( is )) - 1
                    ic = ic + 1
                    segments(ic,1,1) = xstruc(ip,is)
                    segments(ic,2,1) = ystruc(ip,is)
                    segments(ic,1,2) = xstruc(ip + 1, is)
                    segments(ic,2,2) = ystruc(ip + 1, is)
            end do
    end do

    nSeg = ic

  end subroutine csioGetStructureSegments


  !> Write grid state from data structures to silo file
  
  subroutine writeGridStateToSiloFile(filename, equ, struct, grid)
    character(*), intent(in) :: filename
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(in) :: struct
    type(CarreGrid), intent(in), optional :: grid       

    ! internal
    integer :: iReg, iFace, iRad

    ! for writing out point grids
    double precision, dimension(npmamx * nrmamx * nregmx * 2) :: tmpX, tmpY
    integer :: nIntPoints, ip, ir, ip2, ir2
    integer :: ixpoint, ibranch
    integer :: is, np

#ifdef USE_SILO
    call csioOpenFile(filename)

    ! write equilibrium data
    call siloWriteStructured2dGrid( csioDbfile, 'equilibrium_grid', &
         & equ%nx, equ%ny, &
         & equ%x(1:equ%nx), equ%y(1:equ%ny) )

    call siloWriteQuadData( csioDbfile, 'equilibrium_grid', 'psi', &
         & equ%psi(1:equ%nx, 1:equ%ny), DB_NODECENT )
    call siloWriteQuadData( csioDbfile, 'equilibrium_grid', 'psidx', &
         & equ%psidx(1:equ%nx, 1:equ%ny), DB_NODECENT )
    call siloWriteQuadData( csioDbfile, 'equilibrium_grid', 'psidy', &
         & equ%psidy(1:equ%nx, 1:equ%ny), DB_NODECENT )

    ! write parametrized separatrix branches
    do ixpoint = 1, equ%npx
        do ibranch = 1, 4
            if (equ%nptot(ibranch, ixpoint) <= 1) cycle
            
            call siloWriteLineSegmentGridFromPoints( csioDbfile, &
                 & "separatrix_xpoint"//int2str(ixpoint)//"_branch"//int2str(ibranch), &
                 & equ%separx(1:equ%nptot(ibranch, ixpoint), ibranch, ixpoint), &
                 & equ%separy(1:equ%nptot(ibranch, ixpoint), ibranch, ixpoint) )
        end do
    end do

    ! write structures
    do is = 1, struct%nstruc        
        np = abs(struct%npstru(is))
        call siloWriteLineSegmentGridFromPoints( csioDbfile, &
             & "structure_"//int2str(is), &
             & struct%xstruc(1:np, is), &
             & struct%ystruc(1:np, is) )
    end do

    if (present(grid)) then

        ! write region grids
        do iReg = 1, grid%nreg
            if ((grid%np1(iReg) > 0) .and. (grid%nr(iReg) > 0)) then

                call siloWriteQuadGrid( csioDbfile, 'region'//int2str(iReg), &
                     & grid%np1(iReg), grid%nr(iReg), &
                     & grid%xmail(1:grid%np1(iReg), 1:grid%nr(iReg), iReg), &
                     & grid%ymail(1:grid%np1(iReg), 1:grid%nr(iReg), iReg) )
                call siloWriteQuadData( csioDbfile, 'region'//int2str(iReg), &
                     & 'cellFaceFlag'//int2str(iReg), &
                     & real(grid%cellFaceFlag(1:grid%np1(iReg)-1, 1:grid%nr(iReg)-1, iReg),rKind), &
                     & DB_ZONECENT )
                call siloWriteQuadData( csioDbfile, 'region'//int2str(iReg), &
                     & 'cellflag'//int2str(iReg), &
                     & real(grid%cellflag(1:grid%np1(iReg)-1, 1:grid%nr(iReg)-1, iReg),rKind), &
                     & DB_ZONECENT )

                call siloWriteQuadGrid( csioDbfile, 'cregion'//int2str(iReg), &
                     & grid%np1(iReg), grid%nr(iReg), &
                     & grid%xmail(1:grid%np1(iReg), 1:grid%nr(iReg), iReg), &
                     & grid%ymail(1:grid%np1(iReg), 1:grid%nr(iReg), iReg), &
                     & logicalPlot = .true. )
                call siloWriteQuadData( csioDbfile, 'cregion'//int2str(iReg), &
                     & 'ccellflag'//int2str(iReg), &
                     & real(grid%cellflag(1:grid%np1(iReg)-1, 1:grid%nr(iReg)-1, iReg),rKind), &
                     & DB_ZONECENT )
            end if

        end do

       ! internal points
       nIntPoints = 0

       do iReg = 1, grid%nreg
          do ip = 1, grid%np1(iReg)
             do ir = 1, grid%nr(iReg)
                if ( grid%pointFlag(ip, ir, iReg) /= GRID_INTERNAL) cycle

                nIntPoints = nIntPoints + 1

                tmpX(nIntPoints) = grid%xmail(ip, ir, iReg)
                tmpY(nIntPoints) = grid%ymail(ip, ir, iReg)                
             end do
          end do
       end do

       call siloWritePointGrid( csioDbfile, 'internalPoints', &
            & tmpX(1:nIntPoints), tmpY(1:nIntPoints) )

       ! external points
       nIntPoints = 0

       do iReg = 1, grid%nreg
          do ip = 1, grid%np1(iReg)
             do ir = 1, grid%nr(iReg)
                if ( grid%pointFlag(ip, ir, iReg) /= GRID_EXTERNAL) cycle

                nIntPoints = nIntPoints + 1

                tmpX(nIntPoints) = grid%xmail(ip, ir, iReg)
                tmpY(nIntPoints) = grid%ymail(ip, ir, iReg)                
             end do
          end do
       end do

       call siloWritePointGrid( csioDbfile, 'externalPoints', &
            & tmpX(1:nIntPoints), tmpY(1:nIntPoints) )


       ! boundary points
       nIntPoints = 0

       do iReg = 1, grid%nreg
          do ip = 1, grid%np1(iReg)
             do ir = 1, grid%nr(iReg)
                if ( grid%pointFlag(ip, ir, iReg) /= GRID_BOUNDARY) cycle

                nIntPoints = nIntPoints + 1

                tmpX(nIntPoints) = grid%xmail(ip, ir, iReg)
                tmpY(nIntPoints) = grid%ymail(ip, ir, iReg)                
             end do
          end do
       end do

       !call logmsg(LOGDEBUG,  'carre_postprocess: '//int2str(nIntPoints)//' boundary points')
       if (nIntPoints > 0) then
          call siloWritePointGrid( csioDbfile, 'boundaryPoints', &
               & tmpX(1:nIntPoints), tmpY(1:nIntPoints) )
       end if

       ! radial intersected faces
       nIntPoints = 0

       do iReg = 1, grid%nreg
          do ip = 1, grid%np1(iReg)
             do ir = 1, grid%nr(iReg)

                ! radial face
                if ( ir < grid%nr(iReg) ) then
                   if (grid%faceISec(FACE_RADIAL, ip, ir, iReg)) then
                      nIntPoints = nIntPoints + 1
                      tmpX(nIntPoints) = grid%xmail(ip, ir, iReg)
                      tmpY(nIntPoints) = grid%ymail(ip, ir, iReg)                
                      nIntPoints = nIntPoints + 1
                      tmpX(nIntPoints) = grid%xmail(ip, ir+1, iReg)
                      tmpY(nIntPoints) = grid%ymail(ip, ir+1, iReg)                
                   end if
                end if

             end do
          end do
       end do

       call siloWritePointGrid( csioDbfile, 'radialIntersectedFaces', &
            & tmpX(1:nIntPoints), tmpY(1:nIntPoints) )

       ! radial intersected faces
       nIntPoints = 0

       do iReg = 1, grid%nreg
          do ip = 1, grid%np1(iReg)
             do ir = 1, grid%nr(iReg)

                ! poloidal face
                if ( ip < grid%np1(iReg) ) then
                   if ( grid%faceISec(FACE_POLOIDAL, ip, ir, iReg)) then
                      nIntPoints = nIntPoints + 1
                      tmpX(nIntPoints) = grid%xmail(ip, ir, iReg)
                      tmpY(nIntPoints) = grid%ymail(ip, ir, iReg)                
                      nIntPoints = nIntPoints + 1
                      tmpX(nIntPoints) = grid%xmail(ip+1, ir, iReg)
                      tmpY(nIntPoints) = grid%ymail(ip+1, ir, iReg)                
                   end if
                end if

             end do
          end do
       end do

       call siloWritePointGrid( csioDbfile, 'poloidalIntersectedFaces', &
            & tmpX(1:nIntPoints), tmpY(1:nIntPoints) )


       ! intersected faces with two external points
       ! radial intersected faces
       nIntPoints = 0

       do iReg = 1, grid%nreg
          do ip = 1, grid%np1(iReg)
             do ir = 1, grid%nr(iReg)
                do iFace = 1, 2 ! poloidal, radial

                   select case (iFace)
                   case(FACE_POLOIDAL)
                      if (ip == grid%np1(iReg)) cycle
                      ip2 = ip + 1
                      ir2 = ir
                   case(FACE_RADIAL)
                      if (ir == grid%nr(iReg)) cycle
                      ip2 = ip
                      ir2 = ir + 1
                   end select

                   if ( .not. grid%faceISec(iFace, ip, ir, iReg)) cycle

                   if ( (grid%cellFlag(ip,ir,iReg) == GRID_EXTERNAL) &
                        & .and. (grid%cellFlag(ip2,ir2,iReg) == GRID_EXTERNAL) ) then
                      nIntPoints = nIntPoints + 1
                      tmpX(nIntPoints) = grid%xmail(ip, ir, iReg)
                      tmpY(nIntPoints) = grid%ymail(ip, ir, iReg)                
                      nIntPoints = nIntPoints + 1
                      tmpX(nIntPoints) = grid%xmail(ip2, ir2, iReg)
                      tmpY(nIntPoints) = grid%ymail(ip2, ir2, iReg)
                   end if

                end do
             end do
          end do
       end do

       call siloWritePointGrid( csioDbfile, 'intersectedExternalFaces', &
            & tmpX(1:nIntPoints), tmpY(1:nIntPoints) )
       
    end if

    ! limiting level lines
    do iRad = 1, struct%nbniv
       call siloWriteLineSegmentGridFromPoints( csioDbfile, "limlevelline"//int2str(iRad), &
            & struct%nivx(1:struct%nivtot(iRad), iRad), &
            & struct%nivy(1:struct%nivtot(iRad), iRad) )
    end do

    call csioCloseFile()

#endif

  end subroutine writeGridStateToSiloFile


end module CarreSiloIO
