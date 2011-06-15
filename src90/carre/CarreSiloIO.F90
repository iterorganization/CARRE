module CarreSiloIO

  ! Carre-specific Silo IO routines

  use Logging 
  use Helper
  use KindDefinitions , only: iKind, rKind
#ifdef USE_SILO           
  use SiloIO
#endif
  implicit none

#include <CARREDIM.F>
  
  integer :: iReg = 0, iSurf = 0, iRelax = 0
  integer :: csioDbfile
  logical :: fileopen = .false.

  character(5+4+3+3), save :: filename = 'carre000100100001'

  integer :: csioStrucNSeg
  real(rKind), allocatable, dimension(:,:,:) :: csioStrucSegments
  integer :: csioVirtualStrucNSeg
  real(rKind), allocatable, dimension(:,:,:) :: csioVirtualStrucSegments

  private
  public csioOpenFile, csioCloseFile
  public csioSetRegion, csioSetSurface, csioSetRelax, csioSetFilename, csioGetStructureSegments
  public csioStrucNSeg, csioStrucSegments
  public csioVirtualStrucNSeg, csioVirtualStrucSegments

  public csioDbfile

contains
 
  subroutine csioOpenFile(name)
    character(len(filename)), intent(in), optional :: name

    call csioCloseFile()

    call csioSetFilename(name)
#ifdef USE_SILO
    call logmsg(LOGDEBUG, "CarreSiloIO.csioOpenFile: opening file "//filename)
    call siloOpen( filename, csioDbfile )
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

  subroutine csioSetRegion( diReg )
    integer, intent(in) :: diReg
   
    call logmsg(LOGDEBUG, "CarreSiloIO.csioSetRegion: region "//int2str(diReg))    
    iReg = diReg
    call csioCloseFile()
  end subroutine csioSetRegion
 
  subroutine csioSetSurface( diSurf )
    integer, intent(in) :: diSurf

    call logmsg(LOGDEBUG, "CarreSiloIO.csioSetSurface: surface "//int2str(diSurf))    
    iSurf = diSurf
    call csioCloseFile()
  end subroutine csioSetSurface

  subroutine csioSetRelax( diRelax )
    integer, intent(in) :: diRelax

    call logmsg(LOGDEBUG, "CarreSiloIO.csioSetRelax: relaxation "//int2str(diRelax))    
    iRelax = diRelax
    call csioCloseFile()
  end subroutine csioSetRelax


  subroutine csioSetFilename(name)
    character(len(filename)), intent(in), optional :: name

    if (present(name)) then 
            call csioCloseFile()
            filename = name
    else
            write ( filename, '(a5,i4.4,i3.3,i3.3)' ) 'carre', ireg, isurf, irelax
    end if
  end subroutine csioSetFilename


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

end module CarreSiloIO
