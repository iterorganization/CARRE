module CarreSiloIO

  ! Carre-specific Silo IO routines

  use Logging 
  use KindDefinitions , only: iKind, rKind
  use SiloIO

  implicit none


  include "CARREDIM.F"
  
  integer :: iReg = 0, iSurf = 0, iRelax = 0
  integer :: dbfile
  logical :: fileopen = .false.

  character(5+2+4+5), save :: filename = 'carre01000100001'

  integer :: csioStrucNSeg
  real(rKind), allocatable, dimension(:,:,:) :: csioStrucSegments

  private
  public csioOpenFile, csioCloseFile
  public csioSetRegion, csioSetSurface, csioSetRelax, csioSetFilename, csioGetStructureSegments
  public csioStrucNSeg, csioStrucSegments

contains
 
  subroutine csioOpenFile()

    call csioSetFilename()    
    call siloOpen( filename, dbfile )
    
    if ( allocated( csioStrucSegments ) ) then
            call siloWriteLineSegmentGrid( dbfile, "structure", csioStrucNSeg, csioStrucSegments )            
    end if

  end subroutine csioOpenFile

  subroutine csioCloseFile()
    if ( fileopen ) then 
            call siloClose( dbfile )
            fileopen = .false.
    end if
  end subroutine csioCloseFile

  subroutine csioSetRegion( diReg )
    integer, intent(in) :: diReg
   
    iReg = diReg
    call csioCloseFile()
  end subroutine csioSetRegion
 
  subroutine csioSetSurface( diSurf )
    integer, intent(in) :: diSurf

    iSurf = diSurf
    call csioCloseFile()
  end subroutine csioSetSurface

  subroutine csioSetRelax( diRelax )
    integer, intent(in) :: diRelax

    iRelax = diRelax
    call csioCloseFile()
  end subroutine csioSetRelax


  subroutine csioSetFilename()
    write ( filename, '(a5,i2.2,i4.4,i5.5)' ) 'carre', ireg, isurf, irelax
  end subroutine csioSetFilename


  !> Write a line segment grid
  !> Segments: dim. 1: segment index, dim. 2: 1 = start point, 2 = end point, dim. 3: 1 = x coordinate, 2 = y coordinate

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
                    segments(ic,1,2) = ystruc(ip,is)
                    segments(ic,2,1) = xstruc(ip + 1, is)
                    segments(ic,2,2) = ystruc(ip + 1, is)
            end do
    end do

    nSeg = ic

  end subroutine csioGetStructureSegments

end module CarreSiloIO
