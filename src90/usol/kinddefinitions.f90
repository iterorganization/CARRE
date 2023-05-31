module KindDefinitions

  use Logging

  implicit none

  ! Integer: 10e-8...10e8
  !integer, parameter :: iKind = selected_int_kind(r=8)
  ! iKind: the default integer, think 32bit signed
  integer, parameter :: iKind = kind( 1 )
  ! iIndex: used for indices, think 16bit signed for small grids
!!$  integer, parameter :: iIndex = selected_int_kind( r = 4 )
  integer, parameter :: iIndex = iKind
  ! iType: used for specifying types, where a low range is sufficient. Think byte.
!!$  integer, parameter :: iType = selected_int_kind( r = 2 )
  ! At the moment, all integer types are set to iKind, until at one point
  ! they will be introduced in the code.
  integer, parameter :: iType = iKind

  ! iFlag: used for storing object information using bit flags
  integer, parameter :: iFlag = iKind

  ! comp.lang.fortran, Paul van Delst, Message <epohfj$j3m$1@news.nems.noaa.gov>

  INTEGER, PARAMETER :: Byte    = SELECTED_INT_KIND(1)   ! Byte  integer
  INTEGER, PARAMETER :: Short   = SELECTED_INT_KIND(8)   ! Short integer
  INTEGER, PARAMETER :: Long    = SELECTED_INT_KIND(8)   ! Long  integer

  INTEGER, PARAMETER :: Single = SELECTED_REAL_KIND(6)  ! Single precision
  INTEGER, PARAMETER :: Double = SELECTED_REAL_KIND(14) ! Double precision

  !integer, parameter :: rKind = SELECTED_REAL_KIND(14) !kind( 0.d0 )
  integer, parameter :: rKind = Double

  ! constants : TODO: move this somewhere else
  !REAL( rKind), PARAMETER :: pi = 3.14159265358979323846 !acos(-1.0d0)


  real(rKind), parameter :: MYRKINDPREC = 1e-16


contains

  subroutine printKindInformation
    real(kind=rKind) :: xReal = 0.0_rKind
    integer(kind=iIndex) :: iIIndex = 0
    integer(kind=iType) :: iIType = 0
    integer(kind=Byte) :: iByte = 0
    integer(kind=Short) :: iShort = 0
    integer(kind=Long) :: iLong = 0

    xReal = 0.123_rKind

    write (*,*) 'rKind=',rKind,'radix(b)=',radix(xReal),'digits=',digits(xReal) ,'precision=', precision(xReal)
    write (*,*) 'tiny(rKind)=',tiny(xReal), 'spacing(0.1_rKind)', spacing(0.1_rkind), 'spacing(1e20_rKind)', spacing(1e20_rkind)
    write (*,*) 'x(rKind)=',xReal,'exponent=',exponent(xReal),'fraction=',fraction(xReal)


    write (*,*) 'iIndex: bit_size=', bit_size( iIIndex ), ', huge=', huge(iIIndex), ', range=', range(iIIndex)
    write (*,*) 'iType: bit_size=', bit_size( iIType ), ', huge=', huge(iIType), ', range=', range(iIType)
    write (*,*) 'iByte: bit_size=', bit_size( iByte ), ', huge=', huge(iByte), ', range=', range(iByte)
    write (*,*) 'iShort: bit_size=', bit_size( iShort ), ', huge=', huge(iShort), ', range=', range(iShort)
    write (*,*) 'iLong: bit_size=', bit_size( iLong ), ', huge=', huge(iLong), ', range=', range(iLong)
    write (*,*) 'iKind: bit_size=', bit_size( iKind ), ', huge=', huge(iKind), ', range=', range(iKind)


    xReal = 1.0_rKind / 20_iKind
    write (*,'(f50.25)') xReal - sqrt( xReal ** 2 )

    write (*,'(f50.25)')  1.1_rKind - xReal / 2.0_rKind + xReal / 2.0_rKind

    write (*,'(f50.25)') sqrt( xReal ** 2 )


  end subroutine

  logical function realEqual( a, b, tol )
    real(rKind), intent(in) :: a, b
    real(rKind), intent(in), optional :: tol

    if ( present( tol ) ) then
            realEqual = ( abs( a - b ) < tol )
    else
            realEqual = ( abs( a - b ) < 2 * spacing( max( a, b ) ) )
    end if

  end function realEqual


  subroutine testArithmetic()

    real(rKind) :: a, b, c, f1, f2, re1, re2

    write (*,*) 'Test arithmetic:'

    c = 3.5e18_rKind
    a = 1.7e-2_rKind
    b = 1.9e-2_rKind

    f1 = c * ( b - a )

    ! introduce relative error to a, b of order 1e-16
    re1 = 1.0_rKind + 1e-15
    re2 = 1.0_rKind + 1.4e-15

    f2 = c * ( b*re2 - a*re1 )

    write (*,*) 'Abs. err: ', f2 - f1
    write (*,*) 'Rel. err: ', (f2 - f1) / f1


  end subroutine testArithmetic


end module KindDefinitions
