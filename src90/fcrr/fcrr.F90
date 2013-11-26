      program fcrr
!
!  version : 05.02.99 13:41
!
!======================================================================
!*** Converts the dg data into the input files for Carre
!======================================================================

#ifdef USE_ITMCARRE
      use euITM_schemas  
      use euITM_routines
      use b2mod_ual
#endif


      implicit none
#include <impcon.inc>
      logical ex
      character*14 chtrg,chstr,chequ,chaux,chdgo, & 
     &             chcfld,chcequ,chcstr,chccrr,chcslp
      data chtrg ,  chstr ,  chequ ,  chaux , chdgo  / & 
     &   'dg.trg','dg.str','dg.equ','dg.aux','dg.dgo'/, & 
     &     chcfld    ,  chcequ   ,     chcstr    ,  chccrr   / & 
     &   'btor.dat'  ,'rzpsi.dat','structure.dat','carre.dat'/, & 
     &      chcslp   / & 
     &   'selptx.inf'/
      external fcraxn,fcrtrn,fcrdgi

#ifdef USE_ITMCARRE
      type(type_equilibrium), pointer :: cpoequil(:) => null()
      type(type_limiter) :: cpolimiter
      integer :: idx
#endif

!======================================================================
!
      inquire(file=chtrg,exist=ex)
      if(.not.ex) then
        write(*,*) 'fcrr: the file ',chtrg,' must be present!'
      end if
      inquire(file=chstr,exist=ex)
      if(.not.ex) then
        write(*,*) 'fcrr: the file ',chstr,' must be present!'
      end if
      inquire(file=chequ,exist=ex)
      if(.not.ex) then
        write(*,*) 'fcrr: the file ',chequ,' must be present!'
      end if
!
      immed=.false.
      keylen=8
      mimp=1
      lena=8
!
!*** Read the data
!
      open(1,file=chtrg)
      call fcrchktp(1)
      call import(fcrtrn)
      open(1,file=chstr)
      call fcrstri(1)
      open(1,file=chequ)
      call fcrfldi(1)
      open(1,file=chdgo)
      call import(fcrdgi)
      inquire(file=chaux,exist=ex)
      if(ex) then
        open(1,file=chaux)
        call import(fcraxn)
      end if
      close(1)
!
!*** ... make the necessary transformations
!
      call fcrprp
!
!*** ... and produce the files to be read by carre
!
      call fcrequo(chcequ)
      call fcrstro(chcstr)
      call fcrcrro(chccrr)
      call fcrfldo(chcfld)
      call fcrslpo(chcslp)

!     Write equlibrium and limiter CPO 
#ifdef USE_ITMCARRE

      call fillLimiterCpo( cpolimiter )
      allocate(cpoequil(1))
      call fillEquilibriumCpo( cpoequil(1) )

      ! This overwrites other contents of the shot
      call open_ual(idx, doCreate = .true., nmlFile='ual.namelist.limiter')
      call euitm_put(idx,"limiter",cpolimiter)
      call close_ual(idx)

      call open_ual(idx, doCreate = .true., nmlFile='ual.namelist.equilibrium')
      call euitm_put(idx,"equilibrium",cpoequil)
      call close_ual(idx)

      call euitm_deallocate(cpoequil)
      call euitm_deallocate(cpolimiter)

contains

  subroutine fillLimiterCpo( cpo )
    type(type_limiter), intent(inout) :: cpo

    ! DG common block
#include <FCRCOM.F>

    ! internal
    integer :: k, j, l, i

    ! general information
    allocate(cpo%datainfo%dataprovider(1))
    cpo%datainfo%dataprovider="FCRR"

    ! limiter structure information
    allocate( cpo % limiter_unit( nstr ) )

    k = 0
    do j=1,nstr
        allocate( cpo % limiter_unit( j ) % name(1) )
        write(cpo % limiter_unit(j) % name(1),'(a,i4)') 'Structure ', j
        allocate( cpo % limiter_unit( j ) % closed(1) )

        if(j.le.nclstr) then
            ! Closed structure
            cpo % limiter_unit( j ) % closed(1) = 'y'
        else
            ! Open structure
            cpo % limiter_unit( j ) % closed(1) = 'n'
        end if

        l = lstr(j)
        allocate( cpo % limiter_unit(j) % position % r( l ) )
        allocate( cpo % limiter_unit(j) % position % z( l ) )

        do i=1, l
            cpo % limiter_unit(j) % position % r( i ) = xstr(k+i)
            cpo % limiter_unit(j) % position % z( i ) = ystr(k+i)
        end do
        k=k+lstr(j)
      end do

  end subroutine fillLimiterCpo


  subroutine fillEquilibriumCpo( cpo )
    type(type_equilibrium), intent(inout) :: cpo

#include <FCRCOM.F>

    ! general information
    allocate(cpo%datainfo%dataprovider(1))
    cpo%datainfo%dataprovider="FCRR"
    allocate(cpo%codeparam%codename(1))
    cpo%codeparam%codename(1)="FCRR"
    cpo % time = 0.0

    ! equilibrium information
    cpo % global_param % mag_axis % bphi = rbtor

    allocate( cpo % profiles_2d % grid_type(1) )
    cpo % profiles_2d % grid_type(1) = '1' ! Rectangular R,Z grid
    allocate( cpo % profiles_2d % grid % dim1( nr ) )
    allocate( cpo % profiles_2d % grid % dim2( nz ) )
    allocate( cpo % profiles_2d % psi( nr, nz ) )

    cpo % profiles_2d % grid % dim1 = rgr(1:nr)
    cpo % profiles_2d % grid % dim2 = zgr(1:nz)
    cpo % profiles_2d % psi = pfm

    ! X-points
    allocate(cpo % eqgeometry % xpts % r(nxpt))
    allocate(cpo % eqgeometry % xpts % z(nxpt))
    cpo % eqgeometry % xpts % r = xptcntr(1,1:nxpt)
    cpo % eqgeometry % xpts % z = xptcntr(2,1:nxpt)

    ! O-point
    cpo % global_param % mag_axis % position % r = xlpcntr(1)
    cpo % global_param % mag_axis % position % z = xlpcntr(2)

  end subroutine fillEquilibriumCpo

#endif

!======================================================================
      end
