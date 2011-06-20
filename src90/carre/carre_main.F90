module carre_main

  use carre_types
  use CarreDiagnostics
  use itm_string
  use carre_virtualstructures
#ifdef USE_SILO
  use SiloIO
  use CarreSiloIO
#endif

  implicit none

#include <CARREDIM.F>

  private

  public carre_main_computation

contains


  subroutine carre_main_computation(equ, struct, par, grid, diag)

    type(CarreParameters), intent(inout) :: par
    type(CarreEquilibrium), intent(inout) :: equ
    type(CarreStructures), intent(inout) :: struct
    type(CarreGrid), intent(out) :: grid
    type(CarreDiag), intent(out) :: diag


    ! internal
    integer :: isetup, i, j, itmp, is, ip, iLine


    REAL*8, PARAMETER :: stp0=0.01, stpmin=0.001

    logical dovirtualtargets
    parameter (dovirtualtargets=.true.)


    !
    !..4.0  Calculate the first partial derivatives in x and y and store
    !       them in arrays psidx and psidy

    CALL DERIVE(equ)

    !
    !..5.0  Plot the level lines for psidx=0 and psidy=0
    !
    CALL cntour(equ%psidx,equ%psidy,equ%nx,equ%ny,&
        & equ%x(1),equ%x(equ%nx),equ%y(1),equ%y(equ%ny))
    !
    !  interpolation coefficients for psi and its derivatives

    call inipsi(equ,nxmax,nymax)

    !
    !..6.0  Determine the points where the derivatives in x and y vanish
    !

    CALL GRAD0(equ, nxmax,nymax,gradmx)

    !
    !..7.0  Select the X-points of interest
    !

    !ank-970702: moved dimensions to the included file
    CALL SELPTX(equ%npxtot,equ%npx,equ%pointx,equ%pointy,equ%ii,equ%jj,equ%ptx, & 
        &            equ%pty,equ%iptx,equ%jptx,equ%xpto,equ%ypto,equ%racord,equ%limcfg)

    !     when using virtual targets, needs two passes through the setup
    !     steps 8 to 10.

    do isetup = 1, 2
        !
        !..8.0  Parametrise the separatrices
        !
        IF (equ%npx.GT.0 .and. equ%limcfg.eq.0) THEN
            !
            CALL SPTRIS(equ%nx,equ%ny,equ%x,equ%y,equ%psi,equ%npx,equ%ptx,equ%pty, & 
                &      equ%iptx,equ%jptx,equ%fctpx,equ%separx,equ%separy,equ%nptot, & 
                &      struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc,&
                &      struct%indplq,struct%inddef,struct%nbdef, & 
                &      equ%a00,equ%a10,equ%a01,equ%a11)

            !
            !..9.0  Arrange the separatrices
            !
            CALL ARGSEP(equ%npx,equ%ptx,equ%pty,equ%fctpx,equ%separx,equ%separy,&
                & struct%indplq,equ%nptot,npnimx, & 
                & equ%ptsep,equ%racord,equ%ptxint,equ%ypto,struct%nbdef,struct%inddef,&
                & equ%eps_Xpt)

        ELSEIF(equ%LIMCFG.NE.0) THEN
            !
            !  13.   Identify the limiter
            !
            call limfnd(equ%xpto,equ%ypto,struct%nivx,struct%nivy,stp0,stpmin,&
                & struct%distnv,struct%nivtot, & 
                & struct%nbniv,equ%nx,equ%ny,equ%x,equ%y,equ%psi,&
                & equ%npx,equ%ptx,equ%pty,equ%fctpx, & 
                & struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc,&
                & struct%indplq,struct%inddef,struct%nbdef, & 
                & equ%a00,equ%a10,equ%a01,equ%a11)

            do itmp=1,4
                equ%ptsep(itmp,1) = 0
            enddo

        ENDIF

        if(equ%npx.gt.0) then
            !
            !..10.0  Find the level lines in more detail
            !
            !<<<
            write(0,*) '=== carre *..10.0 - before frtier'
            write(0,'(5h ptx:,1p,8e12.4/(5x,8e12.4))') equ%ptx(1:equ%npx)
            write(0,'(5h pty:,1p,8e12.4/(5x,8e12.4))') equ%pty(1:equ%npx)
            if(equ%limcfg.eq.0) then
                write(0,*) 'nptot(4,nxpoints)'
                write(0,'(1x,16i5)') ((equ%nptot(i,j),i=1,4),j=1,equ%npx)
                write(0,*) 'Strike points (presumably)'
                write(0,'(3h x:,1p,8e12.4/(3x,8e12.4))') & 
                    &     ((equ%separx(equ%nptot(i,j),i,j),i=1,4),j=1,equ%npx)
                write(0,'(3h y:,1p,8e12.4/(3x,8e12.4))') & 
                    &     ((equ%separy(equ%nptot(i,j),i,j),i=1,4),j=1,equ%npx)
                !>>>
                call trc_stk_in('carre','*..10.0')
                CALL FRTIER(equ%nx,equ%ny,equ%x,equ%y,equ%psi,struct%nstruc, & 
                    &      struct%npstru,struct%xstruc,struct%ystruc,struct%inddef,&
                    &      struct%nbdef,equ%npx,equ%separx, & 
                    &      equ%separy,equ%nptot,equ%ptsep,equ%racord,struct%nivx,&
                    &      struct%nivy,struct%nivtot, & 
                    &      struct%nbniv,stp0,stpmin, & 
                    &      struct%distnv,equ%ptxint,equ%a00,equ%a10,equ%a01,equ%a11)
                call trc_stk_out
            endif

            call trace2(equ%x(1),equ%x(equ%nx),equ%y(1),equ%y(equ%ny),equ%separx,equ%separy, & 
                &        equ%ptsep,equ%npx,equ%nptot, & 
                &        struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
                &        struct%nivx,struct%nivy,struct%nivtot,struct%nbniv)

#ifdef USE_SILO
            ! write results of first postprocessing step
            call csioOpenFile('carreLevelLines')

            ! limiting level lines
            do iLine = 1, struct%nbniv
               call siloWriteLineSegmentGridFromPoints( csioDbfile, "limlevelline"//int2str(iLine), &
                    & struct%nivx(1:struct%nivtot(iLine), iLine), &
                    & struct%nivy(1:struct%nivtot(iLine), iLine) )
            end do
#endif



            ! If no virtual targets are to be created, exit loop and go directly to grid generation           
            if ( .not. dovirtualtargets ) exit
            ! If we arrive here the second time, the virtual targets have been created and the
            ! setup for the grid generation was done for them. Exit here and go directly to grid generation.
            if ( isetup == 2 ) exit

            !..   10.1  Set up virtual targets

            !..      Save current structures
            struct%rnstruc = struct%nstruc
            struct%rnpstru = struct%npstru
            struct%rxstruc = struct%xstruc
            struct%rystruc = struct%ystruc

            struct%nstruc = 0

            !..   10.1  Set up virtual targets

            CALL VIRTUALTARGETS(equ, struct, &
                 & equ%nx,equ%ny,equ%x,equ%y,equ%psi,equ%npx,equ%ptx,equ%pty, & 
                &       equ%fctpx,equ%separx,equ%separy,equ%nptot, & 
                &       equ%a00,equ%a10,equ%a01,equ%a11 )

            !..   10.2  Set up virtual limiters

            call VIRTUALLIMITERS(struct%nivx,struct%nivy,struct%nivtot,struct%nbniv,&
                & equ%npx,equ%ptx,equ%pty, & 
                & struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc)

            !..   10.2.1 Diagnostics: Write out resulting structures
#ifdef USE_SILO
            call csioGetStructureSegments( struct%nstruc, struct%npstru, &
                 & struct%xstruc, struct%ystruc, csioVirtualStrucNSeg, csioVirtualStrucSegments )

            call csioOpenFile('carreVirtualStr')
            call csioOpenFile()
            call csioCloseFile()                         

#endif

            open(UNIT=100,FILE='virtualstructure.out',STATUS='unknown')
            do is = 1, struct%nstruc
                do ip = 1, abs(struct%npstru( is ))
                    write (100,*) struct%xstruc(ip,is)*1000, & 
                        &             struct%ystruc(ip,is)*1000
                enddo
                write (100,*) ''
            enddo
            close(UNIT=100)

        endif                     ! npx.gt.0

    enddo                     ! end setup loop

    !
    !..12.0  Grid the regions
    !
    if(equ%npx.gt.0) then

        CALL MAILLE(equ%nx,equ%ny,equ%x,equ%y,equ%psi,equ%npx,equ%xpto,equ%ypto,&
            & equ%racord, & 
            &    equ%separx,equ%separy,equ%ptsep,equ%nptot,&
            & struct%distnv,equ%ptxint,struct%nstruc,struct%npstru, & 
            &    struct%xstruc,struct%ystruc,struct%inddef,&
            & grid%nreg,grid%xn,grid%yn,grid%xmail,grid%ymail, & 
            &    grid%np1,equ%ptx,equ%pty,struct%nivx,struct%nivy,struct%nivtot,struct%nbniv, & 
            &    equ%a00,equ%a10,equ%a01,equ%a11,equ%fctpx,equ%limcfg,diag,par,&
            & grid%psim, grid%psidxm, grid%psidym)

    end if

  end subroutine carre_main_computation

end module carre_main
