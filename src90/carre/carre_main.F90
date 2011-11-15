module carre_main

  use carre_types
  use CarreDiagnostics
  use Helper
  use carre_virtualstructures
  use carre_equilibrium
  use carre_postprocess ! for writeSilo...
#ifdef USE_SILO
  use SiloIO
  use CarreSiloIO
#endif

  implicit none

#include <CARREDIM.F>

  private

  public carre_main_computation, carre_open_output_file

  ! Virtual structure setup loop steps
  integer, parameter :: ORIGINAL_STRUCT_STEP = 1
  integer, parameter :: VIRTUAL_STRUCT_STEP = 2

  ! Equilibrium extension loop steps
  integer, parameter :: GIVEN_EQU_STEP = 1
  integer, parameter :: EXTEND_EQU_STEP = 2

contains

  subroutine carre_open_output_file()
    OPEN(UNIT=10,FILE='carre.out',STATUS='unknown')
    rewind(10)
    !  2.1  specify output format
    write(10,*)'output format:'//CARRE_VERSION
  end subroutine carre_open_output_file

  subroutine carre_main_computation(equ, struct, par, grid, diag)

    type(CarreParameters), intent(inout) :: par
    type(CarreEquilibrium), intent(inout) :: equ
    type(CarreStructures), intent(inout) :: struct
    type(CarreGrid), intent(out) :: grid
    type(CarreDiag), intent(out) :: diag

    ! internal
    integer :: i, j, itmp
    integer :: iSetupStruct, nEquSteps, iEquStep


    REAL*8, PARAMETER :: stp0=0.01, stpmin=0.001
    !
    !..4.0  Calculate the first partial derivatives in x and y and store
    !       them in arrays psidx and psidy

    CALL DERIVE(equ)

!!$    !
!!$    !..5.0  Plot the level lines for psidx=0 and psidy=0
!!$    !
!!$    CALL cntour(equ%psidx,equ%psidy,equ%nx,equ%ny,&
!!$        & equ%x(1),equ%x(equ%nx),equ%y(1),equ%y(equ%ny))

    !
    !  interpolation coefficients for psi and its derivatives

    call inipsi(equ,nxmax,nymax)

    !
    !..6.0  Determine the points where the derivatives in x and y vanish
    !
    CALL GRAD0(equ, nxmax,nymax,gradmx)

    !
    !..7.0  Select the X-points of interest from the original equilibrium data
    !
    CALL SELPTX(equ%npxtot,equ%npx,equ%pointx,equ%pointy,equ%ii,equ%jj,equ%ptx, & 
         & equ%pty,equ%iptx,equ%jptx,equ%xpto,equ%ypto,equ%racord,equ%limcfg,&
         & par)

    ! When using virtual targets, needs two passes through the analysis steps that 
    ! find and arrange the separatrix pieces and targets
    do iSetupStruct = ORIGINAL_STRUCT_STEP, VIRTUAL_STRUCT_STEP

       ! When using equilibrium extension, on the first pass through the structure loop
       ! we do the geometry analysis twice: first for the original equilibrium, then 
       ! the psi data is modified, then a second pass is done to update the data.
       ! This allows modifications to the equilibrium data that change psi inside the vessel
       ! (in case you want to do that, at your own risk).

       if (par%equExtensionMode == EQU_EXTENSION_OFF) then 
          ! Use default equlibrium data
          nEquSteps = GIVEN_EQU_STEP
       else
          ! Extend equilibrium data: two passes
          nEquSteps = EXTEND_EQU_STEP
       end if
       ! Only do two passes when currently working with original structures
       if (iSetupStruct /= ORIGINAL_STRUCT_STEP) nEquSteps = GIVEN_EQU_STEP

       ! Equilibrium setup loop
       do iEquStep = GIVEN_EQU_STEP, nEquSteps

          ! We do the equilibrium extension on the beginning of the second
          ! pass through the equilibrium setup loop
          if (iEquStep == EXTEND_EQU_STEP) then
             call extend_equilibrium(equ, par, struct)

             ! Before running the geometry & topology analysis again,
             ! we have to reset the related state variables
             call resetGeometryAndTopologyData()
          end if

          !..8.0  Parametrise the separatrices
          IF (equ%npx.GT.0 .and. equ%limcfg.eq.0) THEN

             ! Configuration with x-points

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

             ! Limiter configuration
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

                call writeGridStateToSiloFile('carreFrtierA000', equ, struct)

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

          end if

       end do ! end equilibrium loop

       ! At this point the equilibrium and topology data is in the final form. 

       ! If no virtual targets are to be created, exit loop and go directly to grid generation           
       if (par%gridExtensionMode == GRID_EXTENSION_OFF) then
          exit
       else           
          ! Otherwise, if we have a case with x-points, set up virtual geometry
          if (equ%npx.gt.0)then

             ! If we arrive here the second time, the virtual targets have been created and the
             ! setup for the grid generation was done for them. Exit here and go directly to grid generation.
             if ( iSetupStruct == VIRTUAL_STRUCT_STEP ) exit

             !..   10.1  Set up virtual targets

             !..      Save current structures
             struct%rnstruc = struct%nstruc
             struct%rnpstru = struct%npstru
             struct%rxstruc = struct%xstruc
             struct%rystruc = struct%ystruc

             !..   10.1  Set up virtual structures

             call setupVirtualStructures(par, equ, struct)
          end if                     ! npx.gt.0
       end if

    end do                     ! end setup loop

    !
    !..12.0  Grid the regions
    !
    if(equ%npx.gt.0) then

       call writeGridStateToSiloFile('carreMaille0000', equ, struct)
       call maille(equ,struct,grid,diag,par)

    end if


  contains

    subroutine resetGeometryAndTopologyData()
      struct%nbdef = 0
      struct%nbniv = 0
      equ%fctpx = 0.0d0
      struct%inddef = 0
      struct%nivtot = 0

      equ%nptot = 0
      struct%indplq = 0
      equ%ptsep = 0
      equ%ptxint = 0
    end subroutine resetGeometryAndTopologyData

  end subroutine carre_main_computation

end module carre_main
