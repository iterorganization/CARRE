module carre_main

  use KindDefinitions
  use carre_types
  use CarreDiagnostics
  use Helper
  use carre_virtualstructures
  use carre_equilibrium
  use carre_postprocess ! for writeSilo...
  use trc_stk_mod
#ifdef USE_SILO
  use SiloIO
#endif
  use CarreSiloIO
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
    integer :: i, j, itmp, iTarget, iret
    integer :: iSetupStruct, nEquSteps, iEquStep

    REAL(rKind), PARAMETER :: stp0=0.01, stpmin=0.001

    ! external routines
    external derive, inipsi, grad0, sptris, argsep, limfnd, frtier, maille
    external trace2, selptx, wreqvr

    !
    !..4.0  Calculate the first partial derivatives in x and y and store
    !       them in arrays psidx and psidy

    CALL DERIVE(equ)

    !
    !  interpolation coefficients for psi and its derivatives
    !
    call inipsi(equ,nxmax,nymax)

    !
    !..6.0  Determine the points where the derivatives in x and y vanish
    !
    CALL GRAD0(equ)

    !
    !..7.0  Select the X-points of interest from the original equilibrium data
    !
    CALL SELPTX(equ%npxtot,equ%npx,equ%pointx,equ%pointy,equ%ii,equ%jj,equ%ptx, &
         & equ%pty,equ%iptx,equ%jptx,equ%xpto,equ%ypto,equ%racord,equ%limcfg,&
         & par)

    ! Short sanity check: virtual structures supplied by user only make sense in CARRE_EXTENDED(_UNSTRUCTURED) mode
    if (par%nVirtualStructs > 0 .and. par%carreMode /= CARRE_EXTENDED .and. par%carreMode /= CARRE_EXTENDED_NONORTHOGONAL) then
        stop "carre_main: you cannot submit virtual structures when not doing extended grid"
    end if

    ! In CARRE_STANDARD mode, we use the real structures and do one pass - no special setup required
    ! In CARRE_STANDARD_VIRTUALSTRUCT mode, we use the real structures and do two passes - no special setup required
    ! In CARRE_EXTENDED(_UNSTRUCTURED) mode
    ! -if virtual structures supplied, use these and only do one pass
    ! -if no virtual structures supplied, use real structure and do two passes

    if (par%nVirtualStructs > 0 .and. (par%carreMode == CARRE_EXTENDED .or. par%carreMode == CARRE_EXTENDED_NONORTHOGONAL)) then
        ! set up virtual structures for first pass
        struct%nstruc = struct%vnstruc
        struct%npstru(1:struct%vnstruc) = struct%vnpstru(1:struct%vnstruc)
        struct%xstruc(:,1:struct%vnstruc) = struct%vxstruc(:,1:struct%vnstruc)
        struct%ystruc(:,1:struct%vnstruc) = struct%vystruc(:,1:struct%vnstruc)
        struct%closed(1:struct%vnstruc) = struct%vclosed(1:struct%vnstruc)
    end if

    ! When asked to create virtual targets, needs two passes through the analysis steps that
    ! find and arrange the separatrix pieces and targets
    ! First pass - with real structures: identify target plates, limiting level lines, create virtual structures (if required)
    ! Second pass pass - with virtual structures: identify target plates, limiting level lines
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

             ! TODO: output modified equilibrium (use service routines from dg tools)
          end if

          !..8.0  Parametrise the separatrices
          IF (equ%npx.GT.0 .and. equ%limcfg.eq.0) THEN

             ! Configuration with X-points

             CALL SPTRIS(equ%nx,equ%ny,equ%x,equ%y,equ%psi,equ%npx,equ%ptx,equ%pty, &
                  &      equ%iptx,equ%jptx,equ%fctpx,equ%separx,equ%separy,equ%nptot, &
                  &      struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc,&
                  &      struct%indplq,struct%inddef,struct%nbdef, &
                  &      equ%a00,equ%a10,equ%a01,equ%a11, struct)

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
                  & equ%a00,equ%a10,equ%a01,equ%a11,struct,equ, iSetupStruct==VIRTUAL_STRUCT_STEP)

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
             write(0,'(a5,1p,8e12.4)') ' ptx:',equ%ptx(1:equ%npx)
             write(0,'(a5,1p,8e12.4)') ' pty:',equ%pty(1:equ%npx)
             if(equ%limcfg.eq.0) then
                write(0,*) 'nptot(4,nxpoints)'
                write(0,'(1x,16i5)') ((equ%nptot(i,j),i=1,4),j=1,equ%npx)
                write(0,*) 'Strike points (presumably)'
                write(0,'(a3,1p,8e12.4)') ' x:',&
                     &     ((equ%separx(equ%nptot(i,j),i,j),i=1,4),j=1,equ%npx)
                write(0,'(a3,1p,8e12.4)') ' y:',&
                     &     ((equ%separy(equ%nptot(i,j),i,j),i=1,4),j=1,equ%npx)
                !>>>
                call trc_stk_in('carre   ','*..10.0 ')

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

             call trace2(equ%x(1),equ%x(equ%nx),equ%y(1),equ%y(equ%ny),&
                  &      equ%separx,equ%separy,equ%ptsep,equ%npx,equ%nptot, &
                  &      struct%nstruc,struct%npstru, &
                  &      struct%xstruc,struct%ystruc, &
                  &      struct%nivx,struct%nivy,struct%nivtot,struct%nbniv)

          end if

       end do ! end equilibrium loop

       ! At this point the equilibrium and topology data is in the final form.
       if ( par%carreMode == CARRE_STANDARD ) then
           ! If no virtual targets are to be created, exit loop and go directly to grid generation
           exit
       elseif ( par%nVirtualStructs > 0 ) then
           ! virtual structures were supplied (only possible in CARRE_EXTENDED mode), do not create them
           exit
       else
          ! If we arrive here the second time, the virtual targets have been created and the
          ! setup for the grid generation was done for them. Exit here and go directly to grid generation.
          if ( iSetupStruct == VIRTUAL_STRUCT_STEP ) exit

          if (equ%limcfg /= 0) then
             call setupVirtualLimiterGeometry(par, equ, struct)
          else if (equ%npx > .0) then
             ! if we have a case with X-points, set up virtual geometry
             !..   10.1  Set up virtual targets/structures
             call setupVirtualStructures(par, equ, struct)

             ! mark the real target structures as to be defined
             do iTarget = 1, struct%nbdef
                struct%refineAtStructure(struct%inddef(iTarget)) = .true.
             end do
          end if

          ! They are only actually used in extended grid mode
          if ( par%carreMode == CARRE_EXTENDED .or. par%carreMode == CARRE_EXTENDED_NONORTHOGONAL ) then
             struct%nstruc = struct%vnstruc
             struct%npstru = struct%vnpstru
             struct%xstruc = struct%vxstruc
             struct%ystruc = struct%vystruc
             struct%closed = struct%vclosed
          end if

          call writeVirtualStructuresToFile( struct )
       end if

       call writeGridStateToSiloFile('carreVirtua0000', equ, struct)

    end do                     ! end setup loop

    !
    !..12.0  Grid the regions
    !
    if(equ%npx.gt.0) then

       call writeGridStateToSiloFile('carreMaille0000', equ, struct)
       call maille(equ,struct,grid,diag,par)
       call writeGridStateToSiloFile('carreMailleF000', equ, struct, grid)

    end if

    if (.not. (par%equExtensionMode == EQU_EXTENSION_OFF)) then
       ! If equilibrium has been extended then we re-write it
       open(2,file='rzpsi_ext.dat')
       call wreqvr(2,nxmax,iret,equ%nx,equ%ny,equ%x,equ%y,equ%psi)
       if(iret.ne.0) then
          write(*,*) 'carre_main: error writing the extended Carre equilibrium file'
          stop
       end if
       close(2)
    endif

    call carre_postprocess_computation(par, equ, grid, struct)

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
