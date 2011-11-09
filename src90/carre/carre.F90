
PROGRAM CARRE
  !======================================================================
  !
  !  version : 1.01.99 15:49
  !
  !======================================================================
  !ank -- The comments are translated from French, sorry for errors!

  !..  Ce programme permet de trouver la localisation des points X
  !  pour un tokamak. Il parametrise ensuite les separatrices qui passent
  !  par ces points X et il les trace ainsi que les structures reelles.
  !  Une maille curviligne orthogonale alignee sur les surfaces de flux
  !  est alors construite.

  !*** This program builds an orthogonal curvilinear mesh aligned to the
  !*** flux surfaces.  It finds the localisation of the X-points,
  !*** parametrises the separatrices passing through them, and traces
  !*** them together with the real structures.
  !======================================================================
  use CarreDiagnostics
  use CarreSiloIO
  use carre_types
  use carre_main
  use carre_postprocess
  use carre_equilibrium
  use carre_parameter_io
  use Logging
  use Helper

  IMPLICIT NONE

  !  a modifier pour generaliser la geometrie:
  !  modifier argsep et maille pour utiliser un critere plus general que
  !  la droite ou la gauche dans la designation des separatrices.
  !  Aussi, distribuer les points de facon proportionelle, et les ortho-
  !  gonaliser par relaxation, avec une protection de proximite.

  !  a faire:
  !        1. permettre l'imposition de points fixes ou de coins


  !  variables.

  !ank-970702: moved dimensions to the CARREDIM.F
#include <CARREDIM.F>
#include <COMLAN.F>


  type(CarreParameters) :: par
  type(CarreEquilibrium) :: equ
  type(CarreStructures) :: struct
  type(CarreGrid) :: grid

  ! diagonstic output
  type(CarreDiag) :: diag

  !..Procedures
  !
!!$      INTRINSIC ABS,MIN,index
!!$      EXTERNAL pltini,pltend,cadre,motifs,DERIVE,cntour,GRAD0, & 
!!$     &   SELPTX,SPTRIS,ARGSEP,FRTIER,MAILLE,trace,trace2,entete, & 
!!$     &     trc_stk_in,trc_stk_out, & 
!!$     &     virtualtargets,virtuallimiters
  !======================================================================
  !.. nxmax,nymax: maximum number of the data points in x and y
  !.. gradmx: maximum number of points where the gradient vanishes
  !.. npxmx : maximum number of the X-points
  !.. nbdmx : maximum number of structures being the divertor targets
  !.. strumx: maximum number of structures
  !.. npstmx: maximum number of points per structure
  !.. npnimx: maximum number of tracing points on a curve
  !.. nivmx : maximum number of limiting level lines
  !.. npmamx: maximum number of grid points in poloidal direction
  !.. nrmamx: maximum number of grid points in radial direction
  !.. nregmx: maximum number of regions
  !.. nrelax: maximum number of iterations in the relaxation procedure
  !           of construction of the orthogonal grid
  !.. relax : relaxation parameter
  !.. stpmin: minimum tolerable distance between any two grid points
  !           in the course of relaxation
  !.. rlcept: convergence criterion in the relaxation procedure
  !
  ! For description of diagnostic output, see CarreDiagnostic.F90.
  !
  !======================================================================
  !

  ! Initialize & read carre input from file
  call carre_init(equ, struct, par)

  call writeGridStateToSiloFile('carrePreProcA00', equ, struct, grid)

!!$  call extend_equilibrium(equ, par, struct)
!!$
!!$  !call extend_equilibrium(equ, -0.14d0, huge(0.0d0), -huge(0.0d0), huge(0.0d0), -huge(0.0d0), 1.2d0, 40, 40, 0, 40)
!!$  call extend_equilibrium(equ, -huge(0.0d0), huge(0.0d0), -huge(0.0d0), huge(0.0d0), -huge(0.0d0), 1.2d0, 40, 40, 0, 40)
!!$  !call extend_equilibrium(equ, -0.14d0, huge(0.0d0), -huge(0.0d0), huge(0.0d0), -huge(0.0d0), huge(0.0d0), 40, 40, 0, 40)

  call writeGridStateToSiloFile('carrePreProcB00', equ, struct, grid)

  ! Create the grid
  call carre_main_computation(equ, struct, par, grid, diag)

  ! Postprocess
  call carre_postprocess_computation(par, equ, grid, struct)

  ! Output grid data piece into carre.out
!!$  CALL SORTIE(equ%nsep,grid%nreg,grid%np1, & 
!!$       & equ%distxo,grid%xmail,grid%ymail,equ%nx,equ%ny, & 
!!$       & equ%x, equ%y, equ%a00, equ%a10, equ%a01, equ%a11,&
!!$       & equ%ptx, equ%pty, equ%npx, equ%racord,&
!!$       & 2,& ! 2 means print grid data
!!$       & equ%fctpx,diag,par,grid%psim,grid%psidxm,grid%psidym)
  call SORTIE(equ, grid, diag, par, 2)

  ! Output final grid to Silo
  call writeGridStateToSiloFile('carreFinal00000', equ, struct, grid)

  ! Finalize Carre 
  call carre_finalize(equ, struct, grid)

  STOP

contains

  subroutine carre_init(equ, struct, par)
    implicit none

    type(CarreParameters), intent(out) :: par
    type(CarreEquilibrium), intent(out) :: equ
    type(CarreStructures), intent(out) :: struct

    ! internal
    REAL*8 zero,rmax,zmax
    PARAMETER ( zero=0.)
    INTEGER i, j, ierror, iflag           
    character lign80*80

    ! Set default loglevel (will be set again after reading carre.dat)
    call setLogLevel(par%logLevel)

    !..1.0  Initialisation des variables par defaut et de la bibliotheque
    !       graphique
    !
    call defaut

    CALL pltini
    CALL cadre
    CALL motifs

    !
    !..2.0  Open the data files
    !
    OPEN(UNIT=7, FILE='rzpsi.dat', STATUS='old')
    OPEN(UNIT=8, FILE='structure.dat', STATUS='old')
    OPEN(UNIT=9, FILE='carre.dat', STATUS='unknown')
    open(unit=11,status='scratch')

    ! Open the output file
    call carre_open_output_file()

    !
    !..3.0  Read the data
    !

    !..Read the values of x

100 format(a)
    iflag=-1
    rewind(7)
    rewind(8)
    rewind(9)
    call entete(7,'$r',iflag)
    read(7,100)lign80
    i=index(lign80,'=')
    call rdfrin(11,lign80(i+1:80),equ%nx,ierror)
    READ(7,*) (equ%x(i), i=1, equ%nx)

    !..Read the values of y

    call entete(7,'$z',iflag)
    read(7,100)lign80
    i=index(lign80,'=')
    call rdfrin(11,lign80(i+1:80),equ%ny,ierror)
    READ(7,*) (equ%y(i), i=1, equ%ny)

    rmax = 0.0
    do i = 1, equ%nx-1
            rmax = max(rmax, (equ%x(i+1)-equ%x(i))**2)
    enddo
    zmax = 0.0
    do i = 1, equ%ny-1
            zmax = max(zmax, (equ%y(i+1)-equ%y(i))**2)
    enddo
    equ%eps_Xpt = sqrt(rmax+zmax)

    !..Read the values of psi

    call entete(7,'$psi',iflag)
    READ(7,*) ((equ%psi(i,j), i=1, equ%nx), j=1, equ%ny)

    DO j=1,equ%ny
            DO i=1,equ%nx
                    equ%psi(i,j) = equ%psi(i,j) + MIN(par%cstlin*(equ%y(j)-equ%y(equ%ny/2)),zero)
            end DO
    end DO
    !---
    !  ceci sert a modifier la symetrie haut-bas de psi, de facon ad hoc.
    !     write(6,*)'facteur de symetrie haut-bas'
    !     read(5,*)a00(1,1,1)
    !     do j=1,ny/2
    !     do i=1,nx
    !       xpto=psi(i,j)
    !       ypto=psi(i,ny-j+1)
    !       psi(i,j)=0.5*((1.+a00(1,1,1))*xpto+(1.-a00(1,1,1))*ypto)
    !       psi(i,ny-j+1)=0.5*((1.+a00(1,1,1))*ypto+(1.-a00(1,1,1))*xpto)
    !     enddo
    !     enddo
    !---
    close(unit=7)

    !
    !..3.1  Read the structures.
    !
    call listru(8,struct)!,nstruc,npstru,nomstr,xstruc,ystruc,npstmx,strumx)

    call csioGetStructureSegments( struct%nstruc, struct%npstru, &
         & struct%xstruc, struct%ystruc, csioStrucNSeg, csioStrucSegments )

    ! Read carre.dat input file the first time to get basic setup parameters
    call read_code_parameters_noninteractive(par)
    ! The carre.dat input file is going to be read again, so we have to rewind it
    rewind(9)

    ! Set loglevel as given in parameters
    call setLogLevel(par%logLevel)

  end subroutine carre_init


  subroutine carre_finalize(equ, struct, grid)
    type(CarreEquilibrium), intent(in) :: equ
    type(CarreStructures), intent(in) :: struct
    type(CarreGrid), intent(in) :: grid


    !*
    !* WARNINGS CALCULATION AND OUTPUT
    !*
    !        if (npx.EQ.1) CALL WARNINGS(separx,separy,nivx,nivy,
    !     &  distnv(1,2),xpto,ypto)

    !
    !..13.0  Plot the structures, the separatrices, and the limiting level
    !        lines for each region, together with the resulting grid
    !
    if(equ%npx.gt.0) then

            call trace(equ%x(1),equ%x(equ%nx),equ%y(1),equ%y(equ%ny),equ%separx,equ%separy,&
                 & equ%ptsep,equ%npx,equ%nptot, & 
                 &           struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc, & 
                 &           struct%nivx,struct%nivy,struct%nivtot,struct%nbniv,&
                 & grid%np1,par%npr,grid%xmail,grid%ymail,grid%nreg)

    endif ! nptx.gt.0

    !
    !.. Close the graphics
    !
    CALL pltend

    call csioCloseFile()

  end subroutine carre_finalize


end PROGRAM CARRE
