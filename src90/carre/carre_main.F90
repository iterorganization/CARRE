module carre_main

  use carre_types
  use CarreDiagnostics
  use SiloIO
  use CarreSiloIO
  use itm_string

  implicit none

#include <CARREDIM.F>

  private

  public carre_main_computation, carre_postprocess

contains


  subroutine carre_main_computation(equ, struct, par, grid, diag)

    type(CarreParameters), intent(inout) :: par
    type(CarreEquilibrium), intent(inout) :: equ
    type(CarreStructures), intent(inout) :: struct
    type(CarreGrid), intent(out) :: grid
    type(CarreDiag), intent(out) :: diag


    ! internal
    integer :: isetup, i, j, itmp, is, ip


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

            CALL VIRTUALTARGETS(equ%nx,equ%ny,equ%x,equ%y,equ%psi,equ%npx,equ%ptx,equ%pty, & 
                &       equ%fctpx,equ%separx,equ%separy,equ%nptot, & 
                &       struct%rnstruc,struct%rnpstru,struct%rxstruc,struct%rystruc,&
                &       struct%indplq,struct%inddef,struct%nbdef, & 
                &       equ%a00,equ%a10,equ%a01,equ%a11,&
                &       struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc)

            !..   10.2  Set up virtual limiters

            call VIRTUALLIMITERS(struct%nivx,struct%nivy,struct%nivtot,struct%nbniv,&
                & equ%npx,equ%ptx,equ%pty, & 
                & struct%nstruc,struct%npstru,struct%xstruc,struct%ystruc)

            !..   10.2.1 Diagnostics: Write out resulting structures

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


  
  subroutine carre_postprocess(par, grid, struct)
    
    type(CarreParameters), intent(in) :: par
    type(CarreGrid), intent(inout) :: grid
    type(CarreStructures), intent(in) :: struct

    ! internal
    integer :: iReg, iPol, iRad, iFace
    double precision :: xx(2), yy(2)
    integer :: faceLabel(npmamx-1,nrmamx-1,nregmx)

    ! Identify cells that are intersected by vessel structure

    faceLabel = 0
    grid%cellflag = GRID_UNDEFINED

    do iReg = 1, grid%nreg
            
            do iPol = 1, grid%np1(iReg) - 1
                    do iRad = 1, par%npr(iReg) - 1

                            do iFace = 1, 4

                                    ! fill xx, yy
                                    call getFace(iFace)

                                    call intersect_structure( xx, yy, &
                                         & struct, grid%faceISec(iFace, iPol, iRad, iReg), &
                                         & ipx = grid%faceISecPx(iFace, iPol, iRad, iReg), &
                                         & ipy = grid%faceISecPy(iFace, iPol, iRad, iReg) )
                                    
                                    if (grid%faceISec(iFace, iPol, iRad, iReg)) then
                                            faceLabel(iPol, iRad, iReg) = &
                                                 & faceLabel(iPol, iRad, iReg) + 2 ** (iFace - 1)
                                    end if
                            end do

                    end do
            end do
    end do         

    ! Translate the facelabel array into cell flags
    ! First assume all boundary cells are problematic
    where (facelabel /= GRID_UNDEFINED) grid%cellflag = GRID_BOUNDARY_REFINE
    ! Then figure out which ones are ok (the ones where opposite faces
    ! are intersected)
    where (facelabel == 5) grid%cellflag = GRID_BOUNDARY
    where (facelabel == 10) grid%cellflag = GRID_BOUNDARY
   
    ! Mark cells to be inside/outside of vessel

    ! For region, find a cell next to x-point (which will be on the inside)
    ! Walk away from there (recursively?) until all cells are marked
    ! For limiter, just take a cell on the core boundary.
    !call labelCells(par, equ, grid)    

    ! write results
    call csioOpenFile('carrePostProces')

    do iReg = 1, grid%nreg
            call siloWriteQuadGrid( csioDbfile, 'region'//int2str(iReg), &
                 & grid%np1(iReg), par%npr(iReg), &
                 & grid%xmail(1:grid%np1(iReg), 1:par%npr(iReg), iReg), &
                 & grid%ymail(1:grid%np1(iReg), 1:par%npr(iReg), iReg) )

            call siloWriteQuadData( csioDbfile, 'region'//int2str(iReg), &
                 & 'facelabel'//int2str(iReg), &
                 & real(faceLabel(1:grid%np1(iReg)-1, 1:par%npr(iReg)-1, iReg),rKind), &
                 & DB_ZONECENT )
            call siloWriteQuadData( csioDbfile, 'region'//int2str(iReg), &
                 & 'cellflag'//int2str(iReg), &
                 & real(grid%cellflag(1:grid%np1(iReg)-1, 1:par%npr(iReg)-1, iReg),rKind), &
                 & DB_ZONECENT )
    end do


  contains

    subroutine getFace(iFace)
      integer, intent(in) :: iFace

      select case (iFace)
      case(1) ! Left face of cell
              xx(1) = grid%xmail(iPol, iRad, iReg)
              yy(1) = grid%ymail(iPol, iRad, iReg)
              xx(2) = grid%xmail(iPol, iRad + 1, iReg)
              yy(2) = grid%ymail(iPol, iRad + 1, iReg)
      case(2) ! Bottom face of cell
              xx(1) = grid%xmail(iPol, iRad, iReg)
              yy(1) = grid%ymail(iPol, iRad, iReg)
              xx(2) = grid%xmail(iPol + 1, iRad, iReg)
              yy(2) = grid%ymail(iPol + 1, iRad, iReg)              
      case(3) ! Right face of cell
              xx(1) = grid%xmail(iPol + 1, iRad, iReg)
              yy(1) = grid%ymail(iPol + 1, iRad, iReg)
              xx(2) = grid%xmail(iPol + 1, iRad + 1, iReg)
              yy(2) = grid%ymail(iPol + 1, iRad + 1, iReg)
      case(4) ! Top face of cell
              xx(1) = grid%xmail(iPol, iRad + 1, iReg)
              yy(1) = grid%ymail(iPol, iRad + 1, iReg)
              xx(2) = grid%xmail(iPol + 1, iRad + 1, iReg)
              yy(2) = grid%ymail(iPol + 1, iRad + 1, iReg)
      end select
      
    end subroutine getFace

    subroutine intersect_structure(xx, yy, struct, doesIntersect, iStruct, &
         & iSegment, ipx, ipy)

      REAL*8, intent(in) :: xx(2),yy(2)     
      type(CarreStructures), intent(in) :: struct
      logical, intent(out) :: doesIntersect
      integer, intent(out), optional :: iSegment, iStruct
      REAL*8, intent(out), optional :: ipx, ipy

      ! internal      
      integer :: is

      do is = 1, struct%rnstruc
              call intersect(xx, yy, &
                   & struct%rxstruc(1:struct%rnpstru(is), is), &
                   & struct%rystruc(1:struct%rnpstru(is), is), &
                   & doesIntersect, iSegment, ipx, ipy)
              if (present(iStruct)) iStruct = is
              if (doesIntersect) return
      end do

      doesIntersect = .false.
      if (present(iStruct)) iStruct = GRID_UNDEFINED

    end subroutine intersect_structure

  end subroutine carre_postprocess

  
  subroutine intersect(xx,yy,xst,yst,doesIntersect,iSegment,ipx,ipy)

    !..  Cette fonction verifie si un segment traverse un segment de
    !  structure autre que celui sur lequel on est en train de marcher.

    !  arguments
    REAL*8, intent(in) :: xx(2),yy(2),xst(:),yst(:)
    
    logical, intent(out) :: doesIntersect
    integer, intent(out), optional :: iSegment
    REAL*8, intent(out), optional :: ipx, ipy

    !  variables locales
    INTEGER i
    REAL*8 mult1,mult2,determ

    !=========================
    !.. xst,yst: tableaux des coordonnees des points de la structure.
    !.. n  : nombre de points de la structure.
    !.. xx,yy: tableaux des coordonnees contenant les 2 points du segment.
    !.. ind: indice du segment de structure.
    !.. determ: determinant de la matrice des deux equations.
    !.. mult1: facteur multiplicatif du segment de courbe.
    !.. mult2: facteur multiplicatif du segment de structure.
    !=========================

    !..Boucle sur la structure.
    DO i=1, size(xst)-1
            !..Calcul du determinant de la matrice.
            determ = (-(xx(2) - xx(1))) * (yst(i+1) - yst(i)) + & 
                 &                   (yy(2) - yy(1)) * (xst(i+1) - xst(i))
            
            !..Si determinant non nul, alors il y a solution.            
            IF (determ .NE. 0.) THEN

                    !..Facteur multiplicatif du segment de courbe avec la methode de Cramer.                    
                    mult1 = ((-(xst(i)-xx(1))) * (yst(i+1)-yst(i)) + & 
                         &              (yst(i)-yy(1)) * (xst(i+1)-xst(i)))/determ
                    
                    !..Pour avoir intersection, il faut que mult1 soit entre 0 et 1
                    IF ((mult1.GT.0.).AND.(mult1.LT.1.)) THEN

                            !..Fact. mult. du segment de structure.
                            mult2= ((xx(2)-xx(1)) * (yst(i)-yy(1)) - & 
                                 &                (yy(2)-yy(1)) * (xst(i)-xx(1)))/determ

                            !..Intersection si mult2 entre 0 et 1
                            IF ((mult2.GT.0.).AND.(mult2.LT.1.)) THEN
                                    doesIntersect = .true.
                                    if (present(iSegment)) iSegment = i
                                    if (present(ipx) .and. present(ipy)) then
                                            ipx = xx(1) + mult1 * (xx(2) - xx(1))
                                            ipy = yy(1) + mult1 * (yy(2) - yy(1))
                                    end if
                                    return
                            ENDIF
                    ENDIF
            ENDIF
    END DO
    
    doesIntersect = .FALSE.
    if (present(iSegment)) iSegment = GRID_UNDEFINED
  END subroutine intersect

end module carre_main
