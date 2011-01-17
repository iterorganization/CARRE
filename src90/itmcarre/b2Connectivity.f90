module b2Connectivity

  use KindDefinitions

  implicit none

contains

  !> Computes the standard b2 connectivity information
  !> Code taken from b2agfs.F.
  !> Only periodic_bc == 0 is supported.

  subroutine computeB2Connectivity (nx1,ny1,crx1,cry1,&
       & leftix1,leftiy1,rightix1,rightiy1, &
       & topix1,topiy1,bottomix1,bottomiy1, &
       & leftcut1,rightcut1,bottomcut1,topcut1, &
       & periodic_bc,nncut,nncutmax)

    !   ..input arguments (unchanged on exit)
    integer  nx1, ny1, nncutmax
    !   .. output arguments
    real (kind=rKind) ::&
         & crx1(-1:nx1,-1:ny1,0:3), cry1(-1:nx1,-1:ny1,0:3)
    integer ::&
         & leftix1(-1:nx1,-1:ny1),leftiy1(-1:nx1,-1:ny1),&
         & rightix1(-1:nx1,-1:ny1),rightiy1(-1:nx1,-1:ny1),&
         & topix1(-1:nx1,-1:ny1),topiy1(-1:nx1,-1:ny1),&
         & bottomix1(-1:nx1,-1:ny1),bottomiy1(-1:nx1,-1:ny1),&
         & leftcut1(nncutmax),rightcut1(nncutmax),&
         & bottomcut1(nncutmax),topcut1(nncutmax),&
         & periodic_bc,nncut

    ! internal

    integer :: ic, ix, iy, i, ixr, ixl
    logical :: found

    ! parameters
    real(rKind), parameter :: geom_match_dist = 1.0e-6_rKind

    ! statement functions
    integer :: ix1,iy1,ip1,ix2,iy2,ip2

    real(rKind) :: dist
    logical match
    dist(ix1,iy1,ip1,ix2,iy2,ip2) = &
         & sqrt((crx1(ix1,iy1,ip1)-crx1(ix2,iy2,ip2))**2+&
         & (cry1(ix1,iy1,ip1)-cry1(ix2,iy2,ip2))**2)
    match(ix1,iy1,ix2,iy2) = &
         & (dist(ix1,iy1,1,ix2,iy2,0)+dist(ix1,iy1,3,ix2,iy2,2)).lt.geom_match_dist


    nncut=0
    do ic=1,nncutmax
            bottomcut1(ic)=ny1+1
            topcut1(ic)=-2
            rightcut1(ic)=-2
            leftcut1(ic)=nx1+1
    enddo
    ic=0
    do iy=-1,ny1
            do ix=-1,nx1
                    leftix1(ix,iy)=ix-1
                    if(ix.gt.-1) then
                            if(.not.match(leftix1(ix,iy),iy,ix,iy)) then
                                    leftix1(ix,iy)=-2
                                    do ixl=-1,nx1
                                            if(match(ixl,iy,ix,iy)) then
                                                    leftix1(ix,iy)=ixl
                                                    exit
                                            endif
                                    enddo
                                    if(leftix1(ix,iy).eq.-2.and.periodic_bc.eq.0) then
                                            write(*,*) 'Left neighbour not found for ',ix,iy
                                    elseif(periodic_bc.eq.1) then 
                                            stop 'computeB2Connectivity: periodic_bc == 1 not supported'
                                    else
                                            write(*,*) 'iy,ix,ixl',iy,ix,ixl
                                            if(ic.eq.0) then
                                                    found=.false.
                                            else
                                                    found=.false.
                                                    do i = 1, ic
                                                            if(ixl+1.eq.rightcut1(i) .or. ixl+1.eq.leftcut1(i)) then
                                                                    found = .true.
                                                                    exit
                                                            endif
                                                    end do
                                            endif
                                            if (.not.found) then
                                                    ic = ic + 1
                                                    write(*,*) 'ic',ic
                                                    i = ic
                                            endif
                                            bottomcut1(i)=min(iy,bottomcut1(i))
                                            topcut1(i)=max(iy+1,topcut1(i))
                                            rightcut1(i)=max(ix,rightcut1(i))
                                    endif
                            endif
                    endif
                    rightix1(ix,iy)=ix+1
                    if(ix.lt.nx1) then
                            if(.not.match(ix,iy,rightix1(ix,iy),iy)) then
                                    rightix1(ix,iy)=nx1+1
                                    do ixr=-1,nx1
                                            if(match(ix,iy,ixr,iy)) then
                                                    rightix1(ix,iy)=ixr
                                                    exit
                                            endif
                                    enddo
                                    if(rightix1(ix,iy).eq.nx1+1.and.periodic_bc.eq.0) then
                                            write(*,*) 'Right neighbour not found for ',ix,iy
                                    elseif(periodic_bc.eq.1) then
                                            stop 'computeB2Connectivity: periodic_bc == 1 not supported'
                                    else
                                            write(*,*) 'iy,ix,ixr',iy,ix,ixr
                                            if(ic.eq.0) then
                                                    found=.false.
                                            else
                                                    found=.false.
                                                    do i = 1, ic
                                                            if(ixr.eq.rightcut1(i) .or. ixr.eq.leftcut1(i)) then
                                                                    found = .true.
                                                                    exit
                                                            endif
                                                    end do
                                            endif
                                            if (.not.found) then
                                                    ic = ic + 1
                                                    write(*,*) 'ic',ic
                                                    i = ic
                                            endif
                                            bottomcut1(i)=min(iy,bottomcut1(i))
                                            topcut1(i)=max(iy+1,topcut1(i))
                                            leftcut1(i)=min(ixr,leftcut1(i))
                                    endif
                            endif
                    endif

                    ! Assume rest OK

                    topix1(ix,iy)=ix
                    bottomix1(ix,iy)=ix
                    leftiy1(ix,iy)=iy
                    rightiy1(ix,iy)=iy
                    topiy1(ix,iy)=iy+1
                    bottomiy1(ix,iy)=iy-1

            end do

!!$          cxpb Must now re-direct neighbours for periodic boundary BC case when the
!!$          cxpb two edges of the mesh coincide in real space.
!!$          cxpb The exact position of the "cut" depends on problem implementation.
!!$          cxpb The dead region is made into a topological torus so as not to
!!$          cxpb interact with the rest of the plasma domain.

            if (periodic_bc.eq.1) then
                    stop 'computeB2Connectivity: periodic_bc == 1 not supported'
            end if

            nncut=max(nncut,ic)
!!$            if(nncut.gt.nncutmax) then
!!$                    write(*,*) ' Increase nncutmax in b2mod_geo!'
!!$                    write(*,*) ' nncut = ',nncut,' nncutmax = ',nncutmax
!!$                    call xertst (nncut.le.nncutmax,'faulty parameter nncutmax')
!!$            end if
    end do

!!$  cxpb  Once we know the vertical boundaries of the periodic region, we can
!!$  cxpb  now finish reconnecting the top and bottom rows of the dead regions
!!$  cxpb  to each other, and, in the case of the diverted island geometry, also
!!$  cxpb  reconnect the guard cells on both sides of the island.

    if (periodic_bc.eq.1) then
            stop 'computeB2Connectivity: periodic_bc == 1 not supported'
    end if

!!$  cxpb For 1-D cases where we apply periodic boundary conditions to eliminate
!!$  cxpb tranverse physics
    if(periodic_bc.eq.-1) then
            stop 'computeB2Connectivity: periodic_bc == -1 not supported'
    endif

    if(nncut.eq.0) write(*,*) 'No cuts found'
    if(nncut.ge.1) write(*,*)&
         &  'Calculated leftcut1, rightcut1, topcut1, bottomcut1 = ', leftcut1(1), rightcut1(1), topcut1(1), bottomcut1(1)
    if(nncut.ge.2) write(*,*)&
         &  'Calculated leftcut2, rightcut2, topcut2, bottomcut2 = ', leftcut1(2), rightcut1(2), topcut1(2), bottomcut1(2)

  end subroutine computeB2Connectivity



end module b2Connectivity
