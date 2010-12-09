program itm_grid_writeexamples
  
  ! This program creates some example grids, packages them in edge cpos and writes them

  use itm_grid_examples
  use euitm_routines

  implicit none

  ! variables
  type (type_edge),pointer :: edgecpo(:) => null()
  integer :: idx

  ! set up cpo
  allocate( edgecpo(1) )
  call packageInEdgeCPO( example_2d(), edgecpo(1) )

  ! write to ual
  
  !call euitm_create( treename='euitm', shot=1, run=1, refshot=0, refrun=0, idx=idx)
  call euitm_create( 'euitm', 1, 1, 0, 0, idx)
  call euitm_put(idx,"edge", edgecpo)
  call euitm_deallocate(edgecpo)

end program itm_grid_writeexamples
