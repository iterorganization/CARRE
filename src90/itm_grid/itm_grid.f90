module itm_grid

  use itm_types

  implicit none
  
  integer, parameter :: GRID_UNDEFINED = 0

  ! The following are part of the ITM convention machine coordinate system
  integer, parameter :: COORDTYPE_R = 2 ! Major radius (m)
  integer, parameter :: COORDTYPE_Z = 3 ! Vertical height (m) 


end module itm_grid
