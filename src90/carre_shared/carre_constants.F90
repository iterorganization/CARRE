module carre_constants

  implicit none

  private 

  integer, parameter, public :: GRID_UNDEFINED = 0

  ! Constants used to categorize grid objects
  ! These are used for points, faces and cells
  integer, parameter, public :: GRID_INTERNAL = 1
  integer, parameter, public :: GRID_EXTERNAL = 2
  integer, parameter, public :: GRID_BOUNDARY = 3
  integer, parameter, public :: GRID_BOUNDARY_REFINE = 4
  integer, parameter, public :: GRID_BOUNDARY_REFINE_FIX = 5
  integer, parameter, public :: GRID_INTERNAL_COARSEN = 6
  integer, parameter, public :: GRID_BOUNDARY_COARSEN = 7
  integer, parameter, public :: GRID_REFINE = 8



end module carre_constants
