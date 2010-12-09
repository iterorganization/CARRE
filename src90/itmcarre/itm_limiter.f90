module itm_limiter_mockup

  ! Mockup for 

  implicit none

  use euITM_utilities

  type type_limelement  !    
          character(len=132), dimension(:), pointer ::name => null()       ! /limelement/name - Name of this element. Should be a matrix of strings (nlims,max_nelements), but not supported by the UAL yet.
          character(len=132), dimension(:), pointer ::id => null()       ! /limelement/id - ID of this element. Should be a matrix of strings (nlims,max_nelements), but not supported by the UAL yet.
          real(DP),pointer  :: area(:,:) => null()     ! /limelement/area - Surface area of this element [m^2]; Matrix (nlims,max_nelements)
          type (type_limgeometry) :: limgeometry  ! /limelement/limgeometry - 
  endtype type_limelement

  type type_limgeometry  !    
          integer,pointer  :: npoints(:,:) => null()     ! /limgeometry/npoints - Number of points describing an element (irregular outline rzcoordinates); Matrix (nlims,max_nelements)
          integer,pointer  :: closed(:,:) ! /limgeometry/closed - Flag indicating whether a structure is closed (i.e. the last and first point are to be connected); closed(:,:) = 0 means open, closed(:,:) = 1 means closed. Matrix (nlims,max_nelements)
          type (type_rz3D) :: rzcoordinate  ! /limgeometry/rzcoordinate - Irregular outline [m]; 3D arrays (nlims,max_nelements,max_npoints)
  endtype type_limgeometry

contains


  !.. nstruc: number of structures
  !.. npstru: number of points per structure
  !.. xstruc,ystruc: coordinates of the structure points
  !                  (point index, structure index)
  
  subroutine itmlimReadFromStructureFile('filename')
    


  end subroutine itmlimReadFromStructureFile



end module itm_limiter_mockup
