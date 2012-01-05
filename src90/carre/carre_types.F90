module carre_types

  use Logging
  use Helper
  use carre_constants

  implicit none

  private 

#include <CARREDIM.F>

  ! forward from carre_constants
  public GRID_UNDEFINED, GRID_INTERNAL, GRID_EXTERNAL, &
       & GRID_BOUNDARY, GRID_BOUNDARY_REFINE, GRID_BOUNDARY_REFINE_FIX, &
       & GRID_INTERNAL_COARSEN, GRID_BOUNDARY_COARSEN, GRID_REFINE

  ! Carre version string
  character(len=8), parameter, public :: CARRE_VERSION = "carre71"

  ! Orientations
  integer, parameter, public :: STRUCT_LEFT = 1
  integer, parameter, public :: STRUCT_RIGHT = 2

  ! Grid line flags
  integer, parameter, public :: GRIDLINE_BASELINE = 1
  integer, parameter, public :: GRIDLINE_XPOINT = 2
  integer, parameter, public :: GRIDLINE_BOUNDARY = 3
  integer, parameter, public :: GRIDLINE_REQUIRED = 4
  integer, parameter, public :: GRIDLINE_REFINED = 5

  ! Face number of a cell / of a point
  integer, parameter, public :: FACE_LEFT = 1
  integer, parameter, public :: FACE_BOTTOM = 2
  integer, parameter, public :: FACE_RIGHT = 3
  integer, parameter, public :: FACE_TOP = 4

  ! Numbering for face alignment in the field-aligned grid
  integer, parameter, public :: FACE_POLOIDAL = 1
  integer, parameter, public :: FACE_RADIAL = 2

  ! Numbering for face alignment in the cartesian equilibrium grid
  integer, parameter, public :: FACE_EQU_RADIAL = 1, FACE_EQU_VERTICAL = 2

  ! Indexing help arrays for faces of cell at position (ip,ir)
  integer, dimension(4), parameter, public :: CELL_FACE_ALIGN = &
       & (/ FACE_RADIAL, FACE_POLOIDAL, FACE_RADIAL, FACE_POLOIDAL /)
  integer, dimension(4), parameter, public :: CELL_FACE_DIP = (/ 0, 0, 1, 0 /)
  integer, dimension(4), parameter, public :: CELL_FACE_DIR = (/ 0, 0, 0, 1 /)

  ! Indexing help arrays for points of cell at position (ip,ir)
  integer, dimension(4,2), parameter, public :: CELL_FACE_POINT_DIP = &
       & reshape( (/ (/0, 0, 1, 0, 0, 1, 1, 1/) /), (/4, 2/) )
  ! should give (/ (/0, 0/), (/0, 1/), (/1, 1/), (/0, 1/) /)

  integer, dimension(4,2), parameter, public :: CELL_FACE_POINT_DIR = &
       & reshape( (/ (/0, 0, 0, 1, 1, 0, 1, 1/) /), (/4, 2/) )
  ! should give (/ (/0, 1/), (/0, 0/), (/0, 1/), (/1, 1/) /)

  ! Indexing help arrays for faces of point at position (ip,ir)
  integer, dimension(4), parameter, public :: POINT_FACE_ALIGN = &
       & (/ FACE_POLOIDAL, FACE_RADIAL, FACE_POLOIDAL, FACE_RADIAL /)
  integer, dimension(4), parameter, public :: POINT_FACE_DIP = (/ -1,  0, 0, 0 /)
  integer, dimension(4), parameter, public :: POINT_FACE_DIR = (/  0, -1, 0, 0 /)

  ! Grid extension modes
  integer, parameter, public :: GRID_EXTENSION_OFF = 0
  integer, parameter, public :: GRID_EXTENSION_MODE_TARGET = 1
  integer, parameter, public :: GRID_EXTENSION_MODE_VESSEL = 2

  ! Equilibrium extension modes
  integer, parameter, public :: EQU_EXTENSION_OFF = 0
  integer, parameter, public :: EQU_EXTENSION_MODE_SIMPLE = 1
  integer, parameter, public :: EQU_EXTENSION_MODE_VESSEL = 2

  type CarreParameters
      !.. nptseg: number of points along differents segments of separatrix (as given
      !..         in input file carre.dat. See also CarreGrid%nptseg)
      !.. repart: selector for the radial point distribution
      !           1=absolute distance, 2=difference in psi
      !.. tgarde: guard length for each divertor target
      !.. npr   : numbers of the grid points in radial direction
      !           (region index) as requested in configuration
      !.. pntrat: penetration value of the X-point relative to the O-point
      !.. deltp1,deltpn: values of the first and last intervals for each sep.
      !.. deltr1,deltrn: values of the first and last intervals for each reg.

      INTEGER :: nptseg(nsepsegmx),repart,npr(nregmx)
      REAL*8 deltp1(nsepsegmx),deltpn(nsepsegmx),&
           & deltr1(nsepsegmx),deltrn(nsepsegmx),pntrat, & 
           & tgarde(4)

      !.. cstlin: a linear constant added along y to artificially disconnect
      !           the X-points. Set to 0 for connected double-nulls
      real*8 :: cstlin = 0.00

      ! Predefined x- and o-point locations
      ! If xPointNum > 0, the values in xPointX/Y and oPointX/Y are assumed to be valid
      integer :: xPointNum = 0 ! Number of predefined x-points
      double precision :: xPointX(gradmx), xPointY(gradmx) ! x-point positions
      double precision :: oPointX, oPointY ! o-point positions

      ! Control flag for equilibrium data extension
      integer :: equExtensionMode = EQU_EXTENSION_OFF

      ! psi cutoff parameters
      double precision :: psimin = -huge(0.0d0), psimax = huge(0.0d0)
      ! boundary cutoff parameters (given in physical space dimensions)
      double precision :: rMin = -huge(0.0d0), rMax = huge(0.0d0)
      double precision :: zMin = -huge(0.0d0), zMax = huge(0.0d0)

      ! extension size for equilibrium grid at the boundaries
      integer :: addLeft = 0, addRight = 0, addTop = 0, addBottom = 0

      ! Desired maximal poloidal cell size at targets in extended grid mode
      ! Default value is chosen to cause no refinement at targets.
      double precision :: targetRes = huge(0.0d0)

      double precision :: maxResJump = 4.0d0

      double precision :: cleanupPasmin = huge(0.0d0)

      ! Extended grid mode
      integer :: gridExtensionMode = GRID_EXTENSION_OFF

      ! Log level (see Logging.f90)
      integer :: logLevel = LOGWARNING

      ! Output format
      character(len=8) :: carre_format = CARRE_VERSION
  end type CarreParameters


  type CarreEquilibrium
      !.. nx,ny : number of data points in x and y
      !.. x,y   : tables of coordinates of the data points
      !.. psi   : psi values at each data point
      !.. psidx,psidy: values of psi derivatives in x and y
      !                at each data point
      !.. npxtot: number of the points where the gradient vanishes
      !.. pointx,pointy: coordinates of the points where the gradient
      !                  vanishes
      !.. ii,jj : x and y indices of the cells where the gradient vanishes
      !.. npx   : number of the X-points
      !.. ptx,pty: X-point co-ordinates
      !.. iptx,jptx: x and y indices of the cells containing the X-points
      !.. xpto,ypto: coordinates of the O-point
      !.. racord: determines whether the X-points are connected
      !.. fctpx: the psi values at each X- or O-point
      !.. separx,separy: coordinates of the points of the parametrised
      !               separatrices (point index, branch index, X-point index)
      !.. nptot : number of parametrisation points for each separatrix
      !           (separatrix index, point index)
      !.. ptsep : separatrix pointer, used as index
      !           (separatrix index, point index)
      !.. ptxint: index of the internal X-point in the case of disconnected
      !           double-null
      !.. limcfg: indicates when a limiter configuration is considered (when
      !           non zero). This variable is assigned the index of the
      !           limiter
      !     N.B.: When the limiter configuration is selected, npx is set equal
      !           to 1 and the coordinates of the X-point correspond to the
      !           innermost point of the limiter.
      !.. eps_Xpt Threshold to decide when we are close to an x-point (is set
      !           depending on equilibrium resolution)
      !   distxo  The distance between the active X-point and the O-point
      !.. nsep  : number of the separatrices per the configuration
      integer :: nx, ny, iptx(npxmx), jptx(npxmx), npx, npxtot,ptsep(4,npxmx), & 
           &   nptot(4,npxmx), ii(gradmx), jj(gradmx),ptxint, limcfg, nsep

      REAL*8 :: &
           &  x(nxmax), y(nymax), psi(nxmax,nymax), &
           & psidx(nxmax,nymax), psidy(nxmax,nymax), &
           & a00(nxmax,nymax,3), a10(nxmax,nymax,3),&
           & a01(nxmax,nymax,3), a11(nxmax,nymax,3), &
           & ptx(npxmx), pty(npxmx), xpto, ypto, fctpx(npxmx), & 
           & separx(npnimx,4,npxmx), separy(npnimx,4,npxmx), &
           & pointx(gradmx), pointy(gradmx), &
           & eps_Xpt, distxo

      LOGICAL racord

      ! Intersections of faces of the equilibrium grid with the vessel structures
      ! For every node, store intersection for the face towards the top or the
      ! right of it. Third dimension is FACE_EQU_RADIAL or FACE_EQU_VERTICAL
      logical :: faceISec(1:nxmax, 1:nymax, 2)     
      ! categorization of points of the equilibrium grid:
      !  set to GRID_EXTERNAL or GRID_INTERNAL
      integer :: pointFlag(1:nxmax, 1:nymax)      

  end type CarreEquilibrium

  type CarreGrid
      !.. nreg  : number of the grid regions depending on the configuration
      !.. np1   : numbers of the grid points in poloidal direction
      !           (region index) (eventually derived from par%nptseg)
      !.. nr    : number of grid points in the radial direction (per region)
      !           Derived from CarreParameters%npr. Caution: the classical
      !           carre code does not use this array, but uses CarraParameters%npr.
      !           CarreGrid%nr is only relevant in the postprocessing, where the
      !           number of poloidal and/or radial grid points is modified.
      !.. xmail,ymail: grid point coordinates
      !                (poloidal, radial, region)
      !.. xn,yn : working array for coordinates along a parametrised curve
      !.. psim : value of psi interpolated to grid points
      !.. psidxm, psidym: grad psi interpolated to grid points
      !.. nptseg: number of points along differents segments of separatrix (as resulting
      !           from the grid generation/postprocessing algorithm). Might differ
      !           from the input values in carre.dat/CarreParameters%nptseg.

      integer :: nreg, np1(nregmx), nr(nregmx)
      INTEGER :: nptseg(nsepsegmx)
      REAL*8  & 
           & xn(npnimx),yn(npnimx),xmail(npmamx,nrmamx,nregmx), & 
           & ymail(npmamx,nrmamx,nregmx), &
           & psim(npmamx,nrmamx,nregmx),psidxm(npmamx,nrmamx,nregmx), & 
           & psidym(npmamx,nrmamx,nregmx)

      REAL*8, dimension(npmamx-1,nrmamx-1,nregmx) :: hx, hy

      ! For every radial line in every region, store from which
      ! separatrix segment this radial line emanates
      integer :: radLineSepSeg(npmamx,nregmx)

      ! Logical flag for face/structure intersection. True means
      ! the specific face is intersected by a structure segment.
      ! Dimensions:
      ! 1: face alignment: 1 = poloidal, 2 = radial
      ! 2: poloidal cell index
      ! 3: radial cell index, 4: region number
      logical :: faceISec(2,npmamx,nrmamx,nregmx)
      ! Position of the intersection point
      double precision :: faceISecPx(2,npmamx,nrmamx,nregmx)
      double precision :: faceISecPy(2,npmamx,nrmamx,nregmx)
      ! During iteration: index of the structure intersecting the face.
      ! After finalization: index of structure the face is aligned to.
      ! If GRID_UNDEFINED, not intersected/in contact with a face
      integer :: faceISecIStruct(2,npmamx,nrmamx,nregmx)

      ! Cell categorization flag (internal, external, ...)
      ! See the GRID_* constants in carre_constants
      integer :: cellflag(npmamx-1,nrmamx-1,nregmx)
      ! Structure index 
      integer :: cellFaceIStruct(1:4,npmamx-1,nrmamx-1,nregmx)

      integer, dimension(npmamx,nrmamx,nregmx) :: pointFlag, pointStructIndex

      ! logical flags marking radial grid lines as required
      integer :: lineFlagRad(npmamx, nregmx)

      ! neighbour information for following cells in the radial direction
      ! from region to region
      ! Dimensions: 1=region, 2=poloidal index of radial cell strip,
      !             3=radial direction (1=positive, 2=negative)
      integer :: nbFaceReg(nregmx, npmamx-1, 2), &
           & nbFaceIPol(nregmx, npmamx-1, 2), nbFaceIRad(nregmx, npmamx-1, 2)

  end type CarreGrid

  type CarreStructures
      !.. nstruc: number of structures
      !.. npstru: number of points per structure
      !..    NOTE: npstru is now always > 0. Use the closed
      !..          flag to check whether a structure is closed
      !.. xstruc,ystruc: coordinates of the structure points
      !                  (point index, structure index)
      !.. nbdef : number of the divertor plates
      !.. inddef: table of indices of the divertor plates
      !.. indplq: table of the structure indices (0 means not a target)
      !           (separatrix index, X-point index)
      !.. nbniv : number of the limiting level lines
      !.. nivx,nivy: coordinates of the points of the parametrised
      !              limiting level lines (point index, curve index)
      !.. nivtot: number of points for each parametrised limiting level line
      !.. distnv: distance along a plate between the separatrix strike-point
      !           and a limiting level line
      !           (distance selector [1=real, 2=psi], curve index)
      !.. nomstr Name of structure
      !.. closed: indicates whether the structure is closed or not
      integer ::  nstruc, npstru(strumx), & 
           &    indplq(4,npxmx),inddef(nbdmx), & 
           &   nbdef, nivtot(nivmx), nbniv      
      REAL*8  & 
           &   xstruc(npstmx,strumx), ystruc(npstmx,strumx), & 
           &   nivx(npnimx,nivmx),nivy(npnimx,nivmx),distnv(5,nivmx)
      logical :: closed(strumx)
      
      ! Internal side of a structure (i.e. which side is facing the inside
      ! of the plasma vessel): going along the structure segments, either STRUCT_RIGHT
      ! (clockwise) or STRUCT_LEFT (counterclockwise).
      ! If GRID_UNDEFINED, the information is unknown for this structure.
      ! Generally it is only known for the target plates (computed in sptris).
      integer :: internalSide(strumx) = GRID_UNDEFINED

      !     arrays to hold copies of structures (r for "real")
      !     (if virtual structures are used)
      integer :: rnstruc, rnpstru(strumx)
      REAL*8 :: rxstruc(npstmx,strumx), rystruc(npstmx,strumx)
      logical :: rclosed(strumx)

      character nomstr(strumx)*80
  end type CarreStructures

  public CarreParameters, CarreEquilibrium, CarreGrid, CarreStructures

  !contains

end module carre_types
