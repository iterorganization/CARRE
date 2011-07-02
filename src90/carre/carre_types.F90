module carre_types
  implicit none

  private 

#include <CARREDIM.F>

  integer, parameter, public :: GRID_UNDEFINED = 0

  ! Grid object labels
  integer, parameter, public :: GRID_INTERNAL = 1
  integer, parameter, public :: GRID_EXTERNAL = 2
  integer, parameter, public :: GRID_BOUNDARY = 3
  integer, parameter, public :: GRID_BOUNDARY_REFINE = 4

  ! Grid line flags
  integer, parameter, public :: GRIDLINE_BASELINE = 1
  integer, parameter, public :: GRIDLINE_REQUIRED = 2

  ! Face number of a cell
  integer, parameter, public :: FACE_LEFT = 1
  integer, parameter, public :: FACE_BOTTOM = 2
  integer, parameter, public :: FACE_RIGHT = 3
  integer, parameter, public :: FACE_TOP = 4
  
  ! Numbering for face alignment
  integer, parameter, public :: FACE_POLOIDAL = 1
  integer, parameter, public :: FACE_RADIAL = 2

  ! Grid extension modes
  integer, parameter, public :: GRID_EXTENSION_OFF = 0
  integer, parameter, public :: GRID_EXTENSION_MODE_TARGET = 1
  integer, parameter, public :: GRID_EXTENSION_MODE_VESSEL = 2
  
  ! Equilibrium extension modes
  integer, parameter, public :: EQU_EXTENSION_OFF = 0
  integer, parameter, public :: EQU_EXTENSION_MODE_SIMPLE = 1
  integer, parameter, public :: EQU_EXTENSION_MODE_VESSEL = 2

  type CarreParameters
!.. nptseg: number of points along differents segments of separatrix
!.. repart: selector for the radial point distribution
!           1=absolute distance, 2=difference in psi
!.. tgarde: guard length for each divertor target
!.. npr   : numbers of the grid points in radial direction
!           (region index) as requested in configuration
!.. pntrat: penetration value of the X-point relative to the O-point
!.. deltp1,deltpn: values of the first and last intervals for each sep.
!.. deltr1,deltrn: values of the first and last intervals for each reg.

      INTEGER nptseg(10),repart,npr(nregmx)
      REAL*8 deltp1(10),deltpn(10),deltr1(10),deltrn(10),pntrat, & 
     &       tgarde(4)

!.. cstlin: a linear constant added along y to artificially disconnect
!           the X-points. Set to 0 for connected double-nulls
      real*8 :: cstlin = 0.00

      ! Control flag for equilibrium data extension
      integer :: equExtensionMode = EQU_EXTENSION_OFF

      ! psi cutoff parameters
      double precision :: psimin = -huge(0.0d0), psimax = huge(0.0d0)
      ! boundary cutoff parameters (given in physical space dimensions)
      double precision :: rMin = -huge(0.0d0), rMax = huge(0.0d0)
      double precision :: zMin = -huge(0.0d0), zMax = huge(0.0d0)
      
      ! extension size for equilibrium grid at the boundaries
      integer :: addLeft = 0, addRight = 0, addTop = 0, addBottom = 0      

      ! Extended grid mode
      integer :: gridExtensionMode = GRID_EXTENSION_MODE_TARGET
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

      integer :: nx, ny, iptx(npxmx), jptx(npxmx), npx, npxtot,ptsep(4,npxmx), & 
          &   nptot(4,npxmx), ii(gradmx), jj(gradmx),ptxint, limcfg

      REAL*8 :: &
          &  x(nxmax), y(nymax), psi(nxmax,nymax), &
          & psidx(nxmax,nymax), psidy(nxmax,nymax), &
          & a00(nxmax,nymax,3), a10(nxmax,nymax,3),&
          & a01(nxmax,nymax,3), a11(nxmax,nymax,3), &
          & ptx(npxmx), pty(npxmx), xpto, ypto, fctpx(npxmx), & 
          & separx(npnimx,4,npxmx), separy(npnimx,4,npxmx), &
          & pointx(gradmx), pointy(gradmx), &
          & eps_Xpt

      LOGICAL racord

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
          integer :: nreg, np1(nregmx), nr(nregmx)

          REAL*8  & 
               & xn(npnimx),yn(npnimx),xmail(npmamx,nrmamx,nregmx), & 
               & ymail(npmamx,nrmamx,nregmx), &
               & psim(npmamx,nrmamx,nregmx),psidxm(npmamx,nrmamx,nregmx), & 
               & psidym(npmamx,nrmamx,nregmx)
          
          ! Logical flag for face/structure intersection. True means
          ! the specific face is intersected by a structure segment.
          ! Dimensions:
          ! 1: face alignment: 1 = poloidal, 2 = radial
          ! 2: poloidal cell index
          ! 3: radial cell index, 4: region number
          logical :: faceISec(2,npmamx,nrmamx,nregmx)
          double precision :: faceISecPx(2,npmamx,nrmamx,nregmx)
          double precision :: faceISecPy(2,npmamx,nrmamx,nregmx)
          
          integer :: cellflag(npmamx-1,nrmamx-1,nregmx)
          integer :: cellFaceFlag(npmamx-1,nrmamx-1,nregmx)

          integer :: pointFlag(npmamx,nrmamx,nregmx)

          ! logical flags marking grid lines as required
          integer :: lineFlagRad(nrmamx, nregmx)

  end type CarreGrid
  
  type CarreStructures
!.. nstruc: number of structures
!.. npstru: number of points per structure
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
      integer ::  nstruc, npstru(strumx), & 
          &    indplq(4,npxmx),inddef(nbdmx), & 
          &   nbdef, nivtot(nivmx), nbniv
      REAL*8  & 
          &   xstruc(npstmx,strumx), ystruc(npstmx,strumx), & 
          &   nivx(npnimx,nivmx),nivy(npnimx,nivmx),distnv(5,nivmx)

!     arrays to hold copies of structures (r for "real")
!     (if virtual structures are used)
      integer :: rnstruc, rnpstru(strumx)
      REAL*8 :: rxstruc(npstmx,strumx), rystruc(npstmx,strumx)

      character nomstr(strumx)*80
  end type CarreStructures

  public CarreParameters, CarreEquilibrium, CarreGrid, CarreStructures

!!$contains

end module carre_types
