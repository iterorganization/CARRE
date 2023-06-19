      subroutine fcrcrro(ofile)
!
!  version : 28.04.97 23:02
!
!======================================================================
!*** Create the input file for Carre
!======================================================================
      use KindDefinitions
      implicit none
      character ofile*(*)
#include <FCRCOM.F>
      integer(Short) :: niscl,nrscl
      parameter (niscl=2, nrscl=4)
!*** niscl : number of integer scalar parameters in the Carre input
!*** nrscl : number of real scalar parameters in the Carre input
      integer i,j
      integer(Short) ::  iprmeq(niscl)
      real(rKind) :: rprmeq(nrscl)
      equivalence (iprmeq,repart), (rprmeq,relax)
      character*8 chiprm(niscl),chrprm(nrscl)
      data chiprm / 'repart  ','nrelax  '/
      data chrprm / 'relax   ','pasmin  ','rlcept  ','pntrat  ' /
!======================================================================
!
      open(2,file=ofile)
      rewind(2)
      write(2,'(a)') '$parameters'
      write(2,700) 'carreMode',carre_mode
      write(2,700) 'gridExtensionMode',grid_ext_mode
      write(2,700) 'equExtensionMode',equ_ext_mode
      if (nstrv.gt.0) then
        ! If vessel structures are present, the first nstr
        ! structures are assumed to be virtual ones
        write(2,700) 'nVirtualStructs', nstr
      endif
      do i=1,niscl
        write(2,700) chiprm(i),iprmeq(i)
      end do
      do i=1,nrscl
        write(2,720) chrprm(i),rprmeq(i)
      end do
      do j=1,nsgm
        write(2,710) 'nptseg',j,nptseg(j)
        write(2,730) 'deltp1',j,deltp1(j)
        write(2,730) 'deltpn',j,deltpn(j)
      end do
      do j=1,nrgn
        write(2,710) 'npr',j,npr(j)
        write(2,730) 'deltr1',j,deltr1(j)
        write(2,730) 'deltrn',j,deltrn(j)
      end do
      do j=1,ntrg
        write(2,730) 'tgarde',j,tgarde(j)
      end do
      write(2,720) 'targetResolution',target_res
      write(2,'(a)') '$end'
      close(2)
!======================================================================
  700 format(a,' = ',i5)
  710 format(a,'(',i1,') = ',i5)
  720 format(a,' = ',1p,e15.7)
  730 format(a,'(',i1,') = ',1p,e15.7)
!======================================================================
      end
