      subroutine fcrfldi(lun)
c
c  version : 16.09.2000 00:15
c
c======================================================================
c*** Read the field data from dg equilibrium file or from a separate
c*** file 'field.dg'
c======================================================================
      implicit none
      integer lun
#include <CARRETYPES.F>
#include <FCRCOM.F>
      integer i
      real(kind=R8) btf,rtf
      logical ex
c======================================================================
c
      rbtor=0.0_R8
      btf=0.0_R8
      rtf=0.0_R8
      inquire(file='field.dg',exist=ex)
      if(ex) then
        open(lun+2,file='field.dg')
      end if
      call rdeqdg(lun,ngpr,ngpz,i,nr,nz,btf,rtf,rgr,zgr,pfm)
      if(i.ne.0) then
        write(*,*) 'fcrfldi: error reading the magnetic data. ',
     ,                                                  'Error code ',i
        stop
      end if
      if(ex) close(lun+2)
      rbtor=rtf*btf
      if(rbtor.ne.0.0_R8) return                                       ! DPC
c======================================================================
      write(*,*) 'fcrfldi: no data on the toroidal field found.'
      write(*,*) 'These data should be either in the equilibrium file',
     ,            ' or in the separate file "field.dg" in the current',
     ,                                                     ' directory'
      stop
c======================================================================
      end
