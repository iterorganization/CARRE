      subroutine fcrfldi(lun)
!
!  version : 16.09.2000 00:15
!
!======================================================================
!*** Read the field data from dg equilibrium file or from a separate
!*** file 'field.dg'
!======================================================================
      use KindDefinitions
      implicit none
      integer(Short) :: lun
#include <FCRCOM.F>
      integer(Short) :: i
      real(rKind) :: btf,rtf
      logical ex
      external rdeqdg
!======================================================================
!
      rbtor=0.
      btf=0.
      rtf=0.
      inquire(file='field.dg',exist=ex)
      if(ex) then
        open(lun+2,file='field.dg')
      end if
      call rdeqdg(lun,ngpr,ngpz,i,nr,nz,btf,rtf,rgr,zgr,pfm)
      if(i.ne.0) then
        write(*,*) 'fcrfldi: error reading the magnetic data. ', & 
     &                                                  'Error code ',i
        stop
      end if
      if(ex) close(lun+2)
      rbtor=real(rtf*btf,Single)
      if(rbtor.ne.0.) return                                           ! DPC
!======================================================================
      write(*,*) 'fcrfldi: no data on the toroidal field found.'
      write(*,*) 'These data should be either in the equilibrium file', & 
     &            ' or in the separate file "field.dg" in the current', & 
     &                                                     ' directory'
      stop
!======================================================================
      end
