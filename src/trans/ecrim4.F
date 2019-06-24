      subroutine ecrim4(nfin,nx,ny,crx,cry,bb,b0r0,fpsi,ffbz,delpsi,
     ,                       nxmax,nymax,ncut,nxcut,nycut,niso,nxiso)
c
c  version : 11.01.99 22:40
c
c======================================================================
c*** This routine writes the grid in the B2 format
cank-19980626: the cut data added to the header
cank-19990801: the data for isolating cut added to the header
cxpb-20070424: removed the abs() of brat so the helicity information 
c              is kept !
c======================================================================
      implicit none
#include <CARRETYPES.F>
c
c  arguments
      integer i,nfin,nx,ny,nxmax,nymax,ncut,nxcut(ncut),nycut(ncut),
     ,	      niso,nxiso(niso+1)
      real(kind=R8)
     .  crx(-1:nxmax,-1:nymax,0:3),cry(-1:nxmax,-1:nymax,0:3),
     .  bb(-1:nxmax,-1:nymax,0:3),b0r0,delpsi,
     .  fpsi(-1:nxmax,-1:nymax,0:3),ffbz(-1:nxmax,-1:nymax,0:3)
c
c  local variables
      integer ix,iy,cut1,cut2,cutrgn,icell
      real(kind=R8) x0,y0,brat
c
c  procedures
c======================================================================
c  calculation

c* 2.   print mesh parameters

      write(nfin,*)
      write(nfin,'(3x,a/)') 'Element output:'
      write(nfin,*) '   R*Btor =',b0r0
      write(nfin,*) '   nx     =',nx
      write(nfin,*) '   ny     =',ny
      write(nfin,*) '   ncut   =',ncut
      if(ncut.gt.0) then
        write(nfin,*) '   nxcut  =',(nxcut(i),i=1,ncut)
        write(nfin,*) '   nycut  =',(nycut(i),i=1,ncut)
      end if
      write(nfin,*) '   niso   =',niso
      if(niso.gt.0) then
        write(nfin,*) '   nxiso  =',(nxiso(i),i=1,niso)
      end if
      write(nfin,'(/3x,2a)') '======================================',
     ,             '=================================================='

       icell=0
      do iy=-1,ny
        do ix=-1,nx

c* 2.1  calculate coordinates of cell centre

          x0=0.25*(crx(ix,iy,0)+crx(ix,iy,1)+crx(ix,iy,2)+crx(ix,iy,3))
          y0=0.25*(cry(ix,iy,0)+cry(ix,iy,1)+cry(ix,iy,2)+cry(ix,iy,3))

c* 2.2  calculate magnetic field ratio

          brat=bb(ix,iy,0)/bb(ix,iy,3)

c* 2.3  print B2 input data

          write(nfin,101) icell,ix+1,iy+1,crx(ix,iy,2),cry(ix,iy,2),
     .      crx(ix,iy,3),cry(ix,iy,3)
 101      format(3x,'Element',i7,' = (',i3,',',i3,'): (',
     .      1pe17.10,',',1pe17.10,')',
     .      6x,'(',1pe17.10,',',1pe17.10,')')
          write(nfin,102)brat,x0,y0
 102      format(3x,'Field ratio  = ',1pe17.10,13x,
     .      '(',1pe17.10,',',1pe17.10,')')
          write(nfin,103)crx(ix,iy,0),cry(ix,iy,0),
     .      crx(ix,iy,1),cry(ix,iy,1)
 103      format(
     .      t32,'(',1pe17.10,',',1pe17.10,')',
     .      6x,'(',1pe17.10,',',1pe17.10,')')
          write(nfin,105)
 105      format(3x,'------------------------------------------------',
     ,                      '----------------------------------------')
          icell=icell+1
        enddo
      enddo

      do iy=-1,ny
        do ix=-1,nx
          write(nfin,110) ix+1,iy+1,
     .     fpsi(ix,iy,0)-delpsi,fpsi(ix,iy,1)-delpsi,
     .     fpsi(ix,iy,2)-delpsi,fpsi(ix,iy,3)-delpsi,
     .     ffbz(ix,iy,0),ffbz(ix,iy,1),ffbz(ix,iy,2),ffbz(ix,iy,3)
        end do
      enddo
 110  format(i4,1x,i4,1x,8(f13.8,1x))
c
c* 3.   look for the outermost ring with a cut and determine the indices
c       of the polygon on which these cuts occur

      cut1=0
      cut2=0
      do iy=ny-1,0,-1
      do ix=0,nx-2
        if(crx(ix,iy,1).ne.crx(ix+1,iy,0) .or.
     .     cry(ix,iy,1).ne.cry(ix+1,iy,0)) then
          if(cut1.eq.0) then
            cut1=ix+1
          elseif(cut2.eq.0) then
            cut2=ix+1
            go to 10
          else
            write(6,*)'error in ecrim4: cut1, cut2 not calculated'
            stop
          endif
        endif
      enddo
      enddo
c
c     write(6,*)'warning in ecrim4: unable to find two cuts'
c     write(6,*)'cut1, cut2=',cut1,cut2
c
 10   continue
c     cutrgn=iy+1
c
c  5.   print grid characteristics
c     write(nfin,104)ny,nx,cutrgn,cut1,cut2
c104  format('''TdeV - grid characteristics: Number of Rings     ''',i7/
c    .       '''                             Number of Knots     ''',i7/
c    .       '''                             Cut ring            ''',i7/
c    .       '''                             Cut point 1         ''',i7/
c    .       '''                             Cut point 2         ''',i7)
c
c  3.   return
      return
c======================================================================
      end
