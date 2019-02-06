c=======================================================================
      subroutine ecrim3(nfin,nx,ny,crx,cry,bb,nxmax,nymax)
      implicit none
#include <CARRETYPES.F>
c  cette routine ecrit la maille sous format DIVIMP. elle est identique
c  a la routine mhdvmp de b2ag.F
cxpb-20070424: removed the abs() of brat so the helicity information 
c              is kept !
c
c  arguments
      integer nfin,nx,ny,nxmax,nymax
      real(kind=R8)
     .  crx(-1:nxmax,-1:nymax,0:3),cry(-1:nxmax,-1:nymax,0:3),
     .  bb(-1:nxmax,-1:nymax,0:3)
c
c  local variables
      integer ix,iy,cut1,cut2,cutrgn,icell
      real(kind=R8) x0,y0,brat
c
c  procedures
c
c  calculation
c
c  2.   print mesh parameters
      icell=0
      write(nfin,100)
 100  format(3x,'Element output:'///
     .  3x,'=================================================='
     .    ,'======================================')
      do iy=0,ny-1
      do ix=0,nx-1
        icell=icell+1
c  2.1  calculate coordinates of cell centre
        x0=0.25*(crx(ix,iy,0)+crx(ix,iy,1)+crx(ix,iy,2)+crx(ix,iy,3))
        y0=0.25*(cry(ix,iy,0)+cry(ix,iy,1)+cry(ix,iy,2)+cry(ix,iy,3))
c  2.2  calculate magnetic field ratio
        brat=bb(ix,iy,0)/bb(ix,iy,3)
c  2.3  print divimp input data
        write(nfin,101)icell,ix,iy,crx(ix,iy,2),cry(ix,iy,2),
     .    crx(ix,iy,3),cry(ix,iy,3)
 101    format(3x,'Element',i7,' = (',i3,',',i3,'): (',
     .    1pe17.10,',',1pe17.10,')',
     .    6x,'(',1pe17.10,',',1pe17.10,')')
        write(nfin,102)brat,x0,y0
 102    format(3x,'Field ratio  = ',1pe17.10,13x,
     .    '(',1pe17.10,',',1pe17.10,')')
        write(nfin,103)crx(ix,iy,0),cry(ix,iy,0),
     .    crx(ix,iy,1),cry(ix,iy,1)
 103    format(
     .    t30,'(',1pe17.10,',',1pe17.10,')',
     .    6x,'(',1pe17.10,',',1pe17.10,')')
        write(nfin,105)
 105    format(3x,'--------------------------------------------------'
     .    ,'--------------------------------------')

      enddo
      enddo
c
c  3.   look for the outermost ring with a cut and determine the indices
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
            write(6,*)'error in ecrim3: cut1, cut2 not calculated'
            stop
          endif
        endif
      enddo
      enddo
c
      write(6,*)'warning in ecrim3: unable to find two cuts'
      write(6,*)'cut1, cut2=',cut1,cut2
c
 10   continue
      cutrgn=iy+1
c
c  5.   print grid characteristics
      write(nfin,104)ny,nx,cutrgn,cut1,cut2
 104  format('''TdeV - grid characteristics: Number of Rings     ''',i7/
     .       '''                             Number of Knots     ''',i7/
     .       '''                             Cut ring            ''',i7/
     .       '''                             Cut point 1         ''',i7/
     .       '''                             Cut point 2         ''',i7)
c
c  3.   return
      return
      end
