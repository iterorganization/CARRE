c=======================================================================
      subroutine ecrim5(nfin,nx,ny,crx,cry,bb,b0r0,fpsi,nxmax,nymax)
      implicit none
#include <CARRETYPES.F>
c  cette routine ecrit la maille sous format DIVIMP. elle est identique
c  a la routine mhdvmp de b2ag.F
c
c     jdemod - this is a modification of the original DIVIMP grid 
c              writing routine. The grid characteristic numbers are
c              written at the beginning instead of the end. In addition, 
c              calculated psi values for the targets are included at the
c              of the file. 
c
c
c  arguments
      integer nfin,nx,ny,nxmax,nymax
      real(kind=R8) 
     .  crx(-1:nxmax,-1:nymax,0:3),cry(-1:nxmax,-1:nymax,0:3),
     .  bb(-1:nxmax,-1:nymax,0:3),b0r0
      real(kind=R8) fpsi(-1:nxmax,-1:nymax,0:3)
c
c  local variables
      integer ix,iy,cut1,cut2,cutrgn,icell
      real(kind=R8) x0,y0,brat,psi0
      real(kind=R8) psitarg
      real(kind=R8) psimin_carre
c
c     Read in the rzpsi.dat file to get the reference PSI values - in particular
c     the minimum value of PSI on the grid. 
c
      call get_psimin_carre(psimin_carre)

      write(0,*) 'PSIMIN_CARRE:=',psimin_carre
c
c  procedures
c
c  1.   look for the outermost ring with a cut and determine the indices
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
            write(6,*)'error in ecrim5: cut1, cut2 not calculated'
            stop
          endif
        endif
      enddo
      enddo
c
      write(6,*)'warning in ecrim5: unable to find two cuts'
      write(6,*)'cut1, cut2=',cut1,cut2+1
c
 10   continue
      cutrgn=iy+1
c
c  2.   print grid characteristics
c
cjde   DIVIMP expects cut1 and cut2 as polygon indices of the cells in the PFZ
cjde  - past the cuts - thus add one to cut2
c
      write(0,104) ny,nx,cutrgn,cut1,cut2+1
 104  format('''DIV  - grid characteristics: Number of Rings     ''',i7/
     .       '''                             Number of Knots     ''',i7/
     .       '''                             Cut ring            ''',i7/
     .       '''                             Cut point 1         ''',i7/
     .       '''                             Cut point 2         ''',i7)

      write(nfin,106) ny,cutrgn,nx,cut1,cut2
 106  format('GEOM:',5(1x,i10))
c
c  calculation
c
c  3.   print mesh parameters
      icell=0
      write(nfin,100)
 100  format(3x,'Element output:'///
     .  3x,'=================================================='
     .    ,'======================================')
cjde  We wish to include boundary cells
cjde  thus the indices run from -1 to nx and -1 to ny
      do iy=-1,ny
      do ix=-1,nx
        icell=icell+1
c  2.1  calculate coordinates of cell centre
        x0=0.25*(crx(ix,iy,0)+crx(ix,iy,1)+crx(ix,iy,2)+crx(ix,iy,3))
        y0=0.25*(cry(ix,iy,0)+cry(ix,iy,1)+cry(ix,iy,2)+cry(ix,iy,3))
c  2.2  calculate magnetic field ratio
        brat=bb(ix,iy,0)/bb(ix,iy,3)
cjde    calculate normalized psi at cell centre
        psi0=0.25*(fpsi(ix,iy,0)+fpsi(ix,iy,1)
     .            +fpsi(ix,iy,2)+fpsi(ix,iy,3))/(-psimin_carre) + 1.0
c  2.3  print divimp input data
c       write cell element data
        write(nfin,101)icell,ix,iy,crx(ix,iy,2),cry(ix,iy,2),
     .    crx(ix,iy,3),cry(ix,iy,3)
 101    format(3x,'Element',i7,' = (',i3,',',i3,'): (',
     .    1pe17.10,',',1pe17.10,')',
     .    6x,'(',1pe17.10,',',1pe17.10,')')
        write(nfin,102)abs(brat),x0,y0,psi0
 102    format(3x,'Field ratio  = ',1pe17.10,13x,
     .    '(',1pe17.10,',',1pe17.10,')',2x,0pf13.6)
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
c 4.  Calculate the PSI values at the targets and print them
c
      write(nfin,107)  2*ny
 107  format('PSI:',1x,i10)
 108     format(2(1x,i10),3(1x,e16.8))
c
c     jdemod -
c     The iy=0 elements at the end of the grid would appear to be elements 0 and 2
c     This is just due to the internal indexing system chosen in b2agfz.F
c
c     iy would appear to be the ring index while ix is the knot/cell index
c
      ix =0
      do iy=0,ny-1
         
         psitarg=((fpsi(ix,iy,0)+fpsi(ix,iy,2))/2.0)/
     .            (-psimin_carre)+1.0
         write(nfin,108) ix+1,iy+1,psitarg,
     >          (crx(ix,iy,0)+crx(ix,iy,2))/2.0,
     >          (cry(ix,iy,0)+cry(ix,iy,2))/2.0

      end do 

c
c     jdemod -
c     The iy=ny-1 elements at the end of the grid would appear to be elements 1 and 3
c
      ix =nx-1
      do iy=0,ny-1
         
         psitarg=((fpsi(ix,iy,1)+fpsi(ix,iy,3))/2.0)/
     .            (-psimin_carre)+1.0
         write(nfin,108) ix+1,iy+1,psitarg,
     >          (crx(ix,iy,0)+crx(ix,iy,2))/2.0,
     >          (cry(ix,iy,0)+cry(ix,iy,2))/2.0

      end do 




      return
      end
c
c
c

      subroutine get_psimin_carre(psimin_carre)
      implicit none
      include 'CARRETYPES.F'
      include 'CARREDIM.F'
      real(kind=R8) psimin_carre
      
      character lign80*80
      integer nx,ny,i,j
      integer ierror,iflag
      REAL(kind=R8) x(nxmax), y(nymax), psi(nxmax,nymax)
      
      ierror = 0
      iflag = 0

      OPEN(UNIT=7, FILE='rzpsi.dat', STATUS='old')

*
*..3.0  Read the data
*

*..Read the values of x

100   format(a)
      iflag=-1
      call entete(7,'$r',iflag)
      read(7,100)lign80
      i=index(lign80,'=')
      call rdfrin(11,lign80(i+1:80),nx,ierror)
      READ(7,*) (x(i), i=1, nx)

*..Read the values of y

      call entete(7,'$z',iflag)
      read(7,100)lign80

      i=index(lign80,'=')

      call rdfrin(11,lign80(i+1:80),ny,ierror)
      READ(7,*) (y(i), i=1, ny)

*..Read the values of psi

      call entete(7,'$psi',iflag)
      READ(7,*) ((psi(i,j), i=1, nx), j=1, ny)

      psimin_carre = 1e20
      do i = 1,nx
         do j = 1,ny
            
           psimin_carre=min(psi(i,j),psimin_carre)
        end do 
      end do


      return
      end 

