      subroutine cntour(f1,f2,n1,n2,xmin,xmax,ymin,ymax)
!
!  version : 07.07.97 20:32
!
!======================================================================
      use KindDefinitions
      implicit none
!  lignes de niveau pour les deux fonctions f1=0 et f2=0
!

!ank-970707: dimensions from the file
!  dimensions
#include <CARREDIM.F>

!  arguments
      integer n1,n2
      real(rKind) :: f1(nxmax,n2),f2(nxmax,n2),xmin,xmax,ymin,ymax
!
!  variables en common
#include <PERIM.F>
!
!  variables locales
      integer iwrk(1000),i1,i2,ll
      real(Single) :: y(nxmax,nymax),rwrk(5000),x1min,x1max,x2min,x2max, &
     &     x1minp,x1maxp,x2minp,x2maxp,x1ap,x1bp,x2ap,x2bp
      character sclx*3,scly*3
!
!  procedures
      external cpseti,cpsetr,cpcldr,endpag,getset,newpag
!======================================================================
!  calculs
!
!  1.   premier tableau
!
!  1.1  copie des variables double a simple precision
      do i1=1,n1
        do i2=1,n2
          y(i1,i2)=real(f1(i1,i2),Single)
        end do
      end do
!
!  1.2  on trace les lignes de niveau
!
      x1min=real(xmin,Single)
      x1max=real(xmax,Single)
      x2min=real(ymin,Single)
      x2max=real(ymax,Single)
      sclx='LIN'
      scly='LIN'
      write(*,*) 'Starting'
      call newpag(x1min,x1max,x2min,x2max,'R (m)$','Z (m)$', &
     &  ' ',sclx,scly)
      write(*,*) 'newpag OK'
      call getset(x1a,x1b,x2a,x2b,x1minp,x1maxp,x2minp,x2maxp,ll)
      if(ll.eq.1 .or. ll.eq.2) then
        x1ap=x1a+(x1min-x1minp)/(x1maxp-x1minp)*(x1b-x1a)
        x1bp=x1a+(x1max-x1minp)/(x1maxp-x1minp)*(x1b-x1a)
      else
        x1ap=x1a+(log10(x1min)-log10(x1minp))/ &
     &    (log10(x1maxp)-log10(x1minp))*(x1b-x1a)
        x1bp=x1a+(log10(x1max)-log10(x1minp))/ &
     &    (log10(x1maxp)-log10(x1minp))*(x1b-x1a)
      endif
      if(ll.eq.1 .or. ll.eq.3) then
        x2ap=x2a+(x2min-x2minp)/(x2maxp-x2minp)*(x2b-x2a)
        x2bp=x2a+(x2max-x2minp)/(x2maxp-x2minp)*(x2b-x2a)
      else
        x2ap=x2a+(log10(x2min)-log10(x2minp))/ &
     &    (log10(x2maxp)-log10(x2minp))*(x2b-x2a)
        x2bp=x2a+(log10(x2max)-log10(x2minp))/ &
     &    (log10(x2maxp)-log10(x2minp))*(x2b-x2a)
      endif
!
      call cpseti('SET',1)
      call cpsetr('VPS',0.)
      call cpsetr('VPL - VIEWPORT LEFT',x1ap)
      call cpsetr('VPR - VIEWPORT RIGHT',x1bp)
      call cpsetr('VPB - VIEWPORT BOTTOM',x2ap)
      call cpsetr('VPT - VIEWPORT TOP',x2bp)
!
      call cpseti('CLS - CONTOUR LEVEL SELECTION',0)
      call cpseti('NCL - NUMBER OF CONTOUR LEVELS',1)
      call cpseti('PAI - PARAMETER ARRAY INDEX',1)
      call cpsetr('CLV - CONTOUR LEVEL VALUE',0.)
      call cpseti('CLU - CONTOUR LEVEL USE',1)
      call cpseti('CLD - CONTOUR LINE DASH PATTERN',65535)
      write(*,*) 'cpsets ok'
      call cprect(y,nxmax,n1,n2,rwrk,5000,iwrk,1000)
      write(*,*) 'cprect ok'
      call cpcldr(y,rwrk,iwrk)
      write(*,*) 'cpcldr ok'
!
!  2.   deuxieme tableau
!  2.1  copie des variables double a simple precision
      do i1=1,n1
        do i2=1,n2
          y(i1,i2)=real(f2(i1,i2),Single)
        end do
      end do
!
!  2.2  on trace les lignes de niveau
!
      call cpseti('CLS - CONTOUR LEVEL SELECTION',0)
      call cpseti('NCL - NUMBER OF CONTOUR LEVELS',1)
      call cpseti('PAI - PARAMETER ARRAY INDEX',1)
      call cpsetr('CLV - CONTOUR LEVEL VALUE',0.)
      call cpseti('CLU - CONTOUR LEVEL USE',1)
      call cpseti('CLD - CONTOUR LINE DASH PATTERN',21845)
      call cprect(y,nxmax,n1,n2,rwrk,5000,iwrk,1000)
      call cpcldr(y,rwrk,iwrk)
!
!nba  The 'endpag' call below is moved to the end of the 'selptx' routine
!nba  for simultaneous visualization of the dpsi/d. = 0 lines and the
!nba  selection of X- and O-points.
!nba  L'appel a 'endpag' est deplace a la fin de la routine 'selptx' de
!nba  facon a ce qu'on puisse voir les lignes de derivee nulle en meme
!nba  temps que l'utilisateur chiosit les points X et O.
!nba  call endpag
!
      return
      end
