      subroutine cntour(f1,f2,n1,n2,xmin,xmax,ymin,ymax)
c
c  version : 07.07.97 20:32
c
c======================================================================
#ifdef DIMENSIONS_MODULE
      use b2mod_dimensions
#endif
      use carre_dimensions
      use carre_types
      use perim
      implicit none
c  lignes de niveau pour les deux fonctions f1=0 et f2=0
c

c  arguments
      integer n1,n2
      real(kind=R8) f1(nxmax,n2),f2(nxmax,n2),xmin,xmax,ymin,ymax
c
c  variables locales
      integer iwrk(1000),i1,i2,ll
      real y(nxmax,nymax),rwrk(5000),x1min,x1max,x2min,x2max,
     ,     x1minp,x1maxp,x2minp,x2maxp,x1ap,x1bp,x2ap,x2bp
      character sclx*3,scly*3
c
c  procedures
      external cpseti,cprect,cpsetr,cpcldr,endpag,getset,newpag
c======================================================================
c  calculs
c
c  1.   premier tableau
c
c  1.1  copie des variables double a simple precision
      do i1=1,n1
        do i2=1,n2
          y(i1,i2)=f1(i1,i2)
        end do
      end do
c
c  1.2  on trace les lignes de niveau
c
      x1min=xmin
      x1max=xmax
      x2min=ymin
      x2max=ymax
      sclx='LIN'
      scly='LIN'
      write(*,*) 'Starting'
      call newpag(x1min,x1max,x2min,x2max,'R (m)$','Z (m)$',
     .  ' ',sclx,scly)
      write(*,*) 'newpag OK'
      call getset(x1a,x1b,x2a,x2b,x1minp,x1maxp,x2minp,x2maxp,ll)
      if(ll.eq.1 .or. ll.eq.2) then
        x1ap=x1a+(x1min-x1minp)/(x1maxp-x1minp)*(x1b-x1a)
        x1bp=x1a+(x1max-x1minp)/(x1maxp-x1minp)*(x1b-x1a)
      else
        x1ap=x1a+(log10(x1min)-log10(x1minp))/
     .    (log10(x1maxp)-log10(x1minp))*(x1b-x1a)
        x1bp=x1a+(log10(x1max)-log10(x1minp))/
     .    (log10(x1maxp)-log10(x1minp))*(x1b-x1a)
      endif
      if(ll.eq.1 .or. ll.eq.3) then
        x2ap=x2a+(x2min-x2minp)/(x2maxp-x2minp)*(x2b-x2a)
        x2bp=x2a+(x2max-x2minp)/(x2maxp-x2minp)*(x2b-x2a)
      else
        x2ap=x2a+(log10(x2min)-log10(x2minp))/
     .    (log10(x2maxp)-log10(x2minp))*(x2b-x2a)
        x2bp=x2a+(log10(x2max)-log10(x2minp))/
     .    (log10(x2maxp)-log10(x2minp))*(x2b-x2a)
      endif
c
      call cpseti('SET',1)
      call cpsetr('VPS',0.)
      call cpsetr('VPL - VIEWPORT LEFT',x1ap)
      call cpsetr('VPR - VIEWPORT RIGHT',x1bp)
      call cpsetr('VPB - VIEWPORT BOTTOM',x2ap)
      call cpsetr('VPT - VIEWPORT TOP',x2bp)
c
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
c
c  2.   deuxieme tableau
c  2.1  copie des variables double a simple precision
      do i1=1,n1
        do i2=1,n2
          y(i1,i2)=f2(i1,i2)
        end do
      end do
c
c  2.2  on trace les lignes de niveau
c
      call cpseti('CLS - CONTOUR LEVEL SELECTION',0)
      call cpseti('NCL - NUMBER OF CONTOUR LEVELS',1)
      call cpseti('PAI - PARAMETER ARRAY INDEX',1)
      call cpsetr('CLV - CONTOUR LEVEL VALUE',0.)
      call cpseti('CLU - CONTOUR LEVEL USE',1)
      call cpseti('CLD - CONTOUR LINE DASH PATTERN',21845)
      call cprect(y,nxmax,n1,n2,rwrk,5000,iwrk,1000)
      call cpcldr(y,rwrk,iwrk)
c
cnba  The 'endpag' call below is moved to the end of the 'selptx' routine
cnba  for simultaneous visualization of the dpsi/d. = 0 lines and the
cnba  selection of X- and O-points.
cnba  L'appel a 'endpag' est deplace a la fin de la routine 'selptx' de
cnba  facon a ce qu'on puisse voir les lignes de derivee nulle en meme
cnba  temps que l'utilisateur chiosit les points X et O.
cnba  call endpag
c
      return
      end
