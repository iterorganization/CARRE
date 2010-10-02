
!***********************************************************************
      subroutine plurap(tx1,ty1,n1,tx2,ty2,n2,x0,y0,ind0)
!***********************************************************************
      implicit none
!  Determination des coordonnees (x0,y0) et de l'indice de segment ind0
!  du point de la structure parametrisee par [tx1, ty1, n1], qui est
!  le plus rapproche de la courbe parametrisee par [tx2, ty2,n2].
!  N.B.: On suppose qu'il n'y a pas intersection entre les deux courbes.
!
!  arguments
      integer n1,n2,ind0
      real*8 tx1(n1),ty1(n1),tx2(n2),ty2(n2),x0,y0
!
!  variables locales
      integer i1,i2
      real*8 h,hmin,x,y,x1,y1,x2,y2,base,zero,un
      parameter(zero=0.,un=1.)
!
!  procedures
      intrinsic abs,sqrt
!
!  calcul
!
!  0.   initialisation
      hmin=sqrt((tx1(1)-tx2(1))**2+(ty1(1)-ty2(1))**2)
      x0=tx1(1)
      y0=ty1(1)
      ind0=1
!
!  1.   boucle sur tous les points de la courbe parametrisee et, pour
!       chaque point, boucle sur chaque segment de la structure.
      do i2=1,n2
        x=tx2(i2)
        y=ty2(i2)
        do i1=1,n1-1
          x1=tx1(i1)
          y1=ty1(i1)
          x2=tx1(i1+1)
          y2=ty1(i1+1)
          base=((x-x1)*(x2-x1)+(y-y1)*(y2-y1)) & 
     &      /((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
!***
!         print*,'i2, i1=',i2,i1
!         print*,' x,  y=',x,y
!         print*,'x1, y1=',x1,y1
!         print*,'x2, y2=',x2,y2
!         print*,'base=',base
!***
          if(base.gt.un) then
            h=sqrt((x-x2)*(x-x2)+(y-y2)*(y-y2))
            if(h.lt.hmin) then
              hmin=h
              x0=x2
              y0=y2
              ind0=i1
            endif
          elseif(base.lt.zero) then
            h=sqrt((x-x1)*(x-x1)+(y-y1)*(y-y1))
            if(h.lt.hmin) then
              hmin=h
              x0=x1
              y0=y1
              ind0=i1
            endif
          else
            h=abs((x-x1)*(y2-y1)-(y-y1)*(x2-x1)) & 
     &        /sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
            if(h.lt.hmin) then
              hmin=h
              x0=x1+base*(x2-x1)
              y0=y1+base*(y2-y1)
              ind0=i1
            endif
          endif
        enddo
      enddo
!
!  2.   boucle sur tous les points de la structure et, pour chaque point,
!       boucle sur chaque segment de la courbe parametrisee.
      do i1=1,n1
        x=tx1(i1)
        y=ty1(i1)
        do i2=1,n2-1
          x1=tx2(i2)
          y1=ty2(i2)
          x2=tx2(i2+1)
          y2=ty2(i2+1)
          base=((x-x1)*(x2-x1)+(y-y1)*(y2-y1)) & 
     &      /((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
!***
!         print*,'i1, i2=',i1,i2
!         print*,' x,  y=',x,y
!         print*,'x1, y1=',x1,y1
!         print*,'x2, y2=',x2,y2
!         print*,'base=',base
!***
          if(base.gt.un) then
            h=sqrt((x-x2)*(x-x2)+(y-y2)*(y-y2))
            if(h.lt.hmin) then
              hmin=h
              x0=x
              y0=y
              ind0=min(i1,n1-1)
            endif
          elseif(base.lt.zero) then
            h=sqrt((x-x1)*(x-x1)+(y-y1)*(y-y1))
            if(h.lt.hmin) then
              hmin=h
              x0=x
              y0=y
              ind0=min(i1,n1-1)
            endif
          else
            h=abs((x-x1)*(y2-y1)-(y-y1)*(x2-x1)) & 
     &        /sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
            if(h.lt.hmin) then
              hmin=h
              x0=x
              y0=y
              ind0=min(i1,n1-1)
            endif
          endif
        enddo
      enddo
      return
      end
