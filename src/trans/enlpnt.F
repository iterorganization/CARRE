c=======================================================================
      subroutine enlpnt(v,nelem,xmail,ymail,ncoord,i1,i2)
      implicit none
c  on retire un point i1 de la liste des noeuds et on le remplace par le
c  point d'indice i2
c
c  arguments
      integer nelem,v(3,nelem),ncoord,i1,i2
      real xmail(ncoord),ymail(ncoord)
c
c  variables locales
      integer i,j,ielem
c
c  calcul
    
      ncoord=ncoord-1
      do i=i1,ncoord
        xmail(i)=xmail(i+1)
        ymail(i)=ymail(i+1)
      enddo

      do ielem=1,nelem
        do j=1,3
          if(v(j,ielem).eq.i1) v(j,ielem)=i2
          if(v(j,ielem).gt.i1) v(j,ielem)=v(j,ielem)-1
        enddo
      enddo

      return
      end
