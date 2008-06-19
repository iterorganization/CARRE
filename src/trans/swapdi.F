c=======================================================================
      subroutine swapdi(t,r,v,e,cotali)
      implicit none
c  change la diagonale dans deux triangles adjacents
c
c  arguments
      integer i,t,r,v(3,*),e(3,*),cotali(*)
c
c  variables locales
      integer v1,v2,v3,u1,u2,u3,e1,e2,e3,f1,f2,f3
c
c  procedures
      integer edg
      external edg
c
c  calcul
      i=edg(t,r,e)
      v2=v(i,t)
      e2=e(i,t)
      i=mod(i,3)+1
      v3=v(i,t)
      e3=e(i,t)
      i=mod(i,3)+1
      v1=v(i,t)
      e1=e(i,t)
      i=edg(r,t,e)
      u2=v(i,r)
      f2=e(i,r)
      i=mod(i,3)+1
      u3=v(i,r)
      f3=e(i,r)
      i=mod(i,3)+1
      u1=v(i,r)
      f1=e(i,r)
      v(1,t)=v1
      v(2,t)=v2
      v(3,t)=u1
      v(1,r)=v1
      v(2,r)=u1
      v(3,r)=u2
      if(f3.gt.0) e(edg(f3,r,e),f3)=t
      if(e3.gt.0) e(edg(e3,t,e),e3)=r
      e(1,t)=e1
      e(2,t)=f3
      e(3,t)=r
      e(1,r)=t
      e(2,r)=f1
      e(3,r)=e3
      cotali(t)=2
      cotali(r)=3
      return
      end
