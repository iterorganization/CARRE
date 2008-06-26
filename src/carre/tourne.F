
************************************************************************
      integer function tourne(i,sens,n)
************************************************************************
      implicit none
c
c  variation cyclique de periode n-1. Le 1er point coincide avec le enieme.
c
c  arguments
      integer i,sens,n
c
c  calculs
      tourne=i+sens
      if(tourne.eq.0) then
        tourne=n-1
      elseif(tourne.eq.n+1) then
        tourne=2
      endif
      return
      end
