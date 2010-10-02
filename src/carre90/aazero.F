
!***********************************************************************
      REAL*8 FUNCTION aazero(fcn,x0,dx0,dxmin,fmin,xmin,xmax,itmax)
!***********************************************************************
      IMPLICIT NONE
!
!  Purpose: find the zero of a REAL function fcn by the secant method.
!
!  Prerequisites: none
!
!  References: none
!
!  Revision    author              comment
!  15/11/91     R. Marchand         initial installation
!
!  Input/output variables:
      INTEGER itmax
      REAL*8 x0,dx0,dxmin,fmin,xmin,xmax
!  x0: initial guess
!  dx0: initial step
!  dxmin: minimum acceptable step: aazero is accepted if dx.le.dxmin
!  fmin: minimum acceptable function value: aazero is accepted if
!        f.le.fmin*f1
!  xmin: lower bound of the x interval in which to find the root
!  xmax: upper bound of the x interval in which to find the root
!        N.B.: if xmin=xmax, no bounds are assumed.
!  itmax: maximum number of iterations allowable
!
!  Common variables: none
!
!  Local variables:
      INTEGER it
      REAL*8 dxmax,x1,f1,x2,f2,zfmin,dx,dxinv,one,xl,xr,sgl,sgr,sg
      save one
!
!  Procedures:
      REAL*8 fcn
      external fcn
      intrinsic abs,sign,min,max
!
!  Data:
      data one/1./
!
!  Computation:
!
      dxmax=100.*abs(dx0)
      if(xmin.lt.xmax) then
        x1=max(xmin,min(xmax,x0))
      else
        x1=x0
      endif
      xl=x1
      xr=x1
!
      f1=fcn(x1)
      if(f1.eq.0.) then
        x2=x0
        go to 20
      endif
      zfmin=abs(fmin*f1)
      sgl=sign(one,f1)
      sgr=sgl
!
!     if(x1-xmin.gt.xmax-x1) then
!       dx=abs(dx0)
!     else
!       dx=-abs(dx0)
!     endif
      if(xmin.lt.xmax) then
        if(x1.eq.xmax) then
          dx=abs(dx0)
        elseif(x1.eq.xmin) then
          dx=-abs(dx0)
        else
          dx=-dx0
        endif
      else
        dx=-dx0
      endif
!
      do 10 it=1,itmax
      x2=x1-dx
      if(xmin.lt.xmax) then
        if(sgl.ne.sgr) then
          x2=max(xl,min(xr,x2))
        else
          x2=max(xmin,min(xmax,x2))
        endif
        if((abs(x2-x1).le.dxmin .and. it.gt.2) .or. x1.eq.x2) go to 30
      endif
      f2=fcn(x2)
      if(abs(f2).le.zfmin) go to 20
      sg=sign(one,f2)
      if(sgl.eq.sgr) then
        xl=min(xl,x2)
        xr=max(xr,x2)
        if(sg.ne.sgl) then
          if(x2.lt.xr) then
            sgl=sg
            xl=x2
          else
            sgr=sg
            xr=x2
          endif
        endif
      endif
      if(sgl.ne.sgr) then
        if(sg.eq.sgl) then
          xl=x2
          sgl=sg
        else
          xr=x2
          sgr=sg
        endif
      endif
!
      dxinv=(f2-f1)/(f2*(x2-x1))
      if(abs(dxinv)*dxmax.le.1.) then
        dx=sign(dxmax,dxinv)
      else
        dx=1./dxinv
        if(abs(dxinv)*dxmin.ge.1. .and. it.gt.1) go to 30
      endif
!
      if(sgl.ne.sgr) then
        if(sg.ne.sgl) then
!         shoot to the left
          if(dx.gt.0.) then
            dx=min(dx,0.8*(x2-xl))
          else
            dx=0.5*(x2-xl)
          endif
        else
!         shoot to the right
          if(dx.lt.0.) then
            dx=max(dx,0.8*(x2-xr))
          else
            dx=0.5*(x2-xr)
          endif
        endif
      endif
!
      x1=x2
      f1=f2
10    continue
!
!  no convergence
      WRITE(6,*)' warning: no convergence in aazero'
      aazero=x0
      RETURN
!
!  good convergence, the function is sufficiently small
20    continue
      aazero=x2
      RETURN
!
!  good convergence, step size is sufficiently small
30    continue
      aazero=x2-dx
      if(xmin.lt.xmax) then
        aazero=max(xmin,min(xmax,aazero))
      endif
      RETURN
      end
