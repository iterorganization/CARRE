      subroutine trc_stk
c
c  version : 22.06.98 14:43
c
c======================================================================
c*** Manage the calling stack for debugging purposes.
c*** Is to be called as trc_stk_in before and trc_stk_out after each
c*** call to the subroutines to be traced.
c*** Two text parameters (*8) given to trc_stk_in appear on the output
c======================================================================
      character*(*) from, at
      integer n
      parameter (n=50)
      integer i, l
      character*8 call_from(n), call_at(n)
      data call_from /n*' '/, call_at /n*' '/, l /0/
      save
c======================================================================
c*** Output of the tracing information
        if(l.gt.0) then
          write(0,*) 'Tracing info:'
          write(0,'(a6,a,a4,a)') (' from ',call_from(i),' at ',
     ,                                             call_at(i), i=l,1,-1)
        end if
      return
c======================================================================
      entry trc_stk_in(from,at)
c======================================================================
      if(l.ge.n) then
        do i=2,n
          call_from(i-1) = call_from(i)
          call_at(i-1)   = call_at(i)
        end do
      else
        l = l + 1
      end if
      call_from(l) = from
      call_at(l) = at
      return
c======================================================================
      entry trc_stk_out
c======================================================================
      if(l.gt.0) l = l - 1
      return
c======================================================================
      entry trc_stk_reset
c======================================================================
      l=0
      return
c======================================================================
      end
