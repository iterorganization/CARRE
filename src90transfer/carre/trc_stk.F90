      subroutine trc_stk
!
!  version : 22.06.98 14:43
!
!======================================================================
!*** Manage the calling stack for debugging purposes.
!*** Is to be called as trc_stk_in before and trc_stk_out after each
!*** call to the subroutines to be traced.
!*** Two text parameters (*8) given to trc_stk_in appear on the output
!======================================================================
      character*(*) from, at
      integer n
      parameter (n=50)
      integer i, l
      character*8 call_from(n), call_at(n)
      data call_from /n*' '/, call_at /n*' '/, l /0/
      save
!======================================================================
!*** Output of the tracing information
        if(l.gt.0) then
          write(0,*) 'Tracing info:'
          write(0,'(6h from ,a,4h at ,a)') (call_from(i), & 
     &                                             call_at(i), i=l,1,-1)
        end if
      return
!======================================================================
      entry trc_stk_in(from,at)
!======================================================================
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
!======================================================================
      entry trc_stk_out
!======================================================================
      if(l.gt.0) l = l - 1
      return
!======================================================================
      entry trc_stk_reset
!======================================================================
      l=0
      return
!======================================================================
      end
