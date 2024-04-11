      subroutine construct_vessel_polygons
!======================================================================
!***  Contructs vessel polygons
!======================================================================
      use KindDefinitions
      implicit none
#include <FCRCOM.F>
      integer(Short) :: i, ipt, npts, npth, nptt, ilbl
      integer(Short) :: fcLblmin, fcLblmax
      integer(Short), allocatable :: fcLbls(:)
      real(rKind) :: xsrt, ysrt, xend, yend
      real(rKind) :: xhead(nmstr), yhead(nmstr), xtail(nmstr), ytail(nmstr)
      logical lfound, ldir, lused(nvess)
      logical points_match
      external points_match
      intrinsic min, max
!======================================================================

      ! init
      nstrv = 0
      lstrv = 0
      lclstrv = .false.
      xstrv = 0.0_rKind
      ystrv = 0.0_rKind

      xhead = 0.0_rKind
      yhead = 0.0_rKind
      xtail = 0.0_rKind
      ytail = 0.0_rKind

      ! check for presence at least one vessel segment
      if (nvess.le.0) then
        ! no vessel elements present - nothing to be done
        write (*,*) 'No vessel elements defined'
        return
      end if

      ! check which values of fcLbl are present
      fcLblmin = 1e5
      fcLblmax = 0
      do i = 1, nvess
        fcLblmin = min(fcLbl(vess_elm(i)),fcLblmin)
        fcLblmax = max(fcLbl(vess_elm(i)),fcLblmax)
      end do

      ! consistency checks
      if (fcLblmin.ne.1.and..not.(fcLblmin.eq.0.and.fcLblmax.eq.0)) then
        write (*,*) 'Warning: possibly inconsistent fcLbl definition in DG model'
        do i = 1, nvess
          if (fcLbl(vess_elm(i)).eq.0) then
            write (*,*) 'Element ', vess_elm(i), ' has fcLbl 0.'
          endif
        end do
        stop
      elseif (fcLblmin.eq.0.and.fcLblmax.eq.0) then
        write (*,*) 'All vessel elements have fcLbl = 0.'
        write (*,*) 'Labels will be automatically assigned to individual polygon pieces.'
      end if
      allocate(fcLbls(fcLblmax-fcLblmin+1))
      fcLbls = [(i, i = fcLblmin, fcLblmax)]

#ifdef DBG
      write (*,*) 'construct_vessel_polygons: fcLbl'
      write (*,*) 'min and max labels: ', fcLblmin, fcLblmax
      do i = 1,nvess
        write (*,*) 'segment i =',i,', label ',fcLbl(vess_elm(i))
      end do
#endif

      ! sort elements into polygons
      lused = .false.
      npts  = 0
      ilbl  = 0
      do while (.not.all(lused))

        nstrv = nstrv + 1
        ilbl  = ilbl + 1

        if (ilbl .gt. (fcLblmax - fcLblmin + 1)) then
          write (*,*) 'Not all vessel elements assigned to a polygon.'
          write (*,*) 'Check for inconsistent fcLbls of the elements.'
          write (*,*) 'Elements with the same fcLbl must form a single'
          write (*,*) 'open or closed polygon (no gaps).'
          write (*,*) ilbl, fcLblmax, fcLblmax
          stop ' ==> Check DG model'
        endif

        ! find suitable unused element to start search
        lfound = .false.
        i = 0
        do while (.not.lfound)
          i = i + 1
          if (.not.lused(i).and.fcLbl(vess_elm(i)).eq.fcLbls(iLbl)) lfound = .true.
          if (i.gt.nvess) then
            write (*,*) 'Found no elements with fcLbl = ',fcLbls(iLbl), '.'
            write (*,*) 'Min fcLbl = ', fcLblmin
            write (*,*) 'Max fcLbl = ', fcLblmax
            stop ' ==> Check DG model'
          end if
        end do
 
        ! set the initial element
        lused(i) = .true.
        xsrt = p1(1,vess_elm(i))
        ysrt = p1(2,vess_elm(i))
        xend = p2(1,vess_elm(i))
        yend = p2(2,vess_elm(i))

        xhead(1) = xsrt
        yhead(1) = ysrt
        xtail(1) = xend
        ytail(1) = yend

        ! first pass: find end point of the polygon
        ldir = .true.
        ipt = 1
        do while (ldir.and..not.all(lused))
          lfound = .false.
          i = 0
          do while (ldir.and..not.lfound)
            i = i + 1
            if (i.gt.nvess) then
               ! no further point found in this direction
               ! switch to other side
               ldir = .false.
            elseif (.not.lused(i).and.fclbl(vess_elm(i)).eq.fclbls(ilbl)) then
              if (points_match (xend, yend, p1(1,vess_elm(i)), p1(2,vess_elm(i)))) then
                lfound   = .true.
                lused(i) = .true.
                ipt = ipt + 1
                xend = p2(1,vess_elm(i))
                yend = p2(2,vess_elm(i))
                xtail(ipt) = p2(1,vess_elm(i))
                ytail(ipt) = p2(2,vess_elm(i))
              elseif (points_match (xend, yend, p2(1,vess_elm(i)), p2(2,vess_elm(i)))) then
                lfound   = .true.
                lused(i) = .true.
                ipt = ipt + 1
                xend = p1(1,vess_elm(i))
                yend = p1(2,vess_elm(i))
                xtail(ipt) = p1(1,vess_elm(i))
                ytail(ipt) = p1(2,vess_elm(i))
              end if
            endif
          end do
        end do
        nptt = ipt

        ! second pass: find start point of polygon
        ipt = 1
        do while (.not.ldir.and..not.all(lused))
          lfound = .false.
          i = 0
          do while (.not.ldir.and..not.lfound)
            i = i + 1
            if (i.gt.nvess) then
               ! no further point found in this direction
               ! terminate search for this polygon
               ldir = .true.
            elseif (.not.lused(i).and.fclbl(vess_elm(i)).eq.fclbls(ilbl)) then
              if (points_match (xsrt, ysrt, p1(1,vess_elm(i)), p1(2,vess_elm(i)))) then
                lfound   = .true.
                lused(i) = .true.
                ipt = ipt + 1
                xsrt = p2(1,vess_elm(i))
                ysrt = p2(2,vess_elm(i))
                xhead(ipt) = p2(1,vess_elm(i))
                yhead(ipt) = p2(2,vess_elm(i))
              elseif (points_match (xsrt, ysrt, p2(1,vess_elm(i)), p2(2,vess_elm(i)))) then
                lfound   = .true.
                lused(i) = .true.
                ipt = ipt + 1
                xsrt = p1(1,vess_elm(i))
                ysrt = p1(2,vess_elm(i))
                xhead(ipt) = p1(1,vess_elm(i))
                yhead(ipt) = p1(2,vess_elm(i))
              end if
            endif
          end do
        end do
        npth = ipt

        ! stitch pieces together
        if (points_match(xhead(npth), yhead(npth), xtail(nptt), ytail(nptt))) then
          ! closed polygon; remove duplicate start/end point here
          ! (will be added again during writing structure.dat file)
          lclstrv(nstrv) = .true.
          lstrv(nstrv) = nptt + npth - 1
          xstrv(npts+1:npts+npth) = xhead(npth:1:-1)
          ystrv(npts+1:npts+npth) = yhead(npth:1:-1)
          xstrv(npts+npth+1:npts+lstrv(nstrv)) = xtail(1:nptt-1)
          ystrv(npts+npth+1:npts+lstrv(nstrv)) = ytail(1:nptt-1)
        else
          ! open polygon
          lclstrv(nstrv) = .false.
          lstrv(nstrv) = nptt + npth
          xstrv(npts+1:npts+npth) = xhead(npth:1:-1)
          ystrv(npts+1:npts+npth) = yhead(npth:1:-1)
          xstrv(npts+npth+1:npts+lstrv(nstrv)) = xtail(1:nptt)
          ystrv(npts+npth+1:npts+lstrv(nstrv)) = ytail(1:nptt)
        endif
        npts = npts + nptt + npth

      end do

      deallocate(fclbls)

      ! Some consistency checks
      ! To be added: check that complete vessel is closed in case of multiple (open) polygons
      if (fclblmin.eq.0.and.fclblmax.eq.0) then
        !if (nstrv.ne.1) then
        !  write (*,*) 'Problem with definition vessel elements.'
        !elseif (.not.lclstrv(1)) then
        !  write (*,*) 'Vessel polygon not closed'
        !end if
      end if
       

      write (*,*) 'construct_vessel_polygons -- built ', nstrv, ' vessel polygons'

!======================================================================
      end 

      real(rKind) function points_dist( x1, y1, x2, y2)
        use KindDefinitions
        implicit none
        real(rKind), intent(in) :: x1, y1, x2, y2

        points_dist = sqrt( (x1-x2)**2 + (y1-y2)**2 )

      end function points_dist

      logical function points_match(x1 , y1 , x2 ,y2)
        use KindDefinitions
        implicit none
        real(rKind), intent(in) :: x1, y1, x2, y2
        real(rKind) :: points_dist
        external points_dist

        points_match = points_dist(x1, y1, x2, y2).lt.1.0e-6_rKind

      end function points_match
        
