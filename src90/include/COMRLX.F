!  version : 07.07.97 17:36
!
      integer nrelax,ipol1,ipoln
      real(rKind) :: l0(npmamx),l1(npmamx),l2(npmamx), &
     &  relax,pasmin,del,rlcept,ortmax
      common/comrlx/l0,l1,l2,relax,pasmin,del,rlcept,ortmax, &
     &  nrelax,ipol1,ipoln
!,ort1(npmamx),ort2(npmamx),
!ort1,ort2,

      save /comrlx/
