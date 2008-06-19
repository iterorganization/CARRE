c  version : 07.07.97 17:36
c
      integer nrelax,ipol1,ipoln
      real*8 l0(npmamx),l1(npmamx),l2(npmamx),ort1(npmamx),ort2(npmamx),
     .  relax,pasmin,del,rlcept,ortmax
      common/comrlx/l0,l1,l2,ort1,ort2,relax,pasmin,del,rlcept,ortmax,
     .  nrelax,ipol1,ipoln
      save /comrlx/
