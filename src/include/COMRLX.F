c  version : 07.07.97 17:36
c
      integer nrelax,ipol1,ipoln
      real(kind=R8)
     .       l0(npmamx),l1(npmamx),l2(npmamx),ort1(npmamx),ort2(npmamx),
     .       relax,pasmin,del,rlcept,ortmax,ortpur1(npmamx),
     .       ortpur2(npmamx),propo1(npmamx),propo2(npmamx),
     .       varr1(npmamx),varr2(npmamx),tot1(npmamx),tot2(npmamx)
      common/comrlx/l0,l1,l2,ort1,ort2,relax,pasmin,del,
     .       rlcept,ortmax,ortpur1,ortpur2,propo1,propo2,varr1,varr2,
     .       tot1,tot2,
     .       nrelax,ipol1,ipoln
      save /comrlx/
