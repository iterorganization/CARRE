program itm_limiter_test1

  use itm_limiter_mockup1

  implicit none


  type(type_limelement) :: limiter

  call itmlimReadFromStructureFile('structure.dat', limiter)

  write (*,*) 'Read ', size( limiter % limgeometry % npoints, 1 ), ' structures'


end program itm_limiter_test1
