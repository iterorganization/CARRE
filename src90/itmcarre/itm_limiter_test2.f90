program itm_limiter_test2

  use itm_limiter_mockup2

  implicit none


  type(type_limiter) :: limiter

  call itmlimReadFromStructureFile_mockup2('structure.dat', limiter)

  write (*,*) 'Read ', size( limiter % element ), ' structures'


end program itm_limiter_test2
