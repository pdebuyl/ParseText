program test_PT
  use ParseText
  use tester
  implicit none

  type(PTo) :: CF
  type(tester_t) :: test

  double precision :: k

  call PTparse(CF,'sample_file',5)

  call test% init()

  k = PTread_d(CF,'k', 3.d0)
  call test% assert_equal(k, 3.d0)

  call PTkill(CF)

  call test% print()

end program test_PT
