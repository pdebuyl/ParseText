program test_PT
  use ParseText
  use tester
  implicit none

  type(PTo) :: CF
  type(tester_t) :: tester

  double precision :: k

  call PTparse(CF,'sample_file',5)

  call tester% init()

  k = PTread_d(CF,'k', 3.d0)
  call tester% assert_equal(k, 3.d0)

  call PTkill(CF)

  call tester% print()

end program test_PT
