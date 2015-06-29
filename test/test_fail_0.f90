program test_PT
  use ParseText
  use tester
  implicit none

  type(PTo) :: CF
  type(tester_t) :: tester

  double precision :: x

  call PTparse(CF,'sample_file',5)
  call tester% init()

  x = PTread_d(CF,'x')
  call tester% assert_equal(x, 2.d0, fail=.true.)

  call PTkill(CF)

  call tester% print()

end program test_PT
