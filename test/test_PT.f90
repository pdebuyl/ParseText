program test_PT
  use ParseText
  use tester
  implicit none

  type(PTo) :: CF
  type(tester_t) :: test

  double precision :: x,y(5), y_known(5)
  integer :: N(3), N_known(3)
  logical :: is_att(4), is_att_known(4)
  logical :: success
  integer :: i

  N_known = (/ -5 , 2, 10 /)
  y_known = (/ 1.d0,  -100.d0,  10d5,  39.d0,  0.d0 /)
  is_att_known = (/ .false., .false., .false., .true. /)
  call PTparse(CF,'sample_file',5)
  call test% init()

  x = PTread_d(CF,'x')
  call test% assert_equal(x, 1.d0)

  N = PTread_ivec(CF, 'N', size(N))

  call test% assert_equal(N, N_known)

  y = PTread_dvec(CF, 'y', size(y))

  call test% assert_close(y, y_known)

  is_att = PTread_lvec(CF,'is_att',size(is_att))

  call test% assert_equal(is_att, is_att_known)

  call PTkill(CF)

  call test% print()

end program test_PT
