program test_PT
  use ParseText
  implicit none

  type(PTo) :: CF

  double precision :: x,y(5)
  integer :: N(3)
  logical :: is_att(4)

  call PTparse(CF,'sample_file',5)

  x = PTread_d(CF,'x')

  write(*,*) x

  N = PTread_ivec(CF, 'N', size(N))

  write(*,*) N

  y = PTread_dvec(CF, 'y', size(y))

  write(*,*) y

  is_att = PTread_lvec(CF,'is_att',size(is_att))

  write(*,*) is_att

  call PTkill(CF)

end program test_PT
