program test_PT
  use ParseText
  implicit none

  type(PTo) :: CF

  double precision :: x,y(5)
  integer :: N(3)
  call PTparse(CF,'sample_file',5)

  x = PTread_d(CF,'x')

  write(*,*) x

  N = PTread_ivec(CF, 'N', size(N))

  write(*,*) N

  y = PTread_dvec(CF, 'y', size(y))

  write(*,*) y

  call PTkill(CF)

end program test_PT
