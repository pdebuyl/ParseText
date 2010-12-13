program test_PT
  use ParseText
  implicit none

  type(PTo) :: CF

  double precision :: x

  call PTparse(CF,'sample_file',5)

  x = PTread_d(CF,'x')

  write(*,*) x

  call PTkill(CF)

end program test_PT
