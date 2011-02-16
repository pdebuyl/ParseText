program test_PT
  use ParseText
  implicit none

  type(PTo) :: CF

  double precision :: alpha

  open(11,file='example_file')

  write(11,*) 'alpha = 1.d0'

  close(11)

  call PTinfo

  call PTparse(CF,'example_file',5)

  alpha = PTread_d(CF,'alpha')
  write(*,*) 'alpha = ', alpha

  call PTkill(CF)

end program test_PT
