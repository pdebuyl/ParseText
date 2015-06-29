program test_PT
  use ParseText
  implicit none

  type(PTo) :: CF

  double precision :: alpha

  integer :: i

  open(11,file='example_file')

  write(11,*) 'alpha = 1.d0'

  close(11)

  call PTinfo

  call PTparse(CF,'example_file',5)

  write(*,*) 'default value for i used'
  alpha = PTread_d(CF,'alpha')
  write(*,*) 'alpha = ', alpha

  i = PTread_i(CF, 'i', -1)
  write(*,*) 'i     = ', i

  call PTkill(CF)

  open(11,file='example_file')
  write(11,*) 'alpha = 1.d0'
  write(11,*) 'i     = 5'
  close(11)

  call PTparse(CF,'example_file',5)

  write(*,*) 'default value for i not used'
  alpha = PTread_d(CF,'alpha')
  write(*,*) 'alpha = ', alpha

  i = PTread_i(CF, 'i', -1)
  write(*,*) 'i     = ', i

  call PTkill(CF)

end program test_PT
