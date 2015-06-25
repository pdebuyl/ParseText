program test_PT
  use ParseText
  implicit none

  type(PTo) :: CF

  double precision :: x
  logical :: success

  call PTparse(CF,'sample_file',5)

  success = .true.

  x = PTread_d(CF,'x')
  if (x.ne.2.d0) then
     success = .false.
     write(*,*) 'failure for x'
  end if

  call PTkill(CF)

  if(success) write(*,*) 'success'

end program test_PT
