program test_PT
  use ParseText
  implicit none

  type(PTo) :: CF

  double precision :: k
  logical :: success

  call PTparse(CF,'sample_file',5)

  success = .true.

  k = PTread_d(CF,'k', 3.d0)
  if (k .ne. 3.d0) then
     success = .false.
     write(*,*) 'failure for k'
  end if

  call PTkill(CF)

  if(success) write(*,*) 'success'

end program test_PT
