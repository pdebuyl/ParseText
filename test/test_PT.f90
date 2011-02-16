program test_PT
  use ParseText
  implicit none

  type(PTo) :: CF

  double precision :: x,y(5), y_known(5)
  integer :: N(3), N_known(3)
  logical :: is_att(4)
  logical :: success
  integer :: i

  open(11,file='sample_file')

  write(11,*) 'x = 1.d0'
  write(11,*) '#N = 93 0 0'
  write(11,*) 'N = -5 2 10'
  N_known = (/ -5 , 2, 10 /)
  write(11,*) 'y = 1.d0 -100.d0 10d5 39. 0.'
  y_known = (/ 1.d0,  -100.d0,  10d5,  39.d0,  0.d0 /)
  write(11,*) 'is_att = F F T T'

  close(11)

  call PTparse(CF,'sample_file',5)

  success = .true.

  x = PTread_d(CF,'x')
  if (x.ne.1.d0) then
     success = .false.
     write(*,*) 'failure for x'
  end if

  N = PTread_ivec(CF, 'N', size(N))

  do i=1,size(N)
     if (N(i).ne.N_known(i)) then
        success = .false.
        write(*,*) 'failure for N', i
     end if
  end do

  y = PTread_dvec(CF, 'y', size(y))

  do i=1,size(y)
     if (y(i).ne.y_known(i)) then
        success = .false.
        write(*,*) 'failure for y', i
     end if
  end do

  is_att = PTread_lvec(CF,'is_att',size(is_att))
  do i=1,size(is_att)
     if (is_att(i).neqv.is_att(i)) then
        success = .false.
        write(*,*) 'failure for is_att', i
     end if
  end do

  call PTkill(CF)

  if(success) write(*,*) 'success'

end program test_PT
