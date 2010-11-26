! Fortran ParseText module by Pierre de Buyl pdebuyl@(nospam).ulb.ac.be
! The following fortran 90 module helps parsing small text files for use
! as input in fortran programs.

! note : fixed length : length of a line -> elements et tempchar
!                     : length of the PTread_s function

module ParseText

  ! a defined type that contains all the info from the input file
  type PTo
     private
     ! the filename
     character(len=128) :: filename
     ! the number of lines in the file, will be computed at parsing
     integer :: nlines
     ! the character used as the equality relation, defaults to '='
     character(len=8) :: equalsign 
     ! the character used as the comment identifier, defaults to '#'
     character(len=8) :: comment
     ! the array containing the lines of the file; the maximum length
     ! of a line is 144 (can be modified); elements will be allocated
     ! at parsing
     character(len=144), allocatable :: elements(:)
  end type PTo

contains

  subroutine PTparse(PTin, filename, fileunit, equalsign, comment)
    type(PTo), intent(out) :: PTin
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: fileunit
    character(len=*), intent(in), optional :: equalsign, comment

    integer :: iostat
    character(len=144) :: tempchar

    PTin%filename = filename
    if (present(fileunit)) then
       write(*,*) ''
    else
       stop 'please give a fileunit to PTparse'
    end if

    if (present(equalsign)) then
       PTin%equalsign = equalsign
    else
       PTin%equalsign = '='
    end if
    if (present(comment)) then
       PTin%comment = comment
    else
       PTin%comment = '#'
    end if
           

    open(fileunit,file=filename)
    PTin%nlines = 0
    count_loop : do
       read(fileunit, '(A)', iostat=iostat) tempchar
       if (iostat.lt.0) exit count_loop
       if (iostat.gt.0) cycle
       
       if (index(tempchar, PTin%comment).ne.1) then
          PTin%nlines = PTin%nlines + 1
       end if
    end do count_loop
    close(fileunit)

    if (PTin%nlines.gt.0) then
       allocate(PTin%elements(PTin%nlines))
    else
       write(*,*) 'filename = ', filename
       stop 'No line to parse in PTparse'
    end if

    i = 1
    open(fileunit,file=filename)
    read_loop : do
       read(fileunit, '(A)', iostat=iostat) tempchar
       if (iostat.lt.0) exit read_loop
       if (iostat.gt.0) cycle
       
       if (index(tempchar, PTin%comment).ne.1) then
          PTin%elements(i) = adjustl(tempchar)
          i = i + 1
       end if
    end do read_loop
    close(fileunit)

  end subroutine PTparse

  subroutine PTprint(PTin)
    type(PTo), intent(in) :: PTin
    
    integer :: i

    if (allocated(PTin%elements)) then
       do i=1, PTin%nlines
          write(*,'(A)') PTin%elements(i)
       end do
    else
       write(*,*) 'nothing to print in PTprint'
    end if
       
  end subroutine PTprint

  subroutine PTkill(PTin)
    type(PTo), intent(inout) :: PTin

    if (allocated(PTin%elements)) then
       deallocate(PTin%elements)
       PTin%nlines = 0
    else
       write(*,*) 'nothing to kill in PTkill'
    end if
  end subroutine PTkill

  double precision function PTread_d(PTin, var_name)
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name

    integer :: i
    character(len=144) :: tempchar
    double precision :: value

    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
          end if
       end if
    end do

    PTread_d = value
  end function PTread_d

  real function PTread_r(PTin, var_name)
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name

    integer :: i
    character(len=144) :: tempchar
    real :: value

    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
          end if
       end if
    end do

    PTread_r = value
  end function PTread_r


  integer function PTread_i(PTin, var_name)
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name

    integer :: i
    character(len=144) :: tempchar
    integer :: value

    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
          end if
       end if
    end do

    PTread_i = value
  end function PTread_i

  character*64 function PTread_s(PTin, var_name)
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name

    integer :: i
    character(len=144) :: tempchar
    character(len=64) :: value

    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
          end if
       end if
    end do

    PTread_s = value
  end function PTread_s

  logical function PTread_l(PTin, var_name)
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name

    integer :: i
    character(len=144) :: tempchar
    logical :: value

    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
          end if
       end if
    end do

    PTread_l = value
  end function PTread_l




end module ParseText



