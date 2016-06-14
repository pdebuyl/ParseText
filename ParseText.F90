!
! This file is part of ParseText
! Copyright 2011-2015 Pierre de Buyl
! License: BSD-3-clause
!
! The following fortran 90 module helps parsing small text files for use
! as input in fortran programs.
!
! note : fixed length : length of a line -> elements et tempchar
!                     : length of the PTread_s function

module ParseText
  use iso_c_binding
#ifdef PT_HDF5
  use hdf5
#endif
  implicit none

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

#ifdef PT_HDF5
  interface hdf5_util_write_dataset
     module procedure hdf5_util_write_dataset_ds
     module procedure hdf5_util_write_dataset_d1
     module procedure hdf5_util_write_dataset_d2
     module procedure hdf5_util_write_dataset_is
     module procedure hdf5_util_write_dataset_i1
     module procedure hdf5_util_write_dataset_i2
     module procedure hdf5_util_write_dataset_rs
     module procedure hdf5_util_write_dataset_r1
     module procedure hdf5_util_write_dataset_cs
     module procedure hdf5_util_write_dataset_c_int64_ts
  end interface hdf5_util_write_dataset
#endif

contains

  subroutine PTinfo(short)
    logical, intent(in), optional :: short
    logical :: short_var
    include 'PT_version.h'

    if (present(short)) then
       short_var = .true.
    else
       short_var = .false.
    end if

    if (short_var) then
       write(*,*) 'ParseText> Version/date : ', trim(adjustl(ParseText_version_date))
    else
       write(*,*) 'ParseText> Library ParseText, to use human readable config files'
       write(*,*) 'ParseText> (C) 2011 P. de Buyl.'
       write(*,*) 'ParseText> Version/date : ', trim(adjustl(ParseText_version_date))
       write(*,*) 'ParseText> Git commit   : ', trim(ParseText_commit)
       write(*,*) 'ParseText> Built on     : ', trim(ParseText_machine)
       write(*,*) 'ParseText> Compiler     : ', trim(ParseText_compiler)
    end if

  end subroutine PTinfo

  subroutine PTparse(PTin, filename, fileunit, equalsign, comment)
    type(PTo), intent(out) :: PTin
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: fileunit
    character(len=*), intent(in), optional :: equalsign, comment

    integer :: i
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

#ifdef PT_HDF5
  double precision function PTread_d(PTin, var_name, default, loc)
#else
  double precision function PTread_d(PTin, var_name, default)
#endif
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name
    double precision, intent(in), optional :: default
#ifdef PT_HDF5
    integer(HID_T), intent(inout), optional :: loc
#endif

    integer :: i
    character(len=144) :: tempchar
    double precision :: value
    logical :: found

    found = .false.
    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
             found = .true.
             exit
          end if
       end if
    end do

    if (found) then
       PTread_d = value
    else
       if (present(default)) then
          PTread_d = default
       else
          write(*,*) 'variable ',var_name,' not found in the file ',PTin%filename
          stop
       end if
    end if

#ifdef PT_HDF5
    if (present(loc)) then
       call hdf5_util_write_dataset(loc, var_name, PTread_d)
    end if
#endif

  end function PTread_d

#ifdef PT_HDF5
  real function PTread_r(PTin, var_name, loc)
#else
  real function PTread_r(PTin, var_name)
#endif
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name
#ifdef PT_HDF5
    integer(HID_T), intent(inout), optional :: loc
#endif

    integer :: i
    character(len=144) :: tempchar
    real :: value
    logical :: found

    found = .false.
    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
             found = .true.
             exit
          end if
       end if
    end do

    if (found) then
    PTread_r = value
    else
       write(*,*) 'variable ',var_name,' not found in the file ',PTin%filename
       stop
    end if

#ifdef PT_HDF5
    if (present(loc)) then
       call hdf5_util_write_dataset(loc, var_name, PTread_r)
    end if
#endif

  end function PTread_r

#ifdef PT_HDF5
  integer function PTread_i(PTin, var_name, default, loc)
#else
  integer function PTread_i(PTin, var_name, default)
#endif
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name
    integer, intent(in), optional :: default
#ifdef PT_HDF5
    integer(HID_T), intent(inout), optional :: loc
#endif

    integer :: i
    character(len=144) :: tempchar
    integer :: value
    logical :: found

    found = .false.
    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
             found = .true.
             exit
          end if
       end if
    end do

    if (found) then
       PTread_i = value
    else
       if (present(default)) then
          PTread_i = default
       else
          write(*,*) 'variable ',var_name,' not found in the file ',PTin%filename
          stop
       end if
    end if

#ifdef PT_HDF5
    if (present(loc)) then
       call hdf5_util_write_dataset(loc, var_name, PTread_i)
    end if
#endif

  end function PTread_i

  integer(c_int64_t) function PTread_c_int64(PTin, var_name, default) result(r)
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name
    integer, intent(in), optional :: default

    integer :: i
    character(len=144) :: tempchar
    integer(c_int64_t) :: value
    logical :: found

    found = .false.
    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
             found = .true.
             exit
          end if
       end if
    end do

    if (found) then
       r = value
    else
       if (present(default)) then
          r = default
       else
          write(*,*) 'variable ',var_name,' not found in the file ',PTin%filename
          stop
       end if
    end if

  end function PTread_c_int64

#ifdef PT_HDF5
  character(len=144) function PTread_s(PTin, var_name, loc)
#else
  character(len=144) function PTread_s(PTin, var_name)
#endif
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name
#ifdef PT_HDF5
    integer(HID_T), intent(inout), optional :: loc
#endif

    integer :: i
    character(len=144) :: tempchar
    character(len=144) :: value
    logical :: found

    found = .false.
    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
             found = .true.
             exit
          end if
       end if
    end do

    if (found) then
    PTread_s = value
    else
       write(*,*) 'variable ',var_name,' not found in the file ',PTin%filename
       stop
    end if

#ifdef PT_HDF5
    if (present(loc)) then
       call hdf5_util_write_dataset(loc, var_name, PTread_s)
    end if
#endif

  end function PTread_s

#ifdef PT_HDF5
  logical function PTread_l(PTin, var_name, loc)
#else
  logical function PTread_l(PTin, var_name)
#endif
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name
#ifdef PT_HDF5
    integer(HID_T), intent(inout), optional :: loc
#endif

    integer :: i
    character(len=144) :: tempchar
    logical :: value
    logical :: found

    found = .false.
    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
             found = .true.
             exit
          end if
       end if
    end do

    if (found) then
    PTread_l = value
    else
       write(*,*) 'variable ',var_name,' not found in the file ',PTin%filename
       stop
    end if

#ifdef PT_HDF5
    if (present(loc)) then
       if (PTread_l) then
          call hdf5_util_write_dataset(loc, var_name, 1)
       else
          call hdf5_util_write_dataset(loc, var_name, 0)
       end if
    end if
#endif
  end function PTread_l

#ifdef PT_HDF5
  integer function PTread_ivec(PTin, var_name, n, loc)
#else
  integer function PTread_ivec(PTin, var_name, n)
#endif
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name
    integer, intent(in) :: n
    dimension :: PTread_ivec(n)
#ifdef PT_HDF5
    integer(HID_T), intent(inout), optional :: loc
#endif

    integer :: i
    character(len=144) :: tempchar
    integer, dimension(n) :: value
    logical :: found

    found = .false.
    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
             found = .true.
             exit
          end if
       end if
    end do

    if (found) then
    PTread_ivec = value
    else
       write(*,*) 'variable ',var_name,' not found in the file ',PTin%filename
       stop
    end if

#ifdef PT_HDF5
    if (present(loc)) then
       call hdf5_util_write_dataset(loc, var_name, PTread_ivec)
    end if
#endif
  end function PTread_ivec

#ifdef PT_HDF5
  double precision function PTread_dvec(PTin, var_name, n, loc)
#else
  double precision function PTread_dvec(PTin, var_name, n)
#endif
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name
    integer, intent(in) :: n
    dimension :: PTread_dvec(n)
#ifdef PT_HDF5
    integer(HID_T), intent(inout), optional :: loc
#endif

    integer :: i
    character(len=144) :: tempchar
    double precision, dimension(n) :: value
    logical :: found

    found = .false.
    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
             found = .true.
             exit
          end if
       end if
    end do

    if (found) then
    PTread_dvec = value
    else
       write(*,*) 'variable ',var_name,' not found in the file ',PTin%filename
       stop
    end if

#ifdef PT_HDF5
    if (present(loc)) then
       call hdf5_util_write_dataset(loc, var_name, PTread_dvec)
    end if
#endif
  end function PTread_dvec

  logical function PTread_lvec(PTin, var_name, n)
    type(PTo), intent(in) :: PTin
    character(len=*), intent(in) :: var_name
    integer, intent(in) :: n
    dimension :: PTread_lvec(n)

    integer :: i
    character(len=144) :: tempchar
    logical, dimension(n) :: value
    logical :: found

    found = .false.
    do i=1,PTin%nlines
       if (index(PTin%elements(i),var_name).eq.1) then
          tempchar = PTin%elements(i)(len(trim(var_name))+1:)
          tempchar = adjustl(tempchar)
          if (index(tempchar,trim(PTin%equalsign)).eq.1) then
             tempchar = adjustl(tempchar)
             tempchar = tempchar(len(trim(PTin%equalsign))+1:)
             read(tempchar,*) value
             found = .true.
             exit
          end if
       end if
    end do

    if (found) then
    PTread_lvec = value
    else
       write(*,*) 'variable ',var_name,' not found in the file ',PTin%filename
       stop
    end if

  end function PTread_lvec

#ifdef PT_HDF5
  subroutine hdf5_util_write_dataset_ds(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: value

    integer(HID_T) :: d, s, t
    integer :: error
    integer(HSIZE_T) :: dims(1)

    t = H5T_NATIVE_DOUBLE
    dims(1) = 1

    call h5screate_f(H5S_SCALAR_F, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine hdf5_util_write_dataset_ds

  subroutine hdf5_util_write_dataset_d1(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: value(:)

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=1
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_DOUBLE
    dims = shape(value)

    call h5screate_simple_f(rank, dims, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine hdf5_util_write_dataset_d1

  subroutine hdf5_util_write_dataset_d2(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: value(:,:)

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=2
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_DOUBLE
    dims = shape(value)

    call h5screate_simple_f(rank, dims, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine hdf5_util_write_dataset_d2

  subroutine hdf5_util_write_dataset_is(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: value

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=1
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_INTEGER
    dims(1) = 1

    call h5screate_f(H5S_SCALAR_F, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine hdf5_util_write_dataset_is

  subroutine hdf5_util_write_dataset_i1(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: value(:)

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=1
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_INTEGER
    dims = shape(value)

    call h5screate_simple_f(rank, dims, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine hdf5_util_write_dataset_i1

  subroutine hdf5_util_write_dataset_i2(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: value(:,:)

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=2
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_INTEGER
    dims = shape(value)

    call h5screate_simple_f(rank, dims, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine hdf5_util_write_dataset_i2

  subroutine hdf5_util_write_dataset_rs(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    real, intent(in) :: value

    integer(HID_T) :: d, s, t
    integer :: error
    integer(HSIZE_T) :: dims(1)

    t = H5T_NATIVE_REAL
    dims(1) = 1

    call h5screate_f(H5S_SCALAR_F, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine hdf5_util_write_dataset_rs

  subroutine hdf5_util_write_dataset_r1(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    real, intent(in) :: value(:)

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=1
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_REAL
    dims = shape(value)

    call h5screate_simple_f(rank, dims, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine hdf5_util_write_dataset_r1

  subroutine hdf5_util_write_dataset_cs(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    integer(HID_T) :: d, s, t
    integer :: error
    integer(HSIZE_T) :: dims(1)
    integer(SIZE_T) :: length

    length = len(value)
    call h5tcopy_f(H5T_NATIVE_CHARACTER, t, error)
    call h5tset_size_f(t, length, error)
    call h5tset_strpad_f(t, H5T_STR_NULLTERM_F, error)
    dims(1) = 1

    call h5screate_f(H5S_SCALAR_F, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5tclose_f(t, error)
    call h5sclose_f(s, error)

  end subroutine hdf5_util_write_dataset_cs

  subroutine hdf5_util_write_dataset_c_int64_ts(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer(c_int64_t), intent(in) :: value

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=1
    integer(HSIZE_T) :: dims(rank)
    type(c_ptr) :: f_ptr
    integer(c_int64_t), target :: tmp

    t = h5kind_to_type(c_int64_t,H5_INTEGER_KIND)
    dims(1) = 1

    call h5screate_f(H5S_SCALAR_F, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    tmp = value
    f_ptr = c_loc(tmp)
    call h5dwrite_f(d, t, f_ptr, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine hdf5_util_write_dataset_c_int64_ts
#endif

end module ParseText

