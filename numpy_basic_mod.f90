module numpy_basic_mod
implicit none

integer, parameter :: dp = kind(1.0d0)
integer, parameter :: i8 = selected_int_kind(18)
real(kind=dp), parameter :: twopi = 8.0_dp * atan(1.0_dp)
real(kind=dp), parameter :: twop53 = 9007199254740992.0_dp

interface
   subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      import :: dp
      integer, intent(in) :: n, nrhs, lda, ldb
      integer, intent(out) :: info
      integer, intent(out) :: ipiv(*)
      real(kind=dp), intent(inout) :: a(lda,*), b(ldb,*)
   end subroutine dgesv
end interface

type :: rng_state
   integer(kind=i8) :: s = 1_i8
end type rng_state

!=========================== generic interfaces ============================

interface array
   module procedure array_real
   module procedure array_int
   module procedure array_real2
   module procedure array_int2
end interface array

interface asarray
   module procedure asarray_real
   module procedure asarray_int
   module procedure asarray_real2
   module procedure asarray_int2
end interface asarray

interface zeros
   module procedure zeros_1d_real
   module procedure zeros_1d_int
   module procedure zeros_2d_real
   module procedure zeros_2d_int
end interface zeros

interface ones
   module procedure ones_1d_real
   module procedure ones_1d_int
   module procedure ones_2d_real
   module procedure ones_2d_int
end interface ones

interface empty
   module procedure empty_1d_real
   module procedure empty_1d_int
   module procedure empty_2d_real
   module procedure empty_2d_int
end interface empty

interface full
   module procedure full_1d_real
   module procedure full_1d_int
   module procedure full_2d_real
   module procedure full_2d_int
end interface full

interface arange
   module procedure arange_stop_int
   module procedure arange_start_stop_step_int
   module procedure arange_stop_real
   module procedure arange_start_stop_step_real
end interface arange

interface linspace
   module procedure linspace_real
end interface linspace

interface eye
   module procedure eye_real
   module procedure eye_int
end interface eye

interface identity
   module procedure identity_real
   module procedure identity_int
end interface identity

interface diag
   module procedure diag_vec_to_mat_real
   module procedure diag_vec_to_mat_int
   module procedure diag_mat_to_vec_real
   module procedure diag_mat_to_vec_int
end interface diag

interface tile
   module procedure tile_1d_real
   module procedure tile_1d_int
end interface tile

interface repeat
   module procedure repeat_1d_real
   module procedure repeat_1d_int
end interface repeat

interface reshape_1d_to_2d
   module procedure reshape_1d_to_2d_real
   module procedure reshape_1d_to_2d_int
end interface reshape_1d_to_2d

interface reshape_c_1d_to_2d
   module procedure reshape_c_1d_to_2d_real
   module procedure reshape_c_1d_to_2d_int
end interface reshape_c_1d_to_2d

interface ravel_f
   module procedure ravel_2d_f_real
   module procedure ravel_2d_f_int
end interface ravel_f

interface ravel_c
   module procedure ravel_2d_c_real
   module procedure ravel_2d_c_int
end interface ravel_c

interface flatten
   module procedure flatten_2d_real
   module procedure flatten_2d_int
end interface flatten

interface squeeze_2d_to_1d
   module procedure squeeze_2d_to_1d_real
   module procedure squeeze_2d_to_1d_int
end interface squeeze_2d_to_1d

interface squeeze_4d_to_2d
   module procedure squeeze_4d_to_2d_real
   module procedure squeeze_4d_to_2d_int
end interface squeeze_4d_to_2d

interface expand_dims_axis0
   module procedure expand_dims_axis0_real
   module procedure expand_dims_axis0_int
end interface expand_dims_axis0

interface expand_dims_axis1
   module procedure expand_dims_axis1_real
   module procedure expand_dims_axis1_int
end interface expand_dims_axis1

interface transpose2
   module procedure transpose_real
   module procedure transpose_int
end interface transpose2

interface swapaxes3
   module procedure swapaxes_3d_real
   module procedure swapaxes_3d_int
end interface swapaxes3

interface concatenate
   module procedure concatenate_1d_real
   module procedure concatenate_1d_int
end interface concatenate

interface stack2
   module procedure stack_1d_real
   module procedure stack_1d_int
end interface stack2

interface vstack
   module procedure vstack_2d_real
   module procedure vstack_2d_int
end interface vstack

interface hstack
   module procedure hstack_2d_real
   module procedure hstack_2d_int
end interface hstack

interface split_equal
   module procedure split_1d_equal_real
   module procedure split_1d_equal_int
end interface split_equal

interface vsplit_equal
   module procedure vsplit_2d_equal_real
   module procedure vsplit_2d_equal_int
end interface vsplit_equal

interface hsplit_equal
   module procedure hsplit_2d_equal_real
   module procedure hsplit_2d_equal_int
end interface hsplit_equal

interface where1
   module procedure where_1d_real
   module procedure where_1d_int
end interface where1

interface nonzero1
   module procedure nonzero_1d_real
   module procedure nonzero_1d_int
end interface nonzero1

interface take1
   module procedure take_1d_real
   module procedure take_1d_int
end interface take1

interface clip1
   module procedure clip_1d_real
   module procedure clip_1d_int
end interface clip1

interface sum1
   module procedure sum_real
   module procedure sum_int
end interface sum1

interface mean1
   module procedure mean_real
   module procedure mean_int
end interface mean1

interface std1
   module procedure std_real
   module procedure std_int
end interface std1

interface var1
   module procedure var_real
   module procedure var_int
end interface var1

interface min1
   module procedure min_real
   module procedure min_int
end interface min1

interface max1
   module procedure max_real
   module procedure max_int
end interface max1

interface argmin1
   module procedure argmin_real
   module procedure argmin_int
end interface argmin1

interface argmax1
   module procedure argmax_real
   module procedure argmax_int
end interface argmax1

interface cumsum1
   module procedure cumsum_real
   module procedure cumsum_int
end interface cumsum1

interface cumprod1
   module procedure cumprod_real
   module procedure cumprod_int
end interface cumprod1

interface prod1
   module procedure prod_real
   module procedure prod_int
end interface prod1

interface round1
   module procedure round_1d_real
end interface round1

interface floor1
   module procedure floor_1d_real
end interface floor1

interface ceil1
   module procedure ceil_1d_real
end interface ceil1

interface abs1
   module procedure abs_1d_real
   module procedure abs_1d_int
end interface abs1

interface sqrt1
   module procedure sqrt_1d_real
end interface sqrt1

interface exp1
   module procedure exp_1d_real
end interface exp1

interface log1
   module procedure log_1d_real
end interface log1

interface sin1
   module procedure sin_1d_real
end interface sin1

interface cos1
   module procedure cos_1d_real
end interface cos1

interface tan1
   module procedure tan_1d_real
end interface tan1

interface sort1
   module procedure sort_real
   module procedure sort_int
end interface sort1

interface argsort1
   module procedure argsort_real
   module procedure argsort_int
end interface argsort1

interface unique1
   module procedure unique_real
   module procedure unique_int
end interface unique1

interface dot1
   module procedure dot_real
   module procedure dot_int
end interface dot1

interface matmul2
   module procedure matmul_real
   module procedure matmul_int
end interface matmul2

interface linalg_solve
   module procedure linalg_solve_real
   module procedure linalg_solve_int
end interface linalg_solve

interface linalg_inv
   module procedure linalg_inv_real
   module procedure linalg_inv_int
end interface linalg_inv

interface choice
   module procedure rng_choice_int
   module procedure rng_choice_real
end interface choice

interface permutation
   module procedure rng_permutation_int
   module procedure rng_permutation_real
end interface permutation


contains

!=========================== array/asarray (copy) ============================

subroutine array_real(xin, xout)
real(kind=dp), intent(in) :: xin(:)
real(kind=dp), allocatable, intent(out) :: xout(:)
allocate(xout(size(xin)))
xout = xin
end subroutine array_real

subroutine array_int(xin, xout)
integer, intent(in) :: xin(:)
integer, allocatable, intent(out) :: xout(:)
allocate(xout(size(xin)))
xout = xin
end subroutine array_int

subroutine array_real2(xin, xout)
real(kind=dp), intent(in) :: xin(:,:)
real(kind=dp), allocatable, intent(out) :: xout(:,:)
allocate(xout(size(xin,1), size(xin,2)))
xout = xin
end subroutine array_real2

subroutine array_int2(xin, xout)
integer, intent(in) :: xin(:,:)
integer, allocatable, intent(out) :: xout(:,:)
allocate(xout(size(xin,1), size(xin,2)))
xout = xin
end subroutine array_int2


subroutine asarray_real(xin, xout)
real(kind=dp), intent(in) :: xin(:)
real(kind=dp), allocatable, intent(out) :: xout(:)
call array_real(xin, xout)
end subroutine asarray_real

subroutine asarray_int(xin, xout)
integer, intent(in) :: xin(:)
integer, allocatable, intent(out) :: xout(:)
call array_int(xin, xout)
end subroutine asarray_int

subroutine asarray_real2(xin, xout)
real(kind=dp), intent(in) :: xin(:,:)
real(kind=dp), allocatable, intent(out) :: xout(:,:)
call array_real2(xin, xout)
end subroutine asarray_real2

subroutine asarray_int2(xin, xout)
integer, intent(in) :: xin(:,:)
integer, allocatable, intent(out) :: xout(:,:)
call array_int2(xin, xout)
end subroutine asarray_int2


!=========================== creation: zeros/ones/empty/full ============================

subroutine zeros_1d_real(n, x)
integer, intent(in) :: n
real(kind=dp), allocatable, intent(out) :: x(:)
allocate(x(n))
x = 0.0_dp
end subroutine zeros_1d_real

subroutine zeros_1d_int(n, x)
integer, intent(in) :: n
integer, allocatable, intent(out) :: x(:)
allocate(x(n))
x = 0
end subroutine zeros_1d_int

subroutine zeros_2d_real(n1, n2, x)
integer, intent(in) :: n1, n2
real(kind=dp), allocatable, intent(out) :: x(:,:)
allocate(x(n1,n2))
x = 0.0_dp
end subroutine zeros_2d_real

subroutine zeros_2d_int(n1, n2, x)
integer, intent(in) :: n1, n2
integer, allocatable, intent(out) :: x(:,:)
allocate(x(n1,n2))
x = 0
end subroutine zeros_2d_int

subroutine ones_1d_real(n, x)
integer, intent(in) :: n
real(kind=dp), allocatable, intent(out) :: x(:)
allocate(x(n))
x = 1.0_dp
end subroutine ones_1d_real

subroutine ones_1d_int(n, x)
integer, intent(in) :: n
integer, allocatable, intent(out) :: x(:)
allocate(x(n))
x = 1
end subroutine ones_1d_int

subroutine ones_2d_real(n1, n2, x)
integer, intent(in) :: n1, n2
real(kind=dp), allocatable, intent(out) :: x(:,:)
allocate(x(n1,n2))
x = 1.0_dp
end subroutine ones_2d_real

subroutine ones_2d_int(n1, n2, x)
integer, intent(in) :: n1, n2
integer, allocatable, intent(out) :: x(:,:)
allocate(x(n1,n2))
x = 1
end subroutine ones_2d_int

subroutine full_1d_real(n, val, x)
integer, intent(in) :: n
real(kind=dp), intent(in) :: val
real(kind=dp), allocatable, intent(out) :: x(:)
allocate(x(n))
x = val
end subroutine full_1d_real

subroutine full_1d_int(n, val, x)
integer, intent(in) :: n
integer, intent(in) :: val
integer, allocatable, intent(out) :: x(:)
allocate(x(n))
x = val
end subroutine full_1d_int

subroutine full_2d_real(n1, n2, val, x)
integer, intent(in) :: n1, n2
real(kind=dp), intent(in) :: val
real(kind=dp), allocatable, intent(out) :: x(:,:)
allocate(x(n1,n2))
x = val
end subroutine full_2d_real

subroutine full_2d_int(n1, n2, val, x)
integer, intent(in) :: n1, n2
integer, intent(in) :: val
integer, allocatable, intent(out) :: x(:,:)
allocate(x(n1,n2))
x = val
end subroutine full_2d_int

subroutine empty_1d_real(n, x)
integer, intent(in) :: n
real(kind=dp), allocatable, intent(out) :: x(:)
allocate(x(n))
end subroutine empty_1d_real

subroutine empty_1d_int(n, x)
integer, intent(in) :: n
integer, allocatable, intent(out) :: x(:)
allocate(x(n))
end subroutine empty_1d_int

subroutine empty_2d_real(n1, n2, x)
integer, intent(in) :: n1, n2
real(kind=dp), allocatable, intent(out) :: x(:,:)
allocate(x(n1,n2))
end subroutine empty_2d_real

subroutine empty_2d_int(n1, n2, x)
integer, intent(in) :: n1, n2
integer, allocatable, intent(out) :: x(:,:)
allocate(x(n1,n2))
end subroutine empty_2d_int

!=========================== arange/linspace ============================

subroutine arange_stop_int(stop, x)
integer, intent(in) :: stop
integer, allocatable, intent(out) :: x(:)
integer :: i
allocate(x(stop))
do i = 1, stop
   x(i) = i - 1
end do
end subroutine arange_stop_int

subroutine arange_start_stop_step_int(start, stop, step, x)
integer, intent(in) :: start, stop, step
integer, allocatable, intent(out) :: x(:)
integer :: n, i, v
if (step == 0) then
   allocate(x(0))
   return
end if
n = 0
v = start
if (step > 0) then
   do while (v < stop)
      n = n + 1
      v = v + step
   end do
else
   do while (v > stop)
      n = n + 1
      v = v + step
   end do
end if
allocate(x(n))
v = start
do i = 1, n
   x(i) = v
   v = v + step
end do
end subroutine arange_start_stop_step_int

subroutine arange_stop_real(stop, x)
integer, intent(in) :: stop
real(kind=dp), allocatable, intent(out) :: x(:)
integer :: i
allocate(x(stop))
do i = 1, stop
   x(i) = real(i - 1, kind=dp)
end do
end subroutine arange_stop_real

subroutine arange_start_stop_step_real(start, stop, step, x)
real(kind=dp), intent(in) :: start, stop, step
real(kind=dp), allocatable, intent(out) :: x(:)
integer :: n, i
real(kind=dp) :: v
if (step == 0.0_dp) then
   allocate(x(0))
   return
end if
n = 0
v = start
if (step > 0.0_dp) then
   do while (v < stop)
      n = n + 1
      v = v + step
   end do
else
   do while (v > stop)
      n = n + 1
      v = v + step
   end do
end if
allocate(x(n))
v = start
do i = 1, n
   x(i) = v
   v = v + step
end do
end subroutine arange_start_stop_step_real

subroutine linspace_real(start, stop, num, x)
real(kind=dp), intent(in) :: start, stop
integer, intent(in) :: num
real(kind=dp), allocatable, intent(out) :: x(:)
integer :: i
real(kind=dp) :: step
if (num <= 0) then
   allocate(x(0))
   return
end if
allocate(x(num))
if (num == 1) then
   x(1) = start
   return
end if
step = (stop - start) / real(num - 1, kind=dp)
do i = 1, num
   x(i) = start + real(i - 1, kind=dp) * step
end do
end subroutine linspace_real

!=========================== eye/identity/diag ============================

subroutine eye_real(n, m, a)
integer, intent(in) :: n, m
real(kind=dp), allocatable, intent(out) :: a(:,:)
integer :: i, k
allocate(a(n,m))
a = 0.0_dp
k = min(n,m)
do i = 1, k
   a(i,i) = 1.0_dp
end do
end subroutine eye_real

subroutine eye_int(n, m, a)
integer, intent(in) :: n, m
integer, allocatable, intent(out) :: a(:,:)
integer :: i, k
allocate(a(n,m))
a = 0
k = min(n,m)
do i = 1, k
   a(i,i) = 1
end do
end subroutine eye_int

subroutine identity_real(n, a)
integer, intent(in) :: n
real(kind=dp), allocatable, intent(out) :: a(:,:)
call eye_real(n, n, a)
end subroutine identity_real

subroutine identity_int(n, a)
integer, intent(in) :: n
integer, allocatable, intent(out) :: a(:,:)
call eye_int(n, n, a)
end subroutine identity_int

subroutine diag_vec_to_mat_real(v, a)
real(kind=dp), intent(in) :: v(:)
real(kind=dp), allocatable, intent(out) :: a(:,:)
integer :: n, i
n = size(v)
allocate(a(n,n))
a = 0.0_dp
do i = 1, n
   a(i,i) = v(i)
end do
end subroutine diag_vec_to_mat_real

subroutine diag_vec_to_mat_int(v, a)
integer, intent(in) :: v(:)
integer, allocatable, intent(out) :: a(:,:)
integer :: n, i
n = size(v)
allocate(a(n,n))
a = 0
do i = 1, n
   a(i,i) = v(i)
end do
end subroutine diag_vec_to_mat_int

subroutine diag_mat_to_vec_real(a, vec)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: vec(:)
integer :: n, i
n = min(size(a,1), size(a,2))
allocate(vec(n))
do i = 1, n
   vec(i) = a(i,i)
end do
end subroutine diag_mat_to_vec_real

subroutine diag_mat_to_vec_int(a, vec)
integer, intent(in) :: a(:,:)
integer, allocatable, intent(out) :: vec(:)
integer :: n, i
n = min(size(a,1), size(a,2))
allocate(vec(n))
do i = 1, n
   vec(i) = a(i,i)
end do
end subroutine diag_mat_to_vec_int

!=========================== tile/repeat ============================

subroutine tile_1d_real(x, reps, y)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: reps
real(kind=dp), allocatable, intent(out) :: y(:)
integer :: n, i
n = size(x)
if (reps <= 0) then
   allocate(y(0))
   return
end if
allocate(y(n*reps))
do i = 1, reps
   y((i-1)*n+1:i*n) = x
end do
end subroutine tile_1d_real

subroutine tile_1d_int(x, reps, y)
integer, intent(in) :: x(:)
integer, intent(in) :: reps
integer, allocatable, intent(out) :: y(:)
integer :: n, i
n = size(x)
if (reps <= 0) then
   allocate(y(0))
   return
end if
allocate(y(n*reps))
do i = 1, reps
   y((i-1)*n+1:i*n) = x
end do
end subroutine tile_1d_int

subroutine repeat_1d_real(x, reps_each, y)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: reps_each
real(kind=dp), allocatable, intent(out) :: y(:)
integer :: n, i
n = size(x)
if (reps_each <= 0) then
   allocate(y(0))
   return
end if
allocate(y(n*reps_each))
do i = 1, n
   y((i-1)*reps_each+1:i*reps_each) = x(i)
end do
end subroutine repeat_1d_real

subroutine repeat_1d_int(x, reps_each, y)
integer, intent(in) :: x(:)
integer, intent(in) :: reps_each
integer, allocatable, intent(out) :: y(:)
integer :: n, i
n = size(x)
if (reps_each <= 0) then
   allocate(y(0))
   return
end if
allocate(y(n*reps_each))
do i = 1, n
   y((i-1)*reps_each+1:i*reps_each) = x(i)
end do
end subroutine repeat_1d_int

!=========================== reshape/ravel/flatten ============================

subroutine reshape_1d_to_2d_real(x, n1, n2, a)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: n1, n2
real(kind=dp), allocatable, intent(out) :: a(:,:)
if (size(x) /= n1*n2) then
   allocate(a(0,0))
   return
end if
allocate(a(n1,n2))
a = reshape(x, [n1,n2])
end subroutine reshape_1d_to_2d_real

subroutine reshape_1d_to_2d_int(x, n1, n2, a)
integer, intent(in) :: x(:)
integer, intent(in) :: n1, n2
integer, allocatable, intent(out) :: a(:,:)
if (size(x) /= n1*n2) then
   allocate(a(0,0))
   return
end if
allocate(a(n1,n2))
a = reshape(x, [n1,n2])
end subroutine reshape_1d_to_2d_int

subroutine reshape_c_1d_to_2d_real(x, n1, n2, a)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: n1, n2
real(kind=dp), allocatable, intent(out) :: a(:,:)
integer :: i, j, k
if (size(x) /= n1*n2) then
   allocate(a(0,0))
   return
end if
allocate(a(n1,n2))
k = 0
do i = 1, n1
   do j = 1, n2
      k = k + 1
      a(i,j) = x(k)
   end do
end do
end subroutine reshape_c_1d_to_2d_real

subroutine reshape_c_1d_to_2d_int(x, n1, n2, a)
integer, intent(in) :: x(:)
integer, intent(in) :: n1, n2
integer, allocatable, intent(out) :: a(:,:)
integer :: i, j, k
if (size(x) /= n1*n2) then
   allocate(a(0,0))
   return
end if
allocate(a(n1,n2))
k = 0
do i = 1, n1
   do j = 1, n2
      k = k + 1
      a(i,j) = x(k)
   end do
end do
end subroutine reshape_c_1d_to_2d_int


subroutine ravel_2d_f_real(a, x)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: x(:)
allocate(x(size(a)))
x = reshape(a, [size(a)])
end subroutine ravel_2d_f_real

subroutine ravel_2d_f_int(a, x)
integer, intent(in) :: a(:,:)
integer, allocatable, intent(out) :: x(:)
allocate(x(size(a)))
x = reshape(a, [size(a)])
end subroutine ravel_2d_f_int

subroutine ravel_2d_c_real(a, x)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: x(:)
integer :: i, j, n1, n2, k
n1 = size(a,1)
n2 = size(a,2)
allocate(x(n1*n2))
k = 0
do i = 1, n1
   do j = 1, n2
      k = k + 1
      x(k) = a(i,j)
   end do
end do
end subroutine ravel_2d_c_real

subroutine ravel_2d_c_int(a, x)
integer, intent(in) :: a(:,:)
integer, allocatable, intent(out) :: x(:)
integer :: i, j, n1, n2, k
n1 = size(a,1)
n2 = size(a,2)
allocate(x(n1*n2))
k = 0
do i = 1, n1
   do j = 1, n2
      k = k + 1
      x(k) = a(i,j)
   end do
end do
end subroutine ravel_2d_c_int

subroutine flatten_2d_real(a, x)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: x(:)
call ravel_2d_f_real(a, x)
end subroutine flatten_2d_real

subroutine flatten_2d_int(a, x)
integer, intent(in) :: a(:,:)
integer, allocatable, intent(out) :: x(:)
call ravel_2d_f_int(a, x)
end subroutine flatten_2d_int

!=========================== squeeze/expand_dims (simple) ============================

subroutine squeeze_2d_to_1d_real(a, x)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: x(:)
integer :: n1, n2
n1 = size(a,1)
n2 = size(a,2)
if (n1 == 1) then
   allocate(x(n2))
   x = a(1,:)
else if (n2 == 1) then
   allocate(x(n1))
   x = a(:,1)
else
   allocate(x(0))
end if
end subroutine squeeze_2d_to_1d_real

subroutine squeeze_2d_to_1d_int(a, x)
integer, intent(in) :: a(:,:)
integer, allocatable, intent(out) :: x(:)
integer :: n1, n2
n1 = size(a,1)
n2 = size(a,2)
if (n1 == 1) then
   allocate(x(n2))
   x = a(1,:)
else if (n2 == 1) then
   allocate(x(n1))
   x = a(:,1)
else
   allocate(x(0))
end if
end subroutine squeeze_2d_to_1d_int

subroutine squeeze_4d_to_2d_real(a, b)
real(kind=dp), intent(in) :: a(:,:,:,:)
real(kind=dp), allocatable, intent(out) :: b(:,:)
integer :: n2, n4
if (size(a,1) /= 1 .or. size(a,3) /= 1) then
   allocate(b(0,0))
   return
end if
n2 = size(a,2)
n4 = size(a,4)
allocate(b(n2,n4))
b = a(1,:,1,:)
end subroutine squeeze_4d_to_2d_real

subroutine squeeze_4d_to_2d_int(a, b)
integer, intent(in) :: a(:,:,:,:)
integer, allocatable, intent(out) :: b(:,:)
integer :: n2, n4
if (size(a,1) /= 1 .or. size(a,3) /= 1) then
   allocate(b(0,0))
   return
end if
n2 = size(a,2)
n4 = size(a,4)
allocate(b(n2,n4))
b = a(1,:,1,:)
end subroutine squeeze_4d_to_2d_int


subroutine expand_dims_axis0_real(x, a)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: a(:,:)
allocate(a(1, size(x)))
a(1,:) = x
end subroutine expand_dims_axis0_real

subroutine expand_dims_axis1_real(x, a)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: a(:,:)
allocate(a(size(x), 1))
a(:,1) = x
end subroutine expand_dims_axis1_real

subroutine expand_dims_axis0_int(x, a)
integer, intent(in) :: x(:)
integer, allocatable, intent(out) :: a(:,:)
allocate(a(1, size(x)))
a(1,:) = x
end subroutine expand_dims_axis0_int

subroutine expand_dims_axis1_int(x, a)
integer, intent(in) :: x(:)
integer, allocatable, intent(out) :: a(:,:)
allocate(a(size(x), 1))
a(:,1) = x
end subroutine expand_dims_axis1_int

!=========================== transpose/swapaxes ============================

subroutine transpose_real(a, b)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: b(:,:)
allocate(b(size(a,2), size(a,1)))
b = transpose(a)
end subroutine transpose_real

subroutine transpose_int(a, b)
integer, intent(in) :: a(:,:)
integer, allocatable, intent(out) :: b(:,:)
allocate(b(size(a,2), size(a,1)))
b = transpose(a)
end subroutine transpose_int

subroutine swapaxes_3d_real(a, ax1, ax2, b)
real(kind=dp), intent(in) :: a(:,:,:)
integer, intent(in) :: ax1, ax2
real(kind=dp), allocatable, intent(out) :: b(:,:,:)
integer :: n1, n2, n3, i, j, k
integer :: s(3)
n1 = size(a,1); n2 = size(a,2); n3 = size(a,3)
s = [n1, n2, n3]
if (ax1 < 1 .or. ax1 > 3 .or. ax2 < 1 .or. ax2 > 3) then
   allocate(b(0,0,0))
   return
end if
call swap_int(s(ax1), s(ax2))
allocate(b(s(1), s(2), s(3)))
do k = 1, n3
   do j = 1, n2
      do i = 1, n1
         b(idx3(i,j,k,ax1,ax2,1), idx3(i,j,k,ax1,ax2,2), idx3(i,j,k,ax1,ax2,3)) = a(i,j,k)
      end do
   end do
end do
end subroutine swapaxes_3d_real

subroutine swapaxes_3d_int(a, ax1, ax2, b)
integer, intent(in) :: a(:,:,:)
integer, intent(in) :: ax1, ax2
integer, allocatable, intent(out) :: b(:,:,:)
integer :: n1, n2, n3, i, j, k
integer :: s(3)
n1 = size(a,1); n2 = size(a,2); n3 = size(a,3)
s = [n1, n2, n3]
if (ax1 < 1 .or. ax1 > 3 .or. ax2 < 1 .or. ax2 > 3) then
   allocate(b(0,0,0))
   return
end if
call swap_int(s(ax1), s(ax2))
allocate(b(s(1), s(2), s(3)))
do k = 1, n3
   do j = 1, n2
      do i = 1, n1
         b(idx3(i,j,k,ax1,ax2,1), idx3(i,j,k,ax1,ax2,2), idx3(i,j,k,ax1,ax2,3)) = a(i,j,k)
      end do
   end do
end do
end subroutine swapaxes_3d_int

subroutine swap_int(a, b)
integer, intent(inout) :: a, b
integer :: t
t = a; a = b; b = t
end subroutine swap_int

integer function idx3(i, j, k, ax1, ax2, which) result(v)
integer, intent(in) :: i, j, k, ax1, ax2, which
integer :: p(3), q(3), t
p = [i, j, k]
q = p
t = q(ax1)
q(ax1) = q(ax2)
q(ax2) = t
v = q(which)
end function idx3

!=========================== concatenate/stack/vstack/hstack ============================

subroutine concatenate_1d_real(a, b, c)
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable, intent(out) :: c(:)
allocate(c(size(a)+size(b)))
c(1:size(a)) = a
c(size(a)+1:) = b
end subroutine concatenate_1d_real

subroutine concatenate_1d_int(a, b, c)
integer, intent(in) :: a(:), b(:)
integer, allocatable, intent(out) :: c(:)
allocate(c(size(a)+size(b)))
c(1:size(a)) = a
c(size(a)+1:) = b
end subroutine concatenate_1d_int

subroutine vstack_2d_real(a, b, c)
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable, intent(out) :: c(:,:)
integer :: n1a, n2a, n1b
n1a = size(a,1); n2a = size(a,2)
n1b = size(b,1)
if (size(b,2) /= n2a) then
   allocate(c(0,0))
   return
end if
allocate(c(n1a+n1b, n2a))
c(1:n1a,:) = a
c(n1a+1:,:) = b
end subroutine vstack_2d_real

subroutine vstack_2d_int(a, b, c)
integer, intent(in) :: a(:,:), b(:,:)
integer, allocatable, intent(out) :: c(:,:)
integer :: n1a, n2a, n1b
n1a = size(a,1); n2a = size(a,2)
n1b = size(b,1)
if (size(b,2) /= n2a) then
   allocate(c(0,0))
   return
end if
allocate(c(n1a+n1b, n2a))
c(1:n1a,:) = a
c(n1a+1:,:) = b
end subroutine vstack_2d_int

subroutine hstack_2d_real(a, b, c)
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable, intent(out) :: c(:,:)
integer :: n1a, n2a, n2b
n1a = size(a,1); n2a = size(a,2)
n2b = size(b,2)
if (size(b,1) /= n1a) then
   allocate(c(0,0))
   return
end if
allocate(c(n1a, n2a+n2b))
c(:,1:n2a) = a
c(:,n2a+1:) = b
end subroutine hstack_2d_real

subroutine hstack_2d_int(a, b, c)
integer, intent(in) :: a(:,:), b(:,:)
integer, allocatable, intent(out) :: c(:,:)
integer :: n1a, n2a, n2b
n1a = size(a,1); n2a = size(a,2)
n2b = size(b,2)
if (size(b,1) /= n1a) then
   allocate(c(0,0))
   return
end if
allocate(c(n1a, n2a+n2b))
c(:,1:n2a) = a
c(:,n2a+1:) = b
end subroutine hstack_2d_int

subroutine stack_1d_real(a, b, c)
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable, intent(out) :: c(:,:)
if (size(a) /= size(b)) then
   allocate(c(0,0))
   return
end if
allocate(c(2, size(a)))
c(1,:) = a
c(2,:) = b
end subroutine stack_1d_real

subroutine stack_1d_int(a, b, c)
integer, intent(in) :: a(:), b(:)
integer, allocatable, intent(out) :: c(:,:)
if (size(a) /= size(b)) then
   allocate(c(0,0))
   return
end if
allocate(c(2, size(a)))
c(1,:) = a
c(2,:) = b
end subroutine stack_1d_int

!=========================== split/vsplit/hsplit (equal parts) ============================

subroutine split_1d_equal_real(x, nparts, out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nparts
real(kind=dp), allocatable, intent(out) :: out(:,:)
integer :: n, m, i
n = size(x)
if (nparts <= 0) then
   allocate(out(0,0))
   return
end if
if (mod(n, nparts) /= 0) then
   allocate(out(0,0))
   return
end if
m = n / nparts
allocate(out(nparts, m))
do i = 1, nparts
   out(i,:) = x((i-1)*m+1:i*m)
end do
end subroutine split_1d_equal_real

subroutine split_1d_equal_int(x, nparts, out)
integer, intent(in) :: x(:)
integer, intent(in) :: nparts
integer, allocatable, intent(out) :: out(:,:)
integer :: n, m, i
n = size(x)
if (nparts <= 0) then
   allocate(out(0,0))
   return
end if
if (mod(n, nparts) /= 0) then
   allocate(out(0,0))
   return
end if
m = n / nparts
allocate(out(nparts, m))
do i = 1, nparts
   out(i,:) = x((i-1)*m+1:i*m)
end do
end subroutine split_1d_equal_int

subroutine vsplit_2d_equal_real(a, nparts, out)
real(kind=dp), intent(in) :: a(:,:)
integer, intent(in) :: nparts
real(kind=dp), allocatable, intent(out) :: out(:,:,:)
integer :: n1, n2, m, i
n1 = size(a,1)
n2 = size(a,2)
if (nparts <= 0) then
   allocate(out(0,0,0))
   return
end if
if (mod(n1, nparts) /= 0) then
   allocate(out(0,0,0))
   return
end if
m = n1 / nparts
allocate(out(nparts, m, n2))
do i = 1, nparts
   out(i,:,:) = a((i-1)*m+1:i*m, :)
end do
end subroutine vsplit_2d_equal_real

subroutine hsplit_2d_equal_real(a, nparts, out)
real(kind=dp), intent(in) :: a(:,:)
integer, intent(in) :: nparts
real(kind=dp), allocatable, intent(out) :: out(:,:,:)
integer :: n1, n2, m, i
n1 = size(a,1)
n2 = size(a,2)
if (nparts <= 0) then
   allocate(out(0,0,0))
   return
end if
if (mod(n2, nparts) /= 0) then
   allocate(out(0,0,0))
   return
end if
m = n2 / nparts
allocate(out(nparts, n1, m))
do i = 1, nparts
   out(i,:,:) = a(:, (i-1)*m+1:i*m)
end do
end subroutine hsplit_2d_equal_real

subroutine vsplit_2d_equal_int(a, nparts, out)
integer, intent(in) :: a(:,:)
integer, intent(in) :: nparts
integer, allocatable, intent(out) :: out(:,:,:)
integer :: n1, n2, m, i
n1 = size(a,1)
n2 = size(a,2)
if (nparts <= 0) then
   allocate(out(0,0,0))
   return
end if
if (mod(n1, nparts) /= 0) then
   allocate(out(0,0,0))
   return
end if
m = n1 / nparts
allocate(out(nparts, m, n2))
do i = 1, nparts
   out(i,:,:) = a((i-1)*m+1:i*m, :)
end do
end subroutine vsplit_2d_equal_int

subroutine hsplit_2d_equal_int(a, nparts, out)
integer, intent(in) :: a(:,:)
integer, intent(in) :: nparts
integer, allocatable, intent(out) :: out(:,:,:)
integer :: n1, n2, m, i
n1 = size(a,1)
n2 = size(a,2)
if (nparts <= 0) then
   allocate(out(0,0,0))
   return
end if
if (mod(n2, nparts) /= 0) then
   allocate(out(0,0,0))
   return
end if
m = n2 / nparts
allocate(out(nparts, n1, m))
do i = 1, nparts
   out(i,:,:) = a(:, (i-1)*m+1:i*m)
end do
end subroutine hsplit_2d_equal_int

!=========================== where/nonzero/take/clip ============================

subroutine where_1d_real(cond, x, y, out)
logical, intent(in) :: cond(:)
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp), allocatable, intent(out) :: out(:)
integer :: n, i
n = size(cond)
allocate(out(n))
do i = 1, n
   if (cond(i)) then
      out(i) = x(i)
   else
      out(i) = y(i)
   end if
end do
end subroutine where_1d_real

subroutine where_1d_int(cond, x, y, out)
logical, intent(in) :: cond(:)
integer, intent(in) :: x(:), y(:)
integer, allocatable, intent(out) :: out(:)
integer :: n, i
n = size(cond)
allocate(out(n))
do i = 1, n
   if (cond(i)) then
      out(i) = x(i)
   else
      out(i) = y(i)
   end if
end do
end subroutine where_1d_int

subroutine nonzero_1d_real(x, idx)
real(kind=dp), intent(in) :: x(:)
integer, allocatable, intent(out) :: idx(:)
integer :: i, cnt
cnt = 0
do i = 1, size(x)
   if (x(i) /= 0.0_dp) cnt = cnt + 1
end do
allocate(idx(cnt))
cnt = 0
do i = 1, size(x)
   if (x(i) /= 0.0_dp) then
      cnt = cnt + 1
      idx(cnt) = i
   end if
end do
end subroutine nonzero_1d_real

subroutine nonzero_1d_int(x, idx)
integer, intent(in) :: x(:)
integer, allocatable, intent(out) :: idx(:)
integer :: i, cnt
cnt = 0
do i = 1, size(x)
   if (x(i) /= 0) cnt = cnt + 1
end do
allocate(idx(cnt))
cnt = 0
do i = 1, size(x)
   if (x(i) /= 0) then
      cnt = cnt + 1
      idx(cnt) = i
   end if
end do
end subroutine nonzero_1d_int

subroutine take_1d_real(x, idx, y)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: idx(:)
real(kind=dp), allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(idx)))
do i = 1, size(idx)
   y(i) = x(idx(i))
end do
end subroutine take_1d_real

subroutine take_1d_int(x, idx, y)
integer, intent(in) :: x(:)
integer, intent(in) :: idx(:)
integer, allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(idx)))
do i = 1, size(idx)
   y(i) = x(idx(i))
end do
end subroutine take_1d_int

subroutine clip_1d_real(x, lo, hi, y)
real(kind=dp), intent(in) :: x(:), lo, hi
real(kind=dp), allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(x)))
do i = 1, size(x)
   y(i) = x(i)
   if (y(i) < lo) y(i) = lo
   if (y(i) > hi) y(i) = hi
end do
end subroutine clip_1d_real

subroutine clip_1d_int(x, lo, hi, y)
integer, intent(in) :: x(:), lo, hi
integer, allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(x)))
do i = 1, size(x)
   y(i) = x(i)
   if (y(i) < lo) y(i) = lo
   if (y(i) > hi) y(i) = hi
end do
end subroutine clip_1d_int

!=========================== reductions and cumulative ============================

subroutine sum_real(x, s)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: s
integer :: i
s = 0.0_dp
do i = 1, size(x)
   s = s + x(i)
end do
end subroutine sum_real

subroutine sum_int(x, s)
integer, intent(in) :: x(:)
integer, intent(out) :: s
integer :: i
s = 0
do i = 1, size(x)
   s = s + x(i)
end do
end subroutine sum_int

subroutine prod_real(x, p)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: p
integer :: i
p = 1.0_dp
do i = 1, size(x)
   p = p * x(i)
end do
end subroutine prod_real

subroutine prod_int(x, p)
integer, intent(in) :: x(:)
integer, intent(out) :: p
integer :: i
p = 1
do i = 1, size(x)
   p = p * x(i)
end do
end subroutine prod_int

subroutine cumsum_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(x)))
if (size(x) == 0) return
y(1) = x(1)
do i = 2, size(x)
   y(i) = y(i-1) + x(i)
end do
end subroutine cumsum_real

subroutine cumsum_int(x, y)
integer, intent(in) :: x(:)
integer, allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(x)))
if (size(x) == 0) return
y(1) = x(1)
do i = 2, size(x)
   y(i) = y(i-1) + x(i)
end do
end subroutine cumsum_int

subroutine cumprod_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(x)))
if (size(x) == 0) return
y(1) = x(1)
do i = 2, size(x)
   y(i) = y(i-1) * x(i)
end do
end subroutine cumprod_real

subroutine cumprod_int(x, y)
integer, intent(in) :: x(:)
integer, allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(x)))
if (size(x) == 0) return
y(1) = x(1)
do i = 2, size(x)
   y(i) = y(i-1) * x(i)
end do
end subroutine cumprod_int

subroutine mean_real(x, m)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: m
real(kind=dp) :: s
call sum_real(x, s)
if (size(x) == 0) then
   m = 0.0_dp
else
   m = s / real(size(x), kind=dp)
end if
end subroutine mean_real

subroutine mean_int(x, m)
integer, intent(in) :: x(:)
real(kind=dp), intent(out) :: m
integer :: s
call sum_int(x, s)
if (size(x) == 0) then
   m = 0.0_dp
else
   m = real(s, kind=dp) / real(size(x), kind=dp)
end if
end subroutine mean_int

subroutine var_real(x, ddof, v)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: ddof
real(kind=dp), intent(out) :: v
real(kind=dp) :: mu, s2, dx
integer :: i, n
n = size(x)
if (n - ddof <= 0) then
   v = 0.0_dp
   return
end if
call mean_real(x, mu)
s2 = 0.0_dp
do i = 1, n
   dx = x(i) - mu
   s2 = s2 + dx*dx
end do
v = s2 / real(n - ddof, kind=dp)
end subroutine var_real

subroutine var_int(x, ddof, v)
integer, intent(in) :: x(:)
integer, intent(in) :: ddof
real(kind=dp), intent(out) :: v
real(kind=dp) :: mu, s2, dx
integer :: i, n
n = size(x)
if (n - ddof <= 0) then
   v = 0.0_dp
   return
end if
call mean_int(x, mu)
s2 = 0.0_dp
do i = 1, n
   dx = real(x(i), kind=dp) - mu
   s2 = s2 + dx*dx
end do
v = s2 / real(n - ddof, kind=dp)
end subroutine var_int

subroutine std_real(x, ddof, s)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: ddof
real(kind=dp), intent(out) :: s
real(kind=dp) :: v
call var_real(x, ddof, v)
s = sqrt(v)
end subroutine std_real

subroutine std_int(x, ddof, s)
integer, intent(in) :: x(:)
integer, intent(in) :: ddof
real(kind=dp), intent(out) :: s
real(kind=dp) :: v
call var_int(x, ddof, v)
s = sqrt(v)
end subroutine std_int

subroutine min_real(x, v)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: v
integer :: i
if (size(x) == 0) then
   v = 0.0_dp
   return
end if
v = x(1)
do i = 2, size(x)
   if (x(i) < v) v = x(i)
end do
end subroutine min_real

subroutine max_real(x, v)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: v
integer :: i
if (size(x) == 0) then
   v = 0.0_dp
   return
end if
v = x(1)
do i = 2, size(x)
   if (x(i) > v) v = x(i)
end do
end subroutine max_real

subroutine min_int(x, v)
integer, intent(in) :: x(:)
integer, intent(out) :: v
integer :: i
if (size(x) == 0) then
   v = 0
   return
end if
v = x(1)
do i = 2, size(x)
   if (x(i) < v) v = x(i)
end do
end subroutine min_int

subroutine max_int(x, v)
integer, intent(in) :: x(:)
integer, intent(out) :: v
integer :: i
if (size(x) == 0) then
   v = 0
   return
end if
v = x(1)
do i = 2, size(x)
   if (x(i) > v) v = x(i)
end do
end subroutine max_int

subroutine argmin_real(x, idx)
real(kind=dp), intent(in) :: x(:)
integer, intent(out) :: idx
integer :: i
if (size(x) == 0) then
   idx = 0
   return
end if
idx = 1
do i = 2, size(x)
   if (x(i) < x(idx)) idx = i
end do
end subroutine argmin_real

subroutine argmax_real(x, idx)
real(kind=dp), intent(in) :: x(:)
integer, intent(out) :: idx
integer :: i
if (size(x) == 0) then
   idx = 0
   return
end if
idx = 1
do i = 2, size(x)
   if (x(i) > x(idx)) idx = i
end do
end subroutine argmax_real

subroutine argmin_int(x, idx)
integer, intent(in) :: x(:)
integer, intent(out) :: idx
integer :: i
if (size(x) == 0) then
   idx = 0
   return
end if
idx = 1
do i = 2, size(x)
   if (x(i) < x(idx)) idx = i
end do
end subroutine argmin_int

subroutine argmax_int(x, idx)
integer, intent(in) :: x(:)
integer, intent(out) :: idx
integer :: i
if (size(x) == 0) then
   idx = 0
   return
end if
idx = 1
do i = 2, size(x)
   if (x(i) > x(idx)) idx = i
end do
end subroutine argmax_int

!=========================== elementwise math ============================

subroutine round_1d_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(x)))
do i = 1, size(x)
   y(i) = real(nint(x(i)), kind=dp)
end do
end subroutine round_1d_real

subroutine floor_1d_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(x)))
do i = 1, size(x)
   y(i) = real(floor(x(i)), kind=dp)
end do
end subroutine floor_1d_real

subroutine ceil_1d_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
integer :: i
allocate(y(size(x)))
do i = 1, size(x)
   y(i) = real(ceiling(x(i)), kind=dp)
end do
end subroutine ceil_1d_real

subroutine abs_1d_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
allocate(y(size(x)))
y = abs(x)
end subroutine abs_1d_real

subroutine abs_1d_int(x, y)
integer, intent(in) :: x(:)
integer, allocatable, intent(out) :: y(:)
allocate(y(size(x)))
y = abs(x)
end subroutine abs_1d_int

subroutine sqrt_1d_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
allocate(y(size(x)))
y = sqrt(x)
end subroutine sqrt_1d_real

subroutine exp_1d_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
allocate(y(size(x)))
y = exp(x)
end subroutine exp_1d_real

subroutine log_1d_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
allocate(y(size(x)))
y = log(x)
end subroutine log_1d_real

subroutine sin_1d_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
allocate(y(size(x)))
y = sin(x)
end subroutine sin_1d_real

subroutine cos_1d_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
allocate(y(size(x)))
y = cos(x)
end subroutine cos_1d_real

subroutine tan_1d_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
allocate(y(size(x)))
y = tan(x)
end subroutine tan_1d_real

!=========================== sort/argsort/unique (stable) ============================

subroutine argsort_int(x, idx)
integer, intent(in) :: x(:)
integer, allocatable, intent(out) :: idx(:)
integer :: n, i
n = size(x)
allocate(idx(n))
do i = 1, n
   idx(i) = i
end do
call mergesort_idx_int(x, idx, 1, n)
end subroutine argsort_int

recursive subroutine mergesort_idx_int(x, idx, l, r)
integer, intent(in) :: x(:)
integer, intent(inout) :: idx(:)
integer, intent(in) :: l, r
integer :: m
if (l >= r) return
m = (l + r) / 2
call mergesort_idx_int(x, idx, l, m)
call mergesort_idx_int(x, idx, m+1, r)
call merge_idx_int(x, idx, l, m, r)
end subroutine mergesort_idx_int

subroutine merge_idx_int(x, idx, l, m, r)
integer, intent(in) :: x(:)
integer, intent(inout) :: idx(:)
integer, intent(in) :: l, m, r
integer, allocatable :: tmp(:)
integer :: i, j, k
allocate(tmp(l:r))
i = l
j = m + 1
k = l
do while (i <= m .and. j <= r)
   if (x(idx(i)) <= x(idx(j))) then
      tmp(k) = idx(i); i = i + 1
   else
      tmp(k) = idx(j); j = j + 1
   end if
   k = k + 1
end do
do while (i <= m)
   tmp(k) = idx(i); i = i + 1; k = k + 1
end do
do while (j <= r)
   tmp(k) = idx(j); j = j + 1; k = k + 1
end do
idx(l:r) = tmp(l:r)
deallocate(tmp)
end subroutine merge_idx_int

subroutine argsort_real(x, idx)
real(kind=dp), intent(in) :: x(:)
integer, allocatable, intent(out) :: idx(:)
integer :: n, i
n = size(x)
allocate(idx(n))
do i = 1, n
   idx(i) = i
end do
call mergesort_idx_real(x, idx, 1, n)
end subroutine argsort_real

recursive subroutine mergesort_idx_real(x, idx, l, r)
real(kind=dp), intent(in) :: x(:)
integer, intent(inout) :: idx(:)
integer, intent(in) :: l, r
integer :: m
if (l >= r) return
m = (l + r) / 2
call mergesort_idx_real(x, idx, l, m)
call mergesort_idx_real(x, idx, m+1, r)
call merge_idx_real(x, idx, l, m, r)
end subroutine mergesort_idx_real

subroutine merge_idx_real(x, idx, l, m, r)
real(kind=dp), intent(in) :: x(:)
integer, intent(inout) :: idx(:)
integer, intent(in) :: l, m, r
integer, allocatable :: tmp(:)
integer :: i, j, k
allocate(tmp(l:r))
i = l
j = m + 1
k = l
do while (i <= m .and. j <= r)
   if (x(idx(i)) <= x(idx(j))) then
      tmp(k) = idx(i); i = i + 1
   else
      tmp(k) = idx(j); j = j + 1
   end if
   k = k + 1
end do
do while (i <= m)
   tmp(k) = idx(i); i = i + 1; k = k + 1
end do
do while (j <= r)
   tmp(k) = idx(j); j = j + 1; k = k + 1
end do
idx(l:r) = tmp(l:r)
deallocate(tmp)
end subroutine merge_idx_real

subroutine sort_real(x, y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: y(:)
integer, allocatable :: idx(:)
call argsort_real(x, idx)
allocate(y(size(x)))
y = x(idx)
deallocate(idx)
end subroutine sort_real

subroutine sort_int(x, y)
integer, intent(in) :: x(:)
integer, allocatable, intent(out) :: y(:)
integer, allocatable :: idx(:)
call argsort_int(x, idx)
allocate(y(size(x)))
y = x(idx)
deallocate(idx)
end subroutine sort_int

subroutine unique_int(x, u)
integer, intent(in) :: x(:)
integer, allocatable, intent(out) :: u(:)
integer, allocatable :: idx(:)
integer :: n, i, cnt
n = size(x)
if (n == 0) then
   allocate(u(0))
   return
end if
call argsort_int(x, idx)
cnt = 1
do i = 2, n
   if (x(idx(i)) /= x(idx(i-1))) cnt = cnt + 1
end do
allocate(u(cnt))
u(1) = x(idx(1))
cnt = 1
do i = 2, n
   if (x(idx(i)) /= x(idx(i-1))) then
      cnt = cnt + 1
      u(cnt) = x(idx(i))
   end if
end do
deallocate(idx)
end subroutine unique_int

subroutine unique_real(x, u)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable, intent(out) :: u(:)
integer, allocatable :: idx(:)
integer :: n, i, cnt
n = size(x)
if (n == 0) then
   allocate(u(0))
   return
end if
call argsort_real(x, idx)
cnt = 1
do i = 2, n
   if (x(idx(i)) /= x(idx(i-1))) cnt = cnt + 1
end do
allocate(u(cnt))
u(1) = x(idx(1))
cnt = 1
do i = 2, n
   if (x(idx(i)) /= x(idx(i-1))) then
      cnt = cnt + 1
      u(cnt) = x(idx(i))
   end if
end do
deallocate(idx)
end subroutine unique_real

!=========================== dot/matmul ============================

subroutine dot_real(x, y, d)
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp), intent(out) :: d
integer :: i
d = 0.0_dp
do i = 1, min(size(x), size(y))
   d = d + x(i)*y(i)
end do
end subroutine dot_real

subroutine dot_int(x, y, d)
integer, intent(in) :: x(:), y(:)
integer, intent(out) :: d
integer :: i
d = 0
do i = 1, min(size(x), size(y))
   d = d + x(i)*y(i)
end do
end subroutine dot_int

subroutine matmul_real(a, b, c)
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable, intent(out) :: c(:,:)
allocate(c(size(a,1), size(b,2)))
c = matmul(a, b)
end subroutine matmul_real

subroutine matmul_int(a, b, c)
integer, intent(in) :: a(:,:), b(:,:)
integer, allocatable, intent(out) :: c(:,:)
allocate(c(size(a,1), size(b,2)))
c = matmul(a, b)
end subroutine matmul_int

!=========================== linalg: solve/inv (lapack dgesv) ============================

subroutine linalg_solve_real(a, b, x, info)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp), allocatable, intent(out) :: x(:)
integer, intent(out) :: info
real(kind=dp), allocatable :: ac(:,:), bc(:,:)
integer, allocatable :: ipiv(:)
integer :: n, lda, ldb, nrhs
n = size(a,1)
if (size(a,2) /= n) then
   info = -1
   allocate(x(0))
   return
end if
if (size(b) /= n) then
   info = -2
   allocate(x(0))
   return
end if
allocate(ac(n,n))
ac = a
nrhs = 1
lda = max(1, n)
ldb = max(1, n)
allocate(bc(ldb,nrhs))
bc(:,1) = b
allocate(ipiv(n))
call dgesv(n, nrhs, ac, lda, ipiv, bc, ldb, info)
allocate(x(n))
x = bc(1:n,1)
deallocate(ac, bc, ipiv)
end subroutine linalg_solve_real

subroutine linalg_solve_int(a, b, x, info)
integer, intent(in) :: a(:,:)
integer, intent(in) :: b(:)
real(kind=dp), allocatable, intent(out) :: x(:)
integer, intent(out) :: info
real(kind=dp), allocatable :: ar(:,:), br(:)
allocate(ar(size(a,1), size(a,2)))
allocate(br(size(b)))
ar = real(a, kind=dp)
br = real(b, kind=dp)
call linalg_solve_real(ar, br, x, info)
deallocate(ar, br)
end subroutine linalg_solve_int

subroutine linalg_inv_real(a, ainv, info)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: ainv(:,:)
integer, intent(out) :: info
real(kind=dp), allocatable :: ac(:,:), b(:,:)
integer, allocatable :: ipiv(:)
integer :: n, lda, ldb, nrhs, i
n = size(a,1)
if (size(a,2) /= n) then
   info = -1
   allocate(ainv(0,0))
   return
end if
allocate(ac(n,n))
ac = a
nrhs = n
lda = max(1, n)
ldb = max(1, n)
allocate(b(ldb,nrhs))
b = 0.0_dp
do i = 1, n
   b(i,i) = 1.0_dp
end do
allocate(ipiv(n))
call dgesv(n, nrhs, ac, lda, ipiv, b, ldb, info)
allocate(ainv(n,n))
ainv = b(1:n,1:n)
deallocate(ac, b, ipiv)
end subroutine linalg_inv_real

subroutine linalg_inv_int(a, ainv, info)
integer, intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: ainv(:,:)
integer, intent(out) :: info
real(kind=dp), allocatable :: ar(:,:)
allocate(ar(size(a,1), size(a,2)))
ar = real(a, kind=dp)
call linalg_inv_real(ar, ainv, info)
deallocate(ar)
end subroutine linalg_inv_int

!=========================== rng: default_rng + methods ============================

subroutine default_rng(seed, rng)
integer(kind=i8), intent(in) :: seed
type(rng_state), intent(out) :: rng
rng%s = splitmix64(seed)
end subroutine default_rng

integer(kind=i8) function splitmix64(x) result(z)
integer(kind=i8), intent(in) :: x
integer(kind=i8) :: y
y = x + int(z'9e3779b97f4a7c15', kind=i8)
z = y
z = ieor(z, ishft(z, -30))
z = z * int(z'bf58476d1ce4e5b9', kind=i8)
z = ieor(z, ishft(z, -27))
z = z * int(z'94d049bb133111eb', kind=i8)
z = ieor(z, ishft(z, -31))
end function splitmix64

integer(kind=i8) function rng_u64(rng) result(u)
type(rng_state), intent(inout) :: rng
integer(kind=i8) :: x
x = rng%s
x = ieor(x, ishft(x, 13))
x = ieor(x, ishft(x, -7))
x = ieor(x, ishft(x, 17))
rng%s = x
u = x
end function rng_u64

subroutine rng_random(rng, n, x)
type(rng_state), intent(inout) :: rng
integer, intent(in) :: n
real(kind=dp), allocatable, intent(out) :: x(:)
integer :: i
integer(kind=i8) :: u
allocate(x(n))
do i = 1, n
   u = rng_u64(rng)
   u = ishft(u, -11)
   x(i) = real(u, kind=dp) / twop53
end do
end subroutine rng_random

subroutine rng_integers(rng, low, high, n, x)
type(rng_state), intent(inout) :: rng
integer, intent(in) :: low, high, n
integer, allocatable, intent(out) :: x(:)
integer :: i, span
integer(kind=i8) :: u
span = high - low
if (span <= 0) then
   allocate(x(0))
   return
end if
allocate(x(n))
do i = 1, n
   u = rng_u64(rng)
   u = iand(u, int(z'7fffffffffffffff', kind=i8))
   x(i) = low + int(mod(u, int(span, kind=i8)), kind=4)
end do
end subroutine rng_integers

subroutine rng_normal(rng, n, x, mu, sigma)
type(rng_state), intent(inout) :: rng
integer, intent(in) :: n
real(kind=dp), intent(in) :: mu, sigma
real(kind=dp), allocatable, intent(out) :: x(:)
real(kind=dp), allocatable :: u(:)
integer :: i
real(kind=dp) :: r, th
allocate(x(n))
call rng_random(rng, n + mod(n,2), u)
do i = 1, n, 2
   r = sqrt(-2.0_dp * log(max(u(i), 1.0e-300_dp)))
   th = twopi * u(i+1)
   x(i) = mu + sigma * r * cos(th)
   if (i+1 <= n) x(i+1) = mu + sigma * r * sin(th)
end do
deallocate(u)
end subroutine rng_normal

subroutine rng_choice_int(rng, a, k, out)
type(rng_state), intent(inout) :: rng
integer, intent(in) :: a(:)
integer, intent(in) :: k
integer, allocatable, intent(out) :: out(:)
integer, allocatable :: idx(:)
integer :: n
n = size(a)
call rng_integers(rng, 1, n+1, k, idx)
allocate(out(k))
out = a(idx)
deallocate(idx)
end subroutine rng_choice_int

subroutine rng_choice_real(rng, a, k, out)
type(rng_state), intent(inout) :: rng
real(kind=dp), intent(in) :: a(:)
integer, intent(in) :: k
real(kind=dp), allocatable, intent(out) :: out(:)
integer, allocatable :: idx(:)
integer :: n
n = size(a)
call rng_integers(rng, 1, n+1, k, idx)
allocate(out(k))
out = a(idx)
deallocate(idx)
end subroutine rng_choice_real

subroutine rng_permutation_int(rng, n, p)
type(rng_state), intent(inout) :: rng
integer, intent(in) :: n
integer, allocatable, intent(out) :: p(:)
integer :: i, j, t
integer, allocatable :: r(:)
allocate(p(n))
do i = 1, n
   p(i) = i
end do
do i = n, 2, -1
   call rng_integers(rng, 1, i+1, 1, r)
   j = r(1)
   t = p(i); p(i) = p(j); p(j) = t
end do
deallocate(r)
end subroutine rng_permutation_int

subroutine rng_permutation_real(rng, a, out)
type(rng_state), intent(inout) :: rng
real(kind=dp), intent(in) :: a(:)
real(kind=dp), allocatable, intent(out) :: out(:)
integer, allocatable :: p(:)
call rng_permutation_int(rng, size(a), p)
allocate(out(size(a)))
out = a(p)
deallocate(p)
end subroutine rng_permutation_real

end module numpy_basic_mod
