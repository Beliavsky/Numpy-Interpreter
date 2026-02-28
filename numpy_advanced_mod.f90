module numpy_advanced_mod
implicit none

integer, parameter :: dp = kind(1.0d0)
real(kind=dp), parameter :: pi = 4.0_dp * atan(1.0_dp)



!=========================== generic interfaces ============================

interface svd
   module procedure svd_real
   module procedure svd_int
end interface svd

interface eig
   module procedure eig_real
   module procedure eig_int
end interface eig

interface solve
   module procedure solve_real
   module procedure solve_int
end interface solve

interface lstsq
   module procedure lstsq_real
   module procedure lstsq_int
end interface lstsq

interface pinv
   module procedure pinv_real
   module procedure pinv_int
end interface pinv

interface det_slogdet
   module procedure det_slogdet_real
   module procedure det_slogdet_int
end interface det_slogdet

interface kron
   module procedure kron_real
   module procedure kron_int
end interface kron

interface sliding_window_1d
   module procedure sliding_window_1d_real
   module procedure sliding_window_1d_int
end interface sliding_window_1d

interface take_along_axis_cols
   module procedure take_along_axis_cols_int
   module procedure take_along_axis_cols_real
end interface take_along_axis_cols

interface put_along_axis_cols
   module procedure put_along_axis_cols_int
   module procedure put_along_axis_cols_real
end interface put_along_axis_cols

interface add_at
   module procedure add_at_int
   module procedure add_at_real
end interface add_at

interface take_1d
   module procedure take_1d_int
   module procedure take_1d_real
end interface take_1d

interface put_1d
   module procedure put_1d_int
   module procedure put_1d_real
end interface put_1d
interface einsum_trace
   module procedure einsum_trace_real
   module procedure einsum_trace_int
end interface einsum_trace

interface einsum_matmul
   module procedure einsum_matmul_real
   module procedure einsum_matmul_int
end interface einsum_matmul

interface
   subroutine dgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)
      import :: dp
      character(len=1), intent(in) :: jobu, jobvt
      integer, intent(in) :: m, n, lda, ldu, ldvt, lwork
      integer, intent(out) :: info
      real(kind=dp), intent(inout) :: a(lda,*)
      real(kind=dp), intent(out) :: s(*), u(ldu,*), vt(ldvt,*), work(*)
   end subroutine dgesvd

   subroutine dgeev(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr, work, lwork, info)
      import :: dp
      character(len=1), intent(in) :: jobvl, jobvr
      integer, intent(in) :: n, lda, ldvl, ldvr, lwork
      integer, intent(out) :: info
      real(kind=dp), intent(inout) :: a(lda,*)
      real(kind=dp), intent(out) :: wr(*), wi(*), vl(ldvl,*), vr(ldvr,*), work(*)
   end subroutine dgeev

   subroutine dsyev(jobz, uplo, n, a, lda, w, work, lwork, info)
      import :: dp
      character(len=1), intent(in) :: jobz, uplo
      integer, intent(in) :: n, lda, lwork
      integer, intent(out) :: info
      real(kind=dp), intent(inout) :: a(lda,*)
      real(kind=dp), intent(out) :: w(*), work(*)
   end subroutine dsyev

   subroutine dgeqrf(m, n, a, lda, tau, work, lwork, info)
      import :: dp
      integer, intent(in) :: m, n, lda, lwork
      integer, intent(out) :: info
      real(kind=dp), intent(inout) :: a(lda,*)
      real(kind=dp), intent(out) :: tau(*), work(*)
   end subroutine dgeqrf

   subroutine dorgqr(m, n, k, a, lda, tau, work, lwork, info)
      import :: dp
      integer, intent(in) :: m, n, k, lda, lwork
      integer, intent(out) :: info
      real(kind=dp), intent(inout) :: a(lda,*)
      real(kind=dp), intent(in) :: tau(*)
      real(kind=dp), intent(out) :: work(*)
   end subroutine dorgqr

   subroutine dpotrf(uplo, n, a, lda, info)
      import :: dp
      character(len=1), intent(in) :: uplo
      integer, intent(in) :: n, lda
      integer, intent(out) :: info
      real(kind=dp), intent(inout) :: a(lda,*)
   end subroutine dpotrf

   subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      import :: dp
      integer, intent(in) :: n, nrhs, lda, ldb
      integer, intent(out) :: info
      integer, intent(out) :: ipiv(*)
      real(kind=dp), intent(inout) :: a(lda,*), b(ldb,*)
   end subroutine dgesv

   subroutine dgels(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
      import :: dp
      character(len=1), intent(in) :: trans
      integer, intent(in) :: m, n, nrhs, lda, ldb, lwork
      integer, intent(out) :: info
      real(kind=dp), intent(inout) :: a(lda,*), b(ldb,*), work(*)
   end subroutine dgels

   subroutine dgetrf(m, n, a, lda, ipiv, info)
      import :: dp
      integer, intent(in) :: m, n, lda
      integer, intent(out) :: info
      integer, intent(out) :: ipiv(*)
      real(kind=dp), intent(inout) :: a(lda,*)
   end subroutine dgetrf
end interface

contains

!=========================== helpers: stable sorting ============================

subroutine argsort_int(x, idx)
integer, intent(in) :: x(:)
integer, intent(out) :: idx(size(x))
integer :: n, i
n = size(x)
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
integer, intent(out) :: idx(size(x))
integer :: n, i
n = size(x)
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

!=========================== linear algebra ============================

subroutine svd_real(a, u, s, vt, info)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: u(:,:), s(:), vt(:,:)
integer, intent(out) :: info
real(kind=dp), allocatable :: ac(:,:), work(:)
real(kind=dp) :: wkopt(1)
integer :: m, n, k, lda, ldu, ldvt, lwork

m = size(a,1)
n = size(a,2)
k = min(m, n)

allocate(ac(m,n))
ac = a

allocate(s(k))
allocate(u(m,k))
allocate(vt(k,n))

lda = max(1, m)
ldu = max(1, m)
ldvt = max(1, k)

lwork = -1
call dgesvd('s', 's', m, n, ac, lda, s, u, ldu, vt, ldvt, wkopt, lwork, info)
if (info /= 0) return

lwork = int(wkopt(1))
allocate(work(max(1,lwork)))
call dgesvd('s', 's', m, n, ac, lda, s, u, ldu, vt, ldvt, work, lwork, info)

deallocate(ac, work)
end subroutine svd_real

subroutine svd_int(a, u, s, vt, info)
integer, intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: u(:,:), s(:), vt(:,:)
integer, intent(out) :: info
real(kind=dp), allocatable :: ar(:,:)
allocate(ar(size(a,1), size(a,2)))
ar = real(a, kind=dp)
call svd_real(ar, u, s, vt, info)
deallocate(ar)
end subroutine svd_int

subroutine eig_real(a, w, vr, info)
real(kind=dp), intent(in) :: a(:,:)
complex(kind=dp), allocatable, intent(out) :: w(:)
real(kind=dp), allocatable, intent(out) :: vr(:,:)
integer, intent(out) :: info

real(kind=dp), allocatable :: ac(:,:), wr(:), wi(:), vl(:,:), work(:)
real(kind=dp) :: wkopt(1)
integer :: n, lda, ldvl, ldvr, lwork

n = size(a,1)
if (size(a,2) /= n) then
   info = -1
   return
end if

allocate(ac(n,n))
ac = a
allocate(wr(n), wi(n))
allocate(vr(n,n))
allocate(vl(1,1))

lda = max(1, n)
ldvl = 1
ldvr = max(1, n)

lwork = -1
call dgeev('n', 'v', n, ac, lda, wr, wi, vl, ldvl, vr, ldvr, wkopt, lwork, info)
if (info /= 0) return

lwork = int(wkopt(1))
allocate(work(max(1,lwork)))
call dgeev('n', 'v', n, ac, lda, wr, wi, vl, ldvl, vr, ldvr, work, lwork, info)

allocate(w(n))
w = cmplx(wr, wi, kind=dp)

deallocate(ac, wr, wi, vl, work)
end subroutine eig_real

subroutine eig_int(a, w, vr, info)
integer, intent(in) :: a(:,:)
complex(kind=dp), allocatable, intent(out) :: w(:)
real(kind=dp), allocatable, intent(out) :: vr(:,:)
integer, intent(out) :: info
real(kind=dp), allocatable :: ar(:,:)
allocate(ar(size(a,1), size(a,2)))
ar = real(a, kind=dp)
call eig_real(ar, w, vr, info)
deallocate(ar)
end subroutine eig_int

subroutine eigh_real(a, w, z, info)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: w(:)
real(kind=dp), allocatable, intent(out) :: z(:,:)
integer, intent(out) :: info

real(kind=dp), allocatable :: ac(:,:), work(:)
real(kind=dp) :: wkopt(1)
integer :: n, lda, lwork

n = size(a,1)
if (size(a,2) /= n) then
   info = -1
   return
end if

allocate(ac(n,n))
ac = a

allocate(w(n))
lda = max(1, n)

lwork = -1
call dsyev('v', 'u', n, ac, lda, w, wkopt, lwork, info)
if (info /= 0) return

lwork = int(wkopt(1))
allocate(work(max(1,lwork)))
call dsyev('v', 'u', n, ac, lda, w, work, lwork, info)

allocate(z(n,n))
z = ac

deallocate(ac, work)
end subroutine eigh_real

subroutine qr_real(a, q, r, info)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: q(:,:), r(:,:)
integer, intent(out) :: info

real(kind=dp), allocatable :: ac(:,:), tau(:), work(:)
real(kind=dp) :: wkopt(1)
integer :: m, n, k, lda, lwork, i, j

m = size(a,1)
n = size(a,2)
k = min(m, n)

allocate(ac(m,n))
ac = a
allocate(tau(k))

lda = max(1, m)

lwork = -1
call dgeqrf(m, n, ac, lda, tau, wkopt, lwork, info)
if (info /= 0) return

lwork = int(wkopt(1))
allocate(work(max(1,lwork)))
call dgeqrf(m, n, ac, lda, tau, work, lwork, info)
if (info /= 0) return

allocate(r(k,n))
r = 0.0_dp
do j = 1, n
   do i = 1, min(k, j)
      r(i,j) = ac(i,j)
   end do
end do

lwork = -1
call dorgqr(m, k, k, ac, lda, tau, wkopt, lwork, info)
if (info /= 0) return

lwork = int(wkopt(1))
deallocate(work)
allocate(work(max(1,lwork)))
call dorgqr(m, k, k, ac, lda, tau, work, lwork, info)

allocate(q(m,k))
q = ac(:,1:k)

deallocate(ac, tau, work)
end subroutine qr_real

subroutine cholesky_real(a, l, info)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: l(:,:)
integer, intent(out) :: info
real(kind=dp), allocatable :: ac(:,:)
integer :: n, lda, i, j

n = size(a,1)
if (size(a,2) /= n) then
   info = -1
   return
end if

allocate(ac(n,n))
ac = a
lda = max(1, n)

call dpotrf('l', n, ac, lda, info)
if (info /= 0) then
   deallocate(ac)
   return
end if

allocate(l(n,n))
l = 0.0_dp
do j = 1, n
   do i = j, n
      l(i,j) = ac(i,j)
   end do
end do

deallocate(ac)
end subroutine cholesky_real

subroutine solve_real(a, b, x, info)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp), allocatable, intent(out) :: x(:)
integer, intent(out) :: info

real(kind=dp), allocatable :: ac(:,:), bc(:,:)
integer, allocatable :: ipiv(:)
integer :: n, nrhs, lda, ldb

n = size(a,1)
if (size(a,2) /= n) then
   info = -1
   return
end if
if (size(b) /= n) then
   info = -2
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
if (info /= 0) then
   deallocate(ac, bc, ipiv)
   return
end if

allocate(x(n))
x = bc(1:n,1)

deallocate(ac, bc, ipiv)
end subroutine solve_real

subroutine solve_int(a, b, x, info)
integer, intent(in) :: a(:,:)
integer, intent(in) :: b(:)
real(kind=dp), allocatable, intent(out) :: x(:)
integer, intent(out) :: info
real(kind=dp), allocatable :: ar(:,:), br(:)
allocate(ar(size(a,1), size(a,2)))
allocate(br(size(b)))
ar = real(a, kind=dp)
br = real(b, kind=dp)
call solve_real(ar, br, x, info)
deallocate(ar, br)
end subroutine solve_int

subroutine lstsq_real(a, b, x, residual_norm2, info)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp), allocatable, intent(out) :: x(:)
real(kind=dp), intent(out) :: residual_norm2
integer, intent(out) :: info

real(kind=dp), allocatable :: ac(:,:), bc(:,:), work(:)
real(kind=dp) :: wkopt(1)
integer :: m, n, nrhs, lda, ldb, lwork, i

m = size(a,1)
n = size(a,2)
if (size(b) /= m) then
   info = -1
   return
end if

allocate(ac(m,n))
ac = a

nrhs = 1
lda = max(1, m)
ldb = max(1, max(m, n))
allocate(bc(ldb,nrhs))
bc = 0.0_dp
bc(1:m,1) = b

lwork = -1
call dgels('n', m, n, nrhs, ac, lda, bc, ldb, wkopt, lwork, info)
if (info /= 0) then
   deallocate(ac, bc)
   return
end if

lwork = int(wkopt(1))
allocate(work(max(1,lwork)))
call dgels('n', m, n, nrhs, ac, lda, bc, ldb, work, lwork, info)
deallocate(work)

if (info /= 0) then
   deallocate(ac, bc)
   return
end if

allocate(x(n))
x = bc(1:n,1)

residual_norm2 = 0.0_dp
if (m > n) then
   do i = n+1, m
      residual_norm2 = residual_norm2 + bc(i,1) * bc(i,1)
   end do
end if

deallocate(ac, bc)
end subroutine lstsq_real

subroutine lstsq_int(a, b, x, residual_norm2, info)
integer, intent(in) :: a(:,:)
integer, intent(in) :: b(:)
real(kind=dp), allocatable, intent(out) :: x(:)
real(kind=dp), intent(out) :: residual_norm2
integer, intent(out) :: info
real(kind=dp), allocatable :: ar(:,:), br(:)
allocate(ar(size(a,1), size(a,2)))
allocate(br(size(b)))
ar = real(a, kind=dp)
br = real(b, kind=dp)
call lstsq_real(ar, br, x, residual_norm2, info)
deallocate(ar, br)
end subroutine lstsq_int

subroutine pinv_real(a, apinv, info)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: apinv(:,:)
integer, intent(out) :: info

real(kind=dp), allocatable :: u(:,:), s(:), vt(:,:), tmp(:,:), invs(:)
integer :: m, n, k, i
real(kind=dp) :: tol, smax

m = size(a,1)
n = size(a,2)
k = min(m, n)

call svd_real(a, u, s, vt, info)
if (info /= 0) return

smax = 0.0_dp
do i = 1, k
   if (s(i) > smax) smax = s(i)
end do
tol = real(max(m,n), kind=dp) * epsilon(1.0_dp) * smax

allocate(invs(k))
do i = 1, k
   if (s(i) > tol) then
      invs(i) = 1.0_dp / s(i)
   else
      invs(i) = 0.0_dp
   end if
end do

allocate(tmp(n,k))
tmp = transpose(vt)
do i = 1, k
   tmp(:,i) = tmp(:,i) * invs(i)
end do

allocate(apinv(n,m))
apinv = matmul(tmp, transpose(u))

deallocate(u, s, vt, tmp, invs)
end subroutine pinv_real

subroutine pinv_int(a, apinv, info)
integer, intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: apinv(:,:)
integer, intent(out) :: info
real(kind=dp), allocatable :: ar(:,:)
allocate(ar(size(a,1), size(a,2)))
ar = real(a, kind=dp)
call pinv_real(ar, apinv, info)
deallocate(ar)
end subroutine pinv_int

subroutine det_slogdet_real(a, det, sign, logabsdet, info)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), intent(out) :: det, sign, logabsdet
integer, intent(out) :: info

real(kind=dp), allocatable :: ac(:,:)
integer, allocatable :: ipiv(:)
integer :: n, lda, i, swaps

n = size(a,1)
if (size(a,2) /= n) then
   info = -1
   det = 0.0_dp
   sign = 0.0_dp
   logabsdet = -huge(1.0_dp)
   return
end if

allocate(ac(n,n))
ac = a
allocate(ipiv(n))
lda = max(1, n)

call dgetrf(n, n, ac, lda, ipiv, info)
if (info /= 0) then
   det = 0.0_dp
   sign = 0.0_dp
   logabsdet = -huge(1.0_dp)
   deallocate(ac, ipiv)
   return
end if

swaps = 0
do i = 1, n
   if (ipiv(i) /= i) swaps = swaps + 1
end do

sign = 1.0_dp
if (mod(swaps,2) == 1) sign = -1.0_dp

logabsdet = 0.0_dp
do i = 1, n
   if (ac(i,i) < 0.0_dp) sign = -sign
   logabsdet = logabsdet + log(abs(ac(i,i)))
end do

det = sign * exp(logabsdet)

deallocate(ac, ipiv)
end subroutine det_slogdet_real

subroutine det_slogdet_int(a, det, sign, logabsdet, info)
integer, intent(in) :: a(:,:)
real(kind=dp), intent(out) :: det, sign, logabsdet
integer, intent(out) :: info
real(kind=dp), allocatable :: ar(:,:)
allocate(ar(size(a,1), size(a,2)))
ar = real(a, kind=dp)
call det_slogdet_real(ar, det, sign, logabsdet, info)
deallocate(ar)
end subroutine det_slogdet_int

function norm2_real(x) result(v)
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: v
integer :: i
v = 0.0_dp
do i = 1, size(x)
   v = v + x(i) * x(i)
end do
v = sqrt(v)
end function norm2_real

function fro_norm_real(a) result(v)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp) :: v
integer :: i, j
v = 0.0_dp
do j = 1, size(a,2)
   do i = 1, size(a,1)
      v = v + a(i,j) * a(i,j)
   end do
end do
v = sqrt(v)
end function fro_norm_real

function einsum_trace_real(a) result(t)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp) :: t
integer :: i, n
n = min(size(a,1), size(a,2))
t = 0.0_dp
do i = 1, n
   t = t + a(i,i)
end do
end function einsum_trace_real

function einsum_trace_int(a) result(t)
integer, intent(in) :: a(:,:)
integer :: t
integer :: i, n
n = min(size(a,1), size(a,2))
t = 0
do i = 1, n
   t = t + a(i,i)
end do
end function einsum_trace_int

subroutine einsum_matmul_real(a, b, c)
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable, intent(out) :: c(:,:)
allocate(c(size(a,1), size(b,2)))
c = matmul(a, b)
end subroutine einsum_matmul_real

subroutine einsum_matmul_int(a, b, c)
integer, intent(in) :: a(:,:), b(:,:)
integer, allocatable, intent(out) :: c(:,:)
allocate(c(size(a,1), size(b,2)))
c = matmul(a, b)
end subroutine einsum_matmul_int
!=========================== tensor ops ============================

subroutine kron_real(a, b, k)
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable, intent(out) :: k(:,:)
integer :: ma, na, mb, nb
integer :: i, j, p, q
ma = size(a,1); na = size(a,2)
mb = size(b,1); nb = size(b,2)
allocate(k(ma*mb, na*nb))
do j = 1, na
   do i = 1, ma
      do q = 1, nb
         do p = 1, mb
            k((i-1)*mb+p, (j-1)*nb+q) = a(i,j) * b(p,q)
         end do
      end do
   end do
end do
end subroutine kron_real

subroutine kron_int(a, b, k)
integer, intent(in) :: a(:,:), b(:,:)
integer, allocatable, intent(out) :: k(:,:)
integer :: ma, na, mb, nb
integer :: i, j, p, q
ma = size(a,1); na = size(a,2)
mb = size(b,1); nb = size(b,2)
allocate(k(ma*mb, na*nb))
do j = 1, na
   do i = 1, ma
      do q = 1, nb
         do p = 1, mb
            k((i-1)*mb+p, (j-1)*nb+q) = a(i,j) * b(p,q)
         end do
      end do
   end do
end do
end subroutine kron_int

subroutine tensordot_a3_b3_contract_last_first_real(a, b, c)
! a(m,n,k), b(k,p,q) -> c(m,n,p,q)
real(kind=dp), intent(in) :: a(:,:,:)
real(kind=dp), intent(in) :: b(:,:,:)
real(kind=dp), allocatable, intent(out) :: c(:,:,:,:)
real(kind=dp), allocatable :: a2(:,:), b2(:,:), c2(:,:)
integer :: m, n, k, p, q
m = size(a,1)
n = size(a,2)
k = size(a,3)
if (size(b,1) /= k) then
   allocate(c(0,0,0,0))
   return
end if
p = size(b,2)
q = size(b,3)

allocate(a2(m*n, k))
allocate(b2(k, p*q))

a2 = reshape(a, [m*n, k])
b2 = reshape(b, [k, p*q])

allocate(c2(m*n, p*q))
c2 = matmul(a2, b2)

allocate(c(m,n,p,q))
c = reshape(c2, [m,n,p,q])

deallocate(a2, b2, c2)
end subroutine tensordot_a3_b3_contract_last_first_real

!=========================== fft (naive dft) ============================

subroutine dft_real(x, xhat)
real(kind=dp), intent(in) :: x(:)
complex(kind=dp), allocatable, intent(out) :: xhat(:)
integer :: n, k, j
real(kind=dp) :: ang
complex(kind=dp) :: s
n = size(x)
allocate(xhat(n))
do k = 1, n
   s = (0.0_dp, 0.0_dp)
   do j = 1, n
      ang = -2.0_dp*pi*real((j-1)*(k-1), kind=dp)/real(n, kind=dp)
      s = s + cmplx(cos(ang), sin(ang), kind=dp) * x(j)
   end do
   xhat(k) = s
end do
end subroutine dft_real

subroutine idft_complex(xhat, x)
complex(kind=dp), intent(in) :: xhat(:)
complex(kind=dp), allocatable, intent(out) :: x(:)
integer :: n, k, j
real(kind=dp) :: ang
complex(kind=dp) :: s
n = size(xhat)
allocate(x(n))
do j = 1, n
   s = (0.0_dp, 0.0_dp)
   do k = 1, n
      ang = 2.0_dp*pi*real((j-1)*(k-1), kind=dp)/real(n, kind=dp)
      s = s + cmplx(cos(ang), sin(ang), kind=dp) * xhat(k)
   end do
   x(j) = s / real(n, kind=dp)
end do
end subroutine idft_complex

subroutine fftshift_complex(x, y)
complex(kind=dp), intent(in) :: x(:)
complex(kind=dp), allocatable, intent(out) :: y(:)
integer :: n, s, i
n = size(x)
allocate(y(n))
s = n / 2
do i = 1, n
   y(i) = x(mod(i-1+s, n) + 1)
end do
end subroutine fftshift_complex

subroutine ifftshift_complex(x, y)
complex(kind=dp), intent(in) :: x(:)
complex(kind=dp), allocatable, intent(out) :: y(:)
integer :: n, s, i
n = size(x)
allocate(y(n))
s = (n + 1) / 2
do i = 1, n
   y(i) = x(mod(i-1+s, n) + 1)
end do
end subroutine ifftshift_complex

!=========================== stride/windowing ============================

subroutine sliding_window_1d_real(x, window, w)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: window
real(kind=dp), allocatable, intent(out) :: w(:,:)
integer :: n, i, j
n = size(x)
if (window < 1 .or. window > n) then
   allocate(w(0,0))
   return
end if
allocate(w(n-window+1, window))
do i = 1, n-window+1
   do j = 1, window
      w(i,j) = x(i+j-1)
   end do
end do
end subroutine sliding_window_1d_real

subroutine sliding_window_1d_int(x, window, w)
integer, intent(in) :: x(:)
integer, intent(in) :: window
integer, allocatable, intent(out) :: w(:,:)
integer :: n, i, j
n = size(x)
if (window < 1 .or. window > n) then
   allocate(w(0,0))
   return
end if
allocate(w(n-window+1, window))
do i = 1, n-window+1
   do j = 1, window
      w(i,j) = x(i+j-1)
   end do
end do
end subroutine sliding_window_1d_int

!=========================== take/put/add.at ============================

subroutine take_along_axis_cols_int(a, idx, out)
! numpy-like: take along axis=1 (columns), but idx is 1-based
integer, intent(in) :: a(:,:)
integer, intent(in) :: idx(:,:)
integer, allocatable, intent(out) :: out(:,:)
integer :: m, n, i, j
m = size(a,1); n = size(a,2)
if (any(shape(idx) /= shape(a))) then
   allocate(out(0,0))
   return
end if
allocate(out(m,n))
do j = 1, n
   do i = 1, m
      out(i,j) = a(i, idx(i,j))
   end do
end do
end subroutine take_along_axis_cols_int

subroutine take_along_axis_cols_real(a, idx, out)
real(kind=dp), intent(in) :: a(:,:)
integer, intent(in) :: idx(:,:)
real(kind=dp), allocatable, intent(out) :: out(:,:)
integer :: m, n, i, j
m = size(a,1); n = size(a,2)
if (any(shape(idx) /= shape(a))) then
   allocate(out(0,0))
   return
end if
allocate(out(m,n))
do j = 1, n
   do i = 1, m
      out(i,j) = a(i, idx(i,j))
   end do
end do
end subroutine take_along_axis_cols_real

subroutine put_along_axis_cols_int(a, idx, vals)
integer, intent(inout) :: a(:,:)
integer, intent(in) :: idx(:,:)
integer, intent(in) :: vals(:,:)
integer :: m, n, i, j
m = size(a,1); n = size(a,2)
if (any(shape(idx) /= shape(a))) return
if (any(shape(vals) /= shape(a))) return
do j = 1, n
   do i = 1, m
      a(i, idx(i,j)) = vals(i,j)
   end do
end do
end subroutine put_along_axis_cols_int

subroutine put_along_axis_cols_real(a, idx, vals)
real(kind=dp), intent(inout) :: a(:,:)
integer, intent(in) :: idx(:,:)
real(kind=dp), intent(in) :: vals(:,:)
integer :: m, n, i, j
m = size(a,1); n = size(a,2)
if (any(shape(idx) /= shape(a))) return
if (any(shape(vals) /= shape(a))) return
do j = 1, n
   do i = 1, m
      a(i, idx(i,j)) = vals(i,j)
   end do
end do
end subroutine put_along_axis_cols_real

subroutine add_at_int(x, idx, vals)
integer, intent(inout) :: x(:)
integer, intent(in) :: idx(:)
integer, intent(in) :: vals(:)
integer :: i
if (size(idx) /= size(vals)) return
do i = 1, size(idx)
   x(idx(i)) = x(idx(i)) + vals(i)
end do
end subroutine add_at_int

subroutine add_at_real(x, idx, vals)
real(kind=dp), intent(inout) :: x(:)
integer, intent(in) :: idx(:)
real(kind=dp), intent(in) :: vals(:)
integer :: i
if (size(idx) /= size(vals)) return
do i = 1, size(idx)
   x(idx(i)) = x(idx(i)) + vals(i)
end do
end subroutine add_at_real

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

subroutine put_1d_int(x, idx, vals)
integer, intent(inout) :: x(:)
integer, intent(in) :: idx(:)
integer, intent(in) :: vals(:)
integer :: i
if (size(idx) /= size(vals)) return
do i = 1, size(idx)
   x(idx(i)) = vals(i)
end do
end subroutine put_1d_int

subroutine put_1d_real(x, idx, vals)
real(kind=dp), intent(inout) :: x(:)
integer, intent(in) :: idx(:)
real(kind=dp), intent(in) :: vals(:)
integer :: i
if (size(idx) /= size(vals)) return
do i = 1, size(idx)
   x(idx(i)) = vals(i)
end do
end subroutine put_1d_real

!=========================== search/sort ============================

subroutine searchsorted_real(x_sorted, v, side_right, pos)
real(kind=dp), intent(in) :: x_sorted(:)
real(kind=dp), intent(in) :: v(:)
logical, intent(in) :: side_right
integer, allocatable, intent(out) :: pos(:)
integer :: i
allocate(pos(size(v)))
do i = 1, size(v)
   pos(i) = searchsorted_one_real(x_sorted, v(i), side_right)
end do
end subroutine searchsorted_real

integer function searchsorted_one_real(x_sorted, val, side_right) result(p)
real(kind=dp), intent(in) :: x_sorted(:)
real(kind=dp), intent(in) :: val
logical, intent(in) :: side_right
integer :: lo, hi, mid
lo = 1
hi = size(x_sorted) + 1
do while (lo < hi)
   mid = (lo + hi) / 2
   if (mid > size(x_sorted)) then
      hi = mid
   else
      if (side_right) then
         if (val < x_sorted(mid)) then
            hi = mid
         else
            lo = mid + 1
         end if
      else
         if (val <= x_sorted(mid)) then
            hi = mid
         else
            lo = mid + 1
         end if
      end if
   end if
end do
p = lo
end function searchsorted_one_real

subroutine lexsort2_int(primary, secondary, idx)
! stable sort: first by secondary, then by primary
integer, intent(in) :: primary(:)
integer, intent(in) :: secondary(:)
integer, allocatable, intent(out) :: idx(:)
integer, allocatable :: idx2(:), tmp(:)
integer :: n
n = size(primary)
allocate(idx(n), idx2(n), tmp(n))

call argsort_int(secondary, idx2)
idx = idx2

call stable_sort_idx_by_int_key(primary, idx, tmp)

deallocate(idx2, tmp)
end subroutine lexsort2_int

subroutine stable_sort_idx_by_int_key(key, idx, tmp)
integer, intent(in) :: key(:)
integer, intent(inout) :: idx(:)
integer, intent(inout) :: tmp(:)
call mergesort_idx_on_key_int(key, idx, tmp, 1, size(idx))
end subroutine stable_sort_idx_by_int_key

recursive subroutine mergesort_idx_on_key_int(key, idx, tmp, l, r)
integer, intent(in) :: key(:)
integer, intent(inout) :: idx(:)
integer, intent(inout) :: tmp(:)
integer, intent(in) :: l, r
integer :: m
if (l >= r) return
m = (l + r) / 2
call mergesort_idx_on_key_int(key, idx, tmp, l, m)
call mergesort_idx_on_key_int(key, idx, tmp, m+1, r)
call merge_idx_on_key_int(key, idx, tmp, l, m, r)
end subroutine mergesort_idx_on_key_int

subroutine merge_idx_on_key_int(key, idx, tmp, l, m, r)
integer, intent(in) :: key(:)
integer, intent(inout) :: idx(:)
integer, intent(inout) :: tmp(:)
integer, intent(in) :: l, m, r
integer :: i, j, k
i = l
j = m + 1
k = l
do while (i <= m .and. j <= r)
   if (key(idx(i)) <= key(idx(j))) then
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
end subroutine merge_idx_on_key_int

subroutine partition_real(x, k, y, idx)
! note: this is a full sort (stronger than numpy partition)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k
real(kind=dp), allocatable, intent(out) :: y(:)
integer, allocatable, intent(out) :: idx(:)
integer :: n
n = size(x)
allocate(idx(n))
call argsort_real(x, idx)
allocate(y(n))
y = x(idx)
end subroutine partition_real

!=========================== hist/binning/set ops ============================

subroutine bincount(labels, counts)
integer, intent(in) :: labels(:)
integer, allocatable, intent(out) :: counts(:)
integer :: i, m
m = 0
do i = 1, size(labels)
   if (labels(i) > m) m = labels(i)
end do
if (m < 0) then
   allocate(counts(0))
   return
end if
allocate(counts(m+1))
counts = 0
do i = 1, size(labels)
   if (labels(i) >= 0) counts(labels(i)+1) = counts(labels(i)+1) + 1
end do
end subroutine bincount

subroutine digitize_real(x, bins, right, idx)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: bins(:)
logical, intent(in) :: right
integer, allocatable, intent(out) :: idx(:)
integer :: i, j, nb
nb = size(bins)
allocate(idx(size(x)))
do i = 1, size(x)
   idx(i) = 0
   do j = 1, nb
      if (right) then
         if (x(i) <= bins(j)) then
            idx(i) = j
            exit
         end if
      else
         if (x(i) < bins(j)) then
            idx(i) = j
            exit
         end if
      end if
   end do
   if (idx(i) == 0) idx(i) = nb + 1
end do
end subroutine digitize_real

subroutine histogram_real(x, edges, hist)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: edges(:)
integer, allocatable, intent(out) :: hist(:)
integer :: nb, i, j
nb = size(edges) - 1
allocate(hist(nb))
hist = 0
do i = 1, size(x)
   do j = 1, nb
      if (j < nb) then
         if (edges(j) <= x(i) .and. x(i) < edges(j+1)) then
            hist(j) = hist(j) + 1
            exit
         end if
      else
         if (edges(j) <= x(i) .and. x(i) <= edges(j+1)) then
            hist(j) = hist(j) + 1
            exit
         end if
      end if
   end do
end do
end subroutine histogram_real

subroutine unique_int(x, u)
integer, intent(in) :: x(:)
integer, allocatable, intent(out) :: u(:)
integer, allocatable :: idx(:)
integer :: n, i, cnt
if (size(x) == 0) then
   allocate(u(0))
   return
end if
n = size(x)
allocate(idx(n))
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

subroutine intersect1d_int(a, b, c)
integer, intent(in) :: a(:), b(:)
integer, allocatable, intent(out) :: c(:)
integer, allocatable :: ua(:), ub(:)
integer :: i, j, na, nb, cnt

call unique_int(a, ua)
call unique_int(b, ub)

na = size(ua); nb = size(ub)
i = 1; j = 1; cnt = 0
do while (i <= na .and. j <= nb)
   if (ua(i) == ub(j)) then
      cnt = cnt + 1
      i = i + 1
      j = j + 1
   else if (ua(i) < ub(j)) then
      i = i + 1
   else
      j = j + 1
   end if
end do

allocate(c(cnt))
i = 1; j = 1; cnt = 0
do while (i <= na .and. j <= nb)
   if (ua(i) == ub(j)) then
      cnt = cnt + 1
      c(cnt) = ua(i)
      i = i + 1
      j = j + 1
   else if (ua(i) < ub(j)) then
      i = i + 1
   else
      j = j + 1
   end if
end do

deallocate(ua, ub)
end subroutine intersect1d_int

subroutine union1d_int(a, b, c)
integer, intent(in) :: a(:), b(:)
integer, allocatable, intent(out) :: c(:)
integer, allocatable :: ua(:), ub(:)
integer :: i, j, na, nb, cnt

call unique_int(a, ua)
call unique_int(b, ub)

na = size(ua); nb = size(ub)
i = 1; j = 1; cnt = 0
do while (i <= na .or. j <= nb)
   if (i > na) then
      cnt = cnt + 1; j = j + 1
   else if (j > nb) then
      cnt = cnt + 1; i = i + 1
   else if (ua(i) == ub(j)) then
      cnt = cnt + 1; i = i + 1; j = j + 1
   else if (ua(i) < ub(j)) then
      cnt = cnt + 1; i = i + 1
   else
      cnt = cnt + 1; j = j + 1
   end if
end do

allocate(c(cnt))
i = 1; j = 1; cnt = 0
do while (i <= na .or. j <= nb)
   if (i > na) then
      cnt = cnt + 1; c(cnt) = ub(j); j = j + 1
   else if (j > nb) then
      cnt = cnt + 1; c(cnt) = ua(i); i = i + 1
   else if (ua(i) == ub(j)) then
      cnt = cnt + 1; c(cnt) = ua(i); i = i + 1; j = j + 1
   else if (ua(i) < ub(j)) then
      cnt = cnt + 1; c(cnt) = ua(i); i = i + 1
   else
      cnt = cnt + 1; c(cnt) = ub(j); j = j + 1
   end if
end do

deallocate(ua, ub)
end subroutine union1d_int

subroutine setdiff1d_int(a, b, c)
integer, intent(in) :: a(:), b(:)
integer, allocatable, intent(out) :: c(:)
integer, allocatable :: ua(:), ub(:)
integer :: i, j, na, nb, cnt

call unique_int(a, ua)
call unique_int(b, ub)

na = size(ua); nb = size(ub)
i = 1; j = 1; cnt = 0
do while (i <= na)
   if (j > nb) then
      cnt = cnt + 1; i = i + 1
   else if (ua(i) == ub(j)) then
      i = i + 1; j = j + 1
   else if (ua(i) < ub(j)) then
      cnt = cnt + 1; i = i + 1
   else
      j = j + 1
   end if
end do

allocate(c(cnt))
i = 1; j = 1; cnt = 0
do while (i <= na)
   if (j > nb) then
      cnt = cnt + 1; c(cnt) = ua(i); i = i + 1
   else if (ua(i) == ub(j)) then
      i = i + 1; j = j + 1
   else if (ua(i) < ub(j)) then
      cnt = cnt + 1; c(cnt) = ua(i); i = i + 1
   else
      j = j + 1
   end if
end do

deallocate(ua, ub)
end subroutine setdiff1d_int

subroutine setxor1d_int(a, b, c)
integer, intent(in) :: a(:), b(:)
integer, allocatable, intent(out) :: c(:)
integer, allocatable :: u(:), inter(:)
call union1d_int(a, b, u)
call intersect1d_int(a, b, inter)
call setdiff1d_int(u, inter, c)
deallocate(u, inter)
end subroutine setxor1d_int

!=========================== nan-aware stats (real only) ============================

function nanmean(x) result(m)
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: m
integer :: i, cnt
real(kind=dp) :: s
s = 0.0_dp
cnt = 0
do i = 1, size(x)
   if (x(i) == x(i)) then
      s = s + x(i)
      cnt = cnt + 1
   end if
end do
if (cnt == 0) then
   m = ieee_value(1.0_dp, ieee_quiet_nan)
else
   m = s / real(cnt, kind=dp)
end if
end function nanmean

function nanstd(x, ddof) result(sd)
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: ddof
real(kind=dp) :: sd
integer :: i, cnt
real(kind=dp) :: mu, s2, dx
mu = nanmean(x)
s2 = 0.0_dp
cnt = 0
do i = 1, size(x)
   if (x(i) == x(i)) then
      dx = x(i) - mu
      s2 = s2 + dx*dx
      cnt = cnt + 1
   end if
end do
if (cnt - ddof <= 0) then
   sd = ieee_value(1.0_dp, ieee_quiet_nan)
else
   sd = sqrt(s2 / real(cnt - ddof, kind=dp))
end if
end function nanstd

end module numpy_advanced_mod
