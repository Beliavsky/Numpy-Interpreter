module numpy_repl_mod
implicit none

integer, parameter :: dp = kind(1.0d0)
integer, parameter :: name_len = 64
integer, parameter :: vt_none = 0
integer, parameter :: vt_i = 1
integer, parameter :: vt_r = 2
integer, parameter :: vt_c = 3
integer, parameter :: vt_i1 = 4
integer, parameter :: vt_r1 = 5
integer, parameter :: vt_c1 = 6
integer, parameter :: vt_i2 = 7
integer, parameter :: vt_r2 = 8
integer, parameter :: vt_c2 = 9
integer, parameter :: vt_list = 10

integer, parameter :: tk_eof = 0
integer, parameter :: tk_ident = 1
integer, parameter :: tk_num = 2
integer, parameter :: tk_lparen = 3
integer, parameter :: tk_rparen = 4
integer, parameter :: tk_lbrack = 5
integer, parameter :: tk_rbrack = 6
integer, parameter :: tk_comma = 7
integer, parameter :: tk_eq = 8
integer, parameter :: tk_plus = 9
integer, parameter :: tk_minus = 10
integer, parameter :: tk_mul = 11
integer, parameter :: tk_div = 12
integer, parameter :: tk_pow = 13
integer, parameter :: tk_at = 14
integer, parameter :: tk_dot = 15

type :: value_t
integer :: tag = vt_none
integer :: i = 0
real(kind=dp) :: r = 0.0_dp
complex(kind=dp) :: c = (0.0_dp, 0.0_dp)
integer, allocatable :: i1(:)
real(kind=dp), allocatable :: r1(:)
complex(kind=dp), allocatable :: c1(:)
integer, allocatable :: i2(:,:)
real(kind=dp), allocatable :: r2(:,:)
complex(kind=dp), allocatable :: c2(:,:)
type(value_t), allocatable :: list(:)
end type value_t

type :: token_t
integer :: kind = tk_eof
character(len=:), allocatable :: text
logical :: is_int = .false.
integer :: ival = 0
real(kind=dp) :: rval = 0.0_dp
end type token_t

type :: parser_t
type(token_t), allocatable :: tok(:)
integer :: pos = 1
end type parser_t

type :: env_t
character(len=name_len), allocatable :: names(:)
type(value_t), allocatable :: vals(:)
end type env_t

contains

recursive subroutine value_free(v)
type(value_t), intent(inout) :: v
integer :: k
if (allocated(v%i1)) deallocate(v%i1)
if (allocated(v%r1)) deallocate(v%r1)
if (allocated(v%c1)) deallocate(v%c1)
if (allocated(v%i2)) deallocate(v%i2)
if (allocated(v%r2)) deallocate(v%r2)
if (allocated(v%c2)) deallocate(v%c2)
if (allocated(v%list)) then
   do k = 1, size(v%list)
      call value_free(v%list(k))
   end do
   deallocate(v%list)
end if
v%tag = vt_none
v%i = 0
v%r = 0.0_dp
v%c = (0.0_dp, 0.0_dp)
end subroutine value_free

subroutine env_init(env)
type(env_t), intent(inout) :: env
allocate(env%names(0))
allocate(env%vals(0))
end subroutine env_init

integer function env_find(env, name) result(idx)
type(env_t), intent(in) :: env
character(len=*), intent(in) :: name
integer :: i
idx = 0
do i = 1, size(env%names)
   if (trim(env%names(i)) == trim(name)) then
      idx = i
      return
   end if
end do
end function env_find

subroutine env_set(env, name, v)
type(env_t), intent(inout) :: env
character(len=*), intent(in) :: name
type(value_t), intent(in) :: v
integer :: idx, n
character(len=:), allocatable :: nm
nm = trim(name)
idx = env_find(env, nm)
if (idx > 0) then
   call value_free(env%vals(idx))
   env%vals(idx) = v
else
   n = size(env%names)
   call extend_env(env, n+1)
   env%names(n+1) = nm
   env%vals(n+1) = v
end if
end subroutine env_set

subroutine extend_env(env, nnew)
type(env_t), intent(inout) :: env
integer, intent(in) :: nnew
character(len=name_len), allocatable :: names2(:)
type(value_t), allocatable :: vals2(:)
integer :: nold, i
nold = size(env%names)
allocate(names2(nnew))
allocate(vals2(nnew))
do i = 1, nold
   names2(i) = env%names(i)
   vals2(i) = env%vals(i)
end do
do i = nold+1, nnew
   names2(i) = ""
   vals2(i)%tag = vt_none
end do
deallocate(env%names, env%vals)
call move_alloc(names2, env%names)
call move_alloc(vals2, env%vals)
end subroutine extend_env

subroutine env_get(env, name, v, ok, msg)
type(env_t), intent(in) :: env
character(len=*), intent(in) :: name
type(value_t), intent(out) :: v
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: idx
ok = .true.
msg = ""
idx = env_find(env, name)
if (idx <= 0) then
   ok = .false.
   msg = "unknown variable: "//trim(name)
   v%tag = vt_none
   return
end if
v = env%vals(idx)
end subroutine env_get

subroutine env_list(env)
type(env_t), intent(in) :: env
integer :: i
if (size(env%names) == 0) then
   print *, "(no variables)"
   return
end if
do i = 1, size(env%names)
   print *, trim(env%names(i))
end do
end subroutine env_list

!=========================== tokenize ============================

subroutine tokenize(line, t, ok, msg)
character(len=*), intent(in) :: line
type(token_t), allocatable, intent(out) :: t(:)
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

integer :: i, n
character(len=1) :: ch

ok = .true.
msg = ""
allocate(t(0))

i = 1
n = len_trim(line)

do while (i <= n)
   ch = line(i:i)
   if (ch == " " .or. ch == char(9)) then
      i = i + 1
   else if (ch == "#") then
      exit
   else if (is_alpha(ch) .or. ch == "_") then
      call read_ident(line, i, t)
   else if (is_digit(ch) .or. (ch == "." .and. i < n .and. is_digit(line(i+1:i+1)))) then
      call read_number(line, i, t, ok, msg)
      if (.not. ok) return
   else
      select case (ch)
      case ("(")
         call push_tok_simple(t, tk_lparen, "("); i = i + 1
      case (")")
         call push_tok_simple(t, tk_rparen, ")"); i = i + 1
      case ("[")
         call push_tok_simple(t, tk_lbrack, "["); i = i + 1
      case ("]")
         call push_tok_simple(t, tk_rbrack, "]"); i = i + 1
      case (",")
         call push_tok_simple(t, tk_comma, ","); i = i + 1
      case ("=")
         call push_tok_simple(t, tk_eq, "="); i = i + 1
      case ("+")
         call push_tok_simple(t, tk_plus, "+"); i = i + 1
      case ("-")
         call push_tok_simple(t, tk_minus, "-"); i = i + 1
      case ("/")
         call push_tok_simple(t, tk_div, "/"); i = i + 1
      case ("@")
         call push_tok_simple(t, tk_at, "@"); i = i + 1
      case (".")
         call push_tok_simple(t, tk_dot, "."); i = i + 1
      case ("*")
         if (i < n .and. line(i+1:i+1) == "*") then
            call push_tok_simple(t, tk_pow, "**"); i = i + 2
         else
            call push_tok_simple(t, tk_mul, "*"); i = i + 1
         end if
      case default
         ok = .false.
         msg = "unexpected character: "//ch
         return
      end select
   end if
end do

call push_tok_simple(t, tk_eof, "")
end subroutine tokenize

logical function is_alpha(ch) result(b)
character(len=1), intent(in) :: ch
integer :: c
c = iachar(ch)
b = (c >= iachar("a") .and. c <= iachar("z")) .or. (c >= iachar("A") .and. c <= iachar("Z"))
end function is_alpha

logical function is_digit(ch) result(b)
character(len=1), intent(in) :: ch
integer :: c
c = iachar(ch)
b = (c >= iachar("0") .and. c <= iachar("9"))
end function is_digit

subroutine push_tok_simple(t, kind, txt)
type(token_t), allocatable, intent(inout) :: t(:)
integer, intent(in) :: kind
character(len=*), intent(in) :: txt
type(token_t), allocatable :: t2(:)
integer :: n
n = size(t)
allocate(t2(n+1))
if (n > 0) t2(1:n) = t
t2(n+1)%kind = kind
t2(n+1)%text = txt
t2(n+1)%is_int = .false.
t2(n+1)%ival = 0
t2(n+1)%rval = 0.0_dp
deallocate(t)
call move_alloc(t2, t)
end subroutine push_tok_simple

subroutine read_ident(line, i, t)
character(len=*), intent(in) :: line
integer, intent(inout) :: i
type(token_t), allocatable, intent(inout) :: t(:)
integer :: n, j
character(len=:), allocatable :: s
n = len_trim(line)
j = i
do while (j <= n)
   if (is_alpha(line(j:j)) .or. is_digit(line(j:j)) .or. line(j:j) == "_") then
      j = j + 1
   else
      exit
   end if
end do
s = line(i:j-1)
call push_tok_simple(t, tk_ident, s)
i = j
end subroutine read_ident

subroutine read_number(line, i, t, ok, msg)
character(len=*), intent(in) :: line
integer, intent(inout) :: i
type(token_t), allocatable, intent(inout) :: t(:)
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

integer :: n, j, ios
character(len=:), allocatable :: s
real(kind=dp) :: rv
logical :: has_dot, has_exp

ok = .true.
msg = ""
n = len_trim(line)
j = i
has_dot = .false.
has_exp = .false.

if (line(j:j) == ".") then
   has_dot = .true.
   j = j + 1
end if

do while (j <= n)
   select case (line(j:j))
   case ("0","1","2","3","4","5","6","7","8","9")
      j = j + 1
   case (".")
      has_dot = .true.
      j = j + 1
   case ("e","E")
      has_exp = .true.
      j = j + 1
      if (j <= n) then
         if (line(j:j) == "+" .or. line(j:j) == "-") j = j + 1
      end if
   case default
      exit
   end select
end do

s = line(i:j-1)
read(s, *, iostat=ios) rv
if (ios /= 0) then
   ok = .false.
   msg = "bad number: "//s
   return
end if

call push_tok_simple(t, tk_num, s)
t(size(t))%rval = rv
t(size(t))%is_int = (.not. has_dot) .and. (.not. has_exp)
if (t(size(t))%is_int) t(size(t))%ival = int(rv)
i = j
end subroutine read_number

!=========================== parser helpers ============================

function peek(p) result(tok)
type(parser_t), intent(in) :: p
type(token_t) :: tok
tok = p%tok(p%pos)
end function peek

subroutine advance(p)
type(parser_t), intent(inout) :: p
if (p%pos < size(p%tok)) p%pos = p%pos + 1
end subroutine advance

subroutine expect(p, kind, ok, msg)
type(parser_t), intent(inout) :: p
integer, intent(in) :: kind
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
type(token_t) :: tk
ok = .true.
msg = ""
tk = peek(p)
if (tk%kind /= kind) then
   ok = .false.
   msg = "parse error: expected token kind "//itoa(kind)
   return
end if
call advance(p)
end subroutine expect

function itoa(i) result(s)
integer, intent(in) :: i
character(len=64) :: buf
character (len=:), allocatable :: s
write(buf,'(I0)') i
s = trim(buf)
end function itoa

integer function prec(kind) result(pv)
integer, intent(in) :: kind
select case (kind)
case (tk_plus, tk_minus)
   pv = 10
case (tk_mul, tk_div)
   pv = 20
case (tk_at)
   pv = 25
case (tk_pow)
   pv = 30
case default
   pv = -1
end select
end function prec

logical function right_assoc(kind) result(b)
integer, intent(in) :: kind
b = (kind == tk_pow)
end function right_assoc

!=========================== parse/eval ============================

subroutine parse_expr(p, env, v, ok, msg)
type(parser_t), intent(inout) :: p
type(env_t), intent(inout) :: env
type(value_t), intent(out) :: v
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
call parse_expr_prec(p, env, 0, v, ok, msg)
end subroutine parse_expr

recursive subroutine parse_expr_prec(p, env, minp, v, ok, msg)
type(parser_t), intent(inout) :: p
type(env_t), intent(inout) :: env
integer, intent(in) :: minp
type(value_t), intent(out) :: v
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

type(value_t) :: left, right, tmp
type(token_t) :: tk
integer :: op, pcur, nextp
logical :: ra

call parse_unary(p, env, left, ok, msg)
if (.not. ok) return

do
   tk = peek(p)
   op = tk%kind
   pcur = prec(op)
   if (pcur < minp) exit

   ra = right_assoc(op)
   call advance(p)
   if (ra) then
      nextp = pcur
   else
      nextp = pcur + 1
   end if

   call parse_expr_prec(p, env, nextp, right, ok, msg)
   if (.not. ok) then
      call value_free(left)
      return
   end if

   call apply_binop(op, left, right, tmp, ok, msg)
   call value_free(left)
   call value_free(right)
   if (.not. ok) return
   left = tmp
end do

v = left
end subroutine parse_expr_prec

recursive subroutine parse_unary(p, env, v, ok, msg)
type(parser_t), intent(inout) :: p
type(env_t), intent(inout) :: env
type(value_t), intent(out) :: v
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

type(token_t) :: tk
type(value_t) :: u

tk = peek(p)
if (tk%kind == tk_plus .or. tk%kind == tk_minus) then
   call advance(p)
   call parse_unary(p, env, u, ok, msg)
   if (.not. ok) return
   if (tk%kind == tk_minus) then
      call apply_unary_minus(u, v, ok, msg)
      call value_free(u)
   else
      v = u
   end if
else
   call parse_postfix(p, env, v, ok, msg)
end if
end subroutine parse_unary

subroutine parse_postfix(p, env, v, ok, msg)
type(parser_t), intent(inout) :: p
type(env_t), intent(inout) :: env
type(value_t), intent(out) :: v
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

type(value_t) :: cur, tmp
type(token_t) :: tk
character(len=:), allocatable :: attr

call parse_primary(p, env, cur, ok, msg)
if (.not. ok) return

do
   tk = peek(p)
   if (tk%kind == tk_lbrack) then
      call parse_index(p, env, cur, tmp, ok, msg)
      call value_free(cur)
      if (.not. ok) return
      cur = tmp
   else if (tk%kind == tk_dot) then
      call advance(p)
      tk = peek(p)
      if (tk%kind /= tk_ident) then
         ok = .false.
         msg = "expected attribute name after '.'"
         call value_free(cur)
         return
      end if
      attr = tk%text
      call advance(p)
      call apply_attr(cur, attr, tmp, ok, msg)
      call value_free(cur)
      if (.not. ok) return
      cur = tmp
   else
      exit
   end if
end do

v = cur
end subroutine parse_postfix

subroutine parse_primary(p, env, v, ok, msg)
type(parser_t), intent(inout) :: p
type(env_t), intent(inout) :: env
type(value_t), intent(out) :: v
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

type(token_t) :: tk, tk2
character(len=:), allocatable :: name
type(value_t), allocatable :: args(:)
integer :: nargs

ok = .true.
msg = ""
tk = peek(p)

select case (tk%kind)
case (tk_num)
   if (tk%is_int) then
      v%tag = vt_i
      v%i = tk%ival
   else
      v%tag = vt_r
      v%r = tk%rval
   end if
   call advance(p)
case (tk_ident)
   name = tk%text
   tk2 = p%tok(min(p%pos+1, size(p%tok)))
   if (tk2%kind == tk_lparen) then
      call advance(p)
      call parse_call_args(p, env, args, nargs, ok, msg)
      if (.not. ok) return
      call call_builtin(name, args, v, ok, msg)
      call free_args(args)
      return
   else
      call advance(p)
      if (trim(name) == "pi") then
         v%tag = vt_r
         v%r = 4.0_dp * atan(1.0_dp)
         return
      else if (trim(name) == "vars") then
         call env_list(env)
         v%tag = vt_none
         return
      end if
      call env_get(env, name, v, ok, msg)
      return
   end if
case (tk_lparen)
   call advance(p)
   call parse_expr(p, env, v, ok, msg)
   if (.not. ok) return
   call expect(p, tk_rparen, ok, msg)
case (tk_lbrack)
   call parse_array_literal(p, env, v, ok, msg)
case default
   ok = .false.
   msg = "unexpected token in expression"
   v%tag = vt_none
end select
end subroutine parse_primary

subroutine free_args(args)
type(value_t), allocatable, intent(inout) :: args(:)
integer :: i
if (.not. allocated(args)) return
do i = 1, size(args)
   call value_free(args(i))
end do
deallocate(args)
end subroutine free_args

subroutine extend_args(args, nnew)
type(value_t), allocatable, intent(inout) :: args(:)
integer, intent(in) :: nnew
type(value_t), allocatable :: a2(:)
integer :: nold, i
nold = size(args)
allocate(a2(nnew))
do i = 1, nold
   a2(i) = args(i)
end do
do i = nold+1, nnew
   a2(i)%tag = vt_none
end do
deallocate(args)
call move_alloc(a2, args)
end subroutine extend_args

subroutine parse_call_args(p, env, args, nargs, ok, msg)
type(parser_t), intent(inout) :: p
type(env_t), intent(inout) :: env
type(value_t), allocatable, intent(out) :: args(:)
integer, intent(out) :: nargs
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

type(token_t) :: tk
type(value_t) :: vtmp
integer :: n

ok = .true.
msg = ""
nargs = 0
allocate(args(0))

call expect(p, tk_lparen, ok, msg)
if (.not. ok) return

tk = peek(p)
if (tk%kind == tk_rparen) then
   call advance(p)
   return
end if

do
   call parse_expr(p, env, vtmp, ok, msg)
   if (.not. ok) then
      call free_args(args)
      return
   end if
   n = size(args)
   call extend_args(args, n+1)
   args(n+1) = vtmp

   tk = peek(p)
   if (tk%kind == tk_comma) then
      call advance(p)
   else
      exit
   end if
end do

call expect(p, tk_rparen, ok, msg)
if (.not. ok) then
   call free_args(args)
   return
end if
nargs = size(args)
end subroutine parse_call_args

subroutine parse_array_literal(p, env, v, ok, msg)
type(parser_t), intent(inout) :: p
type(env_t), intent(inout) :: env
type(value_t), intent(out) :: v
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

type(token_t) :: tk
type(value_t), allocatable :: items(:)
type(value_t) :: it
integer :: n
logical :: is2d

ok = .true.
msg = ""
allocate(items(0))

call expect(p, tk_lbrack, ok, msg)
if (.not. ok) return

tk = peek(p)
if (tk%kind == tk_rbrack) then
   call advance(p)
   v%tag = vt_r1
   allocate(v%r1(0))
   return
end if

is2d = (tk%kind == tk_lbrack)

do
   if (is2d) then
      call parse_row_literal(p, env, it, ok, msg)
   else
      call parse_expr(p, env, it, ok, msg)
      if (it%tag /= vt_i .and. it%tag /= vt_r) then
         ok = .false.; msg = "1d array literal elements must be scalar"
      end if
   end if
   if (.not. ok) then
      call free_args(items)
      return
   end if

   n = size(items)
   call extend_args(items, n+1)
   items(n+1) = it

   tk = peek(p)
   if (tk%kind == tk_comma) then
      call advance(p)
   else
      exit
   end if
end do

call expect(p, tk_rbrack, ok, msg)
if (.not. ok) then
   call free_args(items)
   return
end if

if (.not. is2d) then
   call build_1d_from_scalars(items, v, ok, msg)
else
   call build_2d_from_rows(items, v, ok, msg)
end if
call free_args(items)
end subroutine parse_array_literal

subroutine parse_row_literal(p, env, row, ok, msg)
type(parser_t), intent(inout) :: p
type(env_t), intent(inout) :: env
type(value_t), intent(out) :: row
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

type(token_t) :: tk
type(value_t), allocatable :: items(:)
type(value_t) :: it
integer :: n

ok = .true.
msg = ""
allocate(items(0))

call expect(p, tk_lbrack, ok, msg)
if (.not. ok) return

tk = peek(p)
if (tk%kind == tk_rbrack) then
   call advance(p)
   row%tag = vt_r1
   allocate(row%r1(0))
   return
end if

do
   call parse_expr(p, env, it, ok, msg)
   if (.not. ok) then
      call free_args(items)
      return
   end if
   if (it%tag /= vt_i .and. it%tag /= vt_r) then
      ok = .false.; msg = "2d array literal elements must be scalar"
      call free_args(items)
      return
   end if

   n = size(items)
   call extend_args(items, n+1)
   items(n+1) = it

   tk = peek(p)
   if (tk%kind == tk_comma) then
      call advance(p)
   else
      exit
   end if
end do

call expect(p, tk_rbrack, ok, msg)
if (.not. ok) then
   call free_args(items)
   return
end if

call build_1d_from_scalars(items, row, ok, msg)
call free_args(items)
end subroutine parse_row_literal

subroutine build_1d_from_scalars(items, v, ok, msg)
type(value_t), intent(in) :: items(:)
type(value_t), intent(out) :: v
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

integer :: i, n
logical :: any_real
ok = .true.
msg = ""
n = size(items)
any_real = .false.
do i = 1, n
   if (items(i)%tag == vt_r) any_real = .true.
end do

if (any_real) then
   v%tag = vt_r1
   allocate(v%r1(n))
   do i = 1, n
      if (items(i)%tag == vt_i) then
         v%r1(i) = real(items(i)%i, kind=dp)
      else
         v%r1(i) = items(i)%r
      end if
   end do
else
   v%tag = vt_i1
   allocate(v%i1(n))
   do i = 1, n
      v%i1(i) = items(i)%i
   end do
end if
end subroutine build_1d_from_scalars

subroutine build_2d_from_rows(rows, v, ok, msg)
type(value_t), intent(in) :: rows(:)
type(value_t), intent(out) :: v
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

integer :: nr, nc, i, j
logical :: any_real

ok = .true.
msg = ""
nr = size(rows)
if (nr == 0) then
   v%tag = vt_r2
   allocate(v%r2(0,0))
   return
end if

nc = -1
any_real = .false.
do i = 1, nr
   if (rows(i)%tag == vt_r1) any_real = .true.
   if (rows(i)%tag /= vt_i1 .and. rows(i)%tag /= vt_r1) then
      ok = .false.; msg = "2d literal rows must be 1d vectors"; return
   end if
   if (nc < 0) then
      if (rows(i)%tag == vt_i1) nc = size(rows(i)%i1)
      if (rows(i)%tag == vt_r1) nc = size(rows(i)%r1)
   else
      if (rows(i)%tag == vt_i1) then
         if (size(rows(i)%i1) /= nc) then
            ok = .false.; msg = "inconsistent row length"; return
         end if
      else
         if (size(rows(i)%r1) /= nc) then
            ok = .false.; msg = "inconsistent row length"; return
         end if
      end if
   end if
end do

if (any_real) then
   v%tag = vt_r2
   allocate(v%r2(nr, nc))
   do i = 1, nr
      do j = 1, nc
         if (rows(i)%tag == vt_i1) then
            v%r2(i,j) = real(rows(i)%i1(j), kind=dp)
         else
            v%r2(i,j) = rows(i)%r1(j)
         end if
      end do
   end do
else
   v%tag = vt_i2
   allocate(v%i2(nr, nc))
   do i = 1, nr
      do j = 1, nc
         v%i2(i,j) = rows(i)%i1(j)
      end do
   end do
end if
end subroutine build_2d_from_rows

subroutine parse_index(p, env, base, out, ok, msg)
type(parser_t), intent(inout) :: p
type(env_t), intent(inout) :: env
type(value_t), intent(in) :: base
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

type(value_t) :: i1v, i2v
type(token_t) :: tk
integer :: i1, i2

ok = .true.
msg = ""
call expect(p, tk_lbrack, ok, msg)
if (.not. ok) return

call parse_expr(p, env, i1v, ok, msg)
if (.not. ok) return
if (i1v%tag /= vt_i) then
   ok = .false.; msg = "index must be integer scalar"
   call value_free(i1v)
   return
end if
i1 = i1v%i
call value_free(i1v)

tk = peek(p)
if (tk%kind == tk_comma) then
   call advance(p)
   call parse_expr(p, env, i2v, ok, msg)
   if (.not. ok) return
   if (i2v%tag /= vt_i) then
      ok = .false.; msg = "index must be integer scalar"
      call value_free(i2v)
      return
   end if
   i2 = i2v%i
   call value_free(i2v)
   call expect(p, tk_rbrack, ok, msg)
   if (.not. ok) return
   call apply_index_2d(base, i1, i2, out, ok, msg)
else
   call expect(p, tk_rbrack, ok, msg)
   if (.not. ok) return
   call apply_index_1(base, i1, out, ok, msg)
end if
end subroutine parse_index

subroutine apply_index_1(base, i0, out, ok, msg)
type(value_t), intent(in) :: base
integer, intent(in) :: i0
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: i, n

ok = .true.
msg = ""
i = i0 + 1

select case (base%tag)
case (vt_i1)
   n = size(base%i1)
   if (i < 1 .or. i > n) then
      ok = .false.; msg = "index out of range"; return
   end if
   out%tag = vt_i
   out%i = base%i1(i)
case (vt_r1)
   n = size(base%r1)
   if (i < 1 .or. i > n) then
      ok = .false.; msg = "index out of range"; return
   end if
   out%tag = vt_r
   out%r = base%r1(i)
case (vt_c1)
   n = size(base%c1)
   if (i < 1 .or. i > n) then
      ok = .false.; msg = "index out of range"; return
   end if
   out%tag = vt_c
   out%c = base%c1(i)
case (vt_i2)
   n = size(base%i2,1)
   if (i < 1 .or. i > n) then
      ok = .false.; msg = "row index out of range"; return
   end if
   out%tag = vt_i1
   allocate(out%i1(size(base%i2,2)))
   out%i1 = base%i2(i,:)
case (vt_r2)
   n = size(base%r2,1)
   if (i < 1 .or. i > n) then
      ok = .false.; msg = "row index out of range"; return
   end if
   out%tag = vt_r1
   allocate(out%r1(size(base%r2,2)))
   out%r1 = base%r2(i,:)
case default
   ok = .false.; msg = "indexing not supported"
end select
end subroutine apply_index_1

subroutine apply_index_2d(base, i0, j0, out, ok, msg)
type(value_t), intent(in) :: base
integer, intent(in) :: i0, j0
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: i, j, n1, n2
ok = .true.
msg = ""
i = i0 + 1
j = j0 + 1
select case (base%tag)
case (vt_i2)
   n1 = size(base%i2,1); n2 = size(base%i2,2)
   if (i < 1 .or. i > n1 .or. j < 1 .or. j > n2) then
      ok = .false.; msg = "index out of range"; return
   end if
   out%tag = vt_i
   out%i = base%i2(i,j)
case (vt_r2)
   n1 = size(base%r2,1); n2 = size(base%r2,2)
   if (i < 1 .or. i > n1 .or. j < 1 .or. j > n2) then
      ok = .false.; msg = "index out of range"; return
   end if
   out%tag = vt_r
   out%r = base%r2(i,j)
case (vt_c2)
   n1 = size(base%c2,1); n2 = size(base%c2,2)
   if (i < 1 .or. i > n1 .or. j < 1 .or. j > n2) then
      ok = .false.; msg = "index out of range"; return
   end if
   out%tag = vt_c
   out%c = base%c2(i,j)
case default
   ok = .false.; msg = "2d indexing requires matrix"
end select
end subroutine apply_index_2d

subroutine apply_attr(base, attr, out, ok, msg)
type(value_t), intent(in) :: base
character(len=*), intent(in) :: attr
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.
msg = ""
if (trim(attr) == "t" .or. trim(attr) == "T") then
   call builtin_transpose(base, out, ok, msg)
else
   ok = .false.; msg = "unknown attribute: "//trim(attr)
end if
end subroutine apply_attr

subroutine apply_unary_minus(a, out, ok, msg)
type(value_t), intent(in) :: a
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.
msg = ""
select case (a%tag)
case (vt_i)
   out%tag = vt_i; out%i = -a%i
case (vt_r)
   out%tag = vt_r; out%r = -a%r
case (vt_c)
   out%tag = vt_c; out%c = -a%c
case (vt_i1)
   out%tag = vt_i1; allocate(out%i1(size(a%i1))); out%i1 = -a%i1
case (vt_r1)
   out%tag = vt_r1; allocate(out%r1(size(a%r1))); out%r1 = -a%r1
case (vt_c1)
   out%tag = vt_c1; allocate(out%c1(size(a%c1))); out%c1 = -a%c1
case (vt_i2)
   out%tag = vt_i2; allocate(out%i2(size(a%i2,1), size(a%i2,2))); out%i2 = -a%i2
case (vt_r2)
   out%tag = vt_r2; allocate(out%r2(size(a%r2,1), size(a%r2,2))); out%r2 = -a%r2
case (vt_c2)
   out%tag = vt_c2; allocate(out%c2(size(a%c2,1), size(a%c2,2))); out%c2 = -a%c2
case default
   ok = .false.; msg = "unary minus not supported"
end select
end subroutine apply_unary_minus

!=========================== numeric operations ============================

subroutine apply_binop(op, a, b, out, ok, msg)
type(value_t), intent(in) :: a, b
integer, intent(in) :: op
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

type(value_t) :: ar, br

ok = .true.
msg = ""

if (op == tk_at) then
   call apply_matmul(a, b, out, ok, msg)
   return
end if

if (op == tk_div) then
   call to_real(a, ar, ok, msg); if (.not. ok) return
   call to_real(b, br, ok, msg); if (.not. ok) return
   call apply_elemwise_real(op, ar, br, out, ok, msg)
   call value_free(ar); call value_free(br)
   return
end if

if (is_complex(a) .or. is_complex(b)) then
   call to_complex(a, ar, ok, msg); if (.not. ok) return
   call to_complex(b, br, ok, msg); if (.not. ok) return
   call apply_elemwise_complex(op, ar, br, out, ok, msg)
   call value_free(ar); call value_free(br)
else if (is_real(a) .or. is_real(b)) then
   call to_real(a, ar, ok, msg); if (.not. ok) return
   call to_real(b, br, ok, msg); if (.not. ok) return
   call apply_elemwise_real(op, ar, br, out, ok, msg)
   call value_free(ar); call value_free(br)
else
   call apply_elemwise_int(op, a, b, out, ok, msg)
end if
end subroutine apply_binop

logical function is_real(v) result(b)
type(value_t), intent(in) :: v
b = (v%tag == vt_r .or. v%tag == vt_r1 .or. v%tag == vt_r2)
end function is_real

logical function is_complex(v) result(b)
type(value_t), intent(in) :: v
b = (v%tag == vt_c .or. v%tag == vt_c1 .or. v%tag == vt_c2)
end function is_complex

subroutine to_real(v, out, ok, msg)
type(value_t), intent(in) :: v
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: i, j
ok = .true.
msg = ""
select case (v%tag)
case (vt_r, vt_r1, vt_r2)
   out = v
case (vt_i)
   out%tag = vt_r; out%r = real(v%i, kind=dp)
case (vt_i1)
   out%tag = vt_r1; allocate(out%r1(size(v%i1)))
   do i = 1, size(v%i1)
      out%r1(i) = real(v%i1(i), kind=dp)
   end do
case (vt_i2)
   out%tag = vt_r2; allocate(out%r2(size(v%i2,1), size(v%i2,2)))
   do j = 1, size(v%i2,2)
      do i = 1, size(v%i2,1)
         out%r2(i,j) = real(v%i2(i,j), kind=dp)
      end do
   end do
case default
   ok = .false.; msg = "cannot convert to real"
end select
end subroutine to_real

subroutine to_complex(v, out, ok, msg)
type(value_t), intent(in) :: v
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: i, j
ok = .true.
msg = ""
select case (v%tag)
case (vt_c, vt_c1, vt_c2)
   out = v
case (vt_r)
   out%tag = vt_c; out%c = cmplx(v%r, 0.0_dp, kind=dp)
case (vt_i)
   out%tag = vt_c; out%c = cmplx(real(v%i,kind=dp), 0.0_dp, kind=dp)
case (vt_r1)
   out%tag = vt_c1; allocate(out%c1(size(v%r1)))
   out%c1 = cmplx(v%r1, 0.0_dp, kind=dp)
case (vt_i1)
   out%tag = vt_c1; allocate(out%c1(size(v%i1)))
   do i = 1, size(v%i1)
      out%c1(i) = cmplx(real(v%i1(i),kind=dp), 0.0_dp, kind=dp)
   end do
case (vt_r2)
   out%tag = vt_c2; allocate(out%c2(size(v%r2,1), size(v%r2,2)))
   out%c2 = cmplx(v%r2, 0.0_dp, kind=dp)
case (vt_i2)
   out%tag = vt_c2; allocate(out%c2(size(v%i2,1), size(v%i2,2)))
   do j = 1, size(v%i2,2)
      do i = 1, size(v%i2,1)
         out%c2(i,j) = cmplx(real(v%i2(i,j),kind=dp), 0.0_dp, kind=dp)
      end do
   end do
case default
   ok = .false.; msg = "cannot convert to complex"
end select
end subroutine to_complex

subroutine apply_elemwise_int(op, a, b, out, ok, msg)
type(value_t), intent(in) :: a, b
integer, intent(in) :: op
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: i

ok = .true.
msg = ""

if (a%tag == vt_i .and. b%tag == vt_i) then
   out%tag = vt_i
   select case (op)
   case (tk_plus); out%i = a%i + b%i
   case (tk_minus); out%i = a%i - b%i
   case (tk_mul); out%i = a%i * b%i
   case (tk_pow); out%i = a%i ** b%i
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if

! scalar broadcast
if (a%tag == vt_i .and. b%tag == vt_i1) then
   out%tag = vt_i1; allocate(out%i1(size(b%i1)))
   select case (op)
   case (tk_plus); out%i1 = a%i + b%i1
   case (tk_minus); out%i1 = a%i - b%i1
   case (tk_mul); out%i1 = a%i * b%i1
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if
if (a%tag == vt_i1 .and. b%tag == vt_i) then
   out%tag = vt_i1; allocate(out%i1(size(a%i1)))
   select case (op)
   case (tk_plus); out%i1 = a%i1 + b%i
   case (tk_minus); out%i1 = a%i1 - b%i
   case (tk_mul); out%i1 = a%i1 * b%i
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if
if (a%tag == vt_i .and. b%tag == vt_i2) then
   out%tag = vt_i2; allocate(out%i2(size(b%i2,1), size(b%i2,2)))
   select case (op)
   case (tk_plus); out%i2 = a%i + b%i2
   case (tk_minus); out%i2 = a%i - b%i2
   case (tk_mul); out%i2 = a%i * b%i2
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if
if (a%tag == vt_i2 .and. b%tag == vt_i) then
   out%tag = vt_i2; allocate(out%i2(size(a%i2,1), size(a%i2,2)))
   select case (op)
   case (tk_plus); out%i2 = a%i2 + b%i
   case (tk_minus); out%i2 = a%i2 - b%i
   case (tk_mul); out%i2 = a%i2 * b%i
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if

! same-shape arrays
if (a%tag == vt_i1 .and. b%tag == vt_i1) then
   if (size(a%i1) /= size(b%i1)) then
      ok = .false.; msg = "shape mismatch"; return
   end if
   out%tag = vt_i1; allocate(out%i1(size(a%i1)))
   select case (op)
   case (tk_plus); out%i1 = a%i1 + b%i1
   case (tk_minus); out%i1 = a%i1 - b%i1
   case (tk_mul); out%i1 = a%i1 * b%i1
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if
if (a%tag == vt_i2 .and. b%tag == vt_i2) then
   if (size(a%i2,1) /= size(b%i2,1) .or. size(a%i2,2) /= size(b%i2,2)) then
      ok = .false.; msg = "shape mismatch"; return
   end if
   out%tag = vt_i2; allocate(out%i2(size(a%i2,1), size(a%i2,2)))
   select case (op)
   case (tk_plus); out%i2 = a%i2 + b%i2
   case (tk_minus); out%i2 = a%i2 - b%i2
   case (tk_mul); out%i2 = a%i2 * b%i2
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if

ok = .false.; msg = "type mismatch"
end subroutine apply_elemwise_int

subroutine apply_elemwise_real(op, a, b, out, ok, msg)
type(value_t), intent(in) :: a, b
integer, intent(in) :: op
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

ok = .true.
msg = ""

if (a%tag == vt_r .and. b%tag == vt_r) then
   out%tag = vt_r
   select case (op)
   case (tk_plus); out%r = a%r + b%r
   case (tk_minus); out%r = a%r - b%r
   case (tk_mul); out%r = a%r * b%r
   case (tk_div); out%r = a%r / b%r
   case (tk_pow); out%r = a%r ** b%r
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if

! scalar broadcast
if (a%tag == vt_r .and. b%tag == vt_r1) then
   out%tag = vt_r1; allocate(out%r1(size(b%r1)))
   select case (op)
   case (tk_plus); out%r1 = a%r + b%r1
   case (tk_minus); out%r1 = a%r - b%r1
   case (tk_mul); out%r1 = a%r * b%r1
   case (tk_div); out%r1 = a%r / b%r1
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if
if (a%tag == vt_r1 .and. b%tag == vt_r) then
   out%tag = vt_r1; allocate(out%r1(size(a%r1)))
   select case (op)
   case (tk_plus); out%r1 = a%r1 + b%r
   case (tk_minus); out%r1 = a%r1 - b%r
   case (tk_mul); out%r1 = a%r1 * b%r
   case (tk_div); out%r1 = a%r1 / b%r
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if
if (a%tag == vt_r .and. b%tag == vt_r2) then
   out%tag = vt_r2; allocate(out%r2(size(b%r2,1), size(b%r2,2)))
   select case (op)
   case (tk_plus); out%r2 = a%r + b%r2
   case (tk_minus); out%r2 = a%r - b%r2
   case (tk_mul); out%r2 = a%r * b%r2
   case (tk_div); out%r2 = a%r / b%r2
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if
if (a%tag == vt_r2 .and. b%tag == vt_r) then
   out%tag = vt_r2; allocate(out%r2(size(a%r2,1), size(a%r2,2)))
   select case (op)
   case (tk_plus); out%r2 = a%r2 + b%r
   case (tk_minus); out%r2 = a%r2 - b%r
   case (tk_mul); out%r2 = a%r2 * b%r
   case (tk_div); out%r2 = a%r2 / b%r
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if

! same-shape arrays
if (a%tag == vt_r1 .and. b%tag == vt_r1) then
   if (size(a%r1) /= size(b%r1)) then
      ok = .false.; msg = "shape mismatch"; return
   end if
   out%tag = vt_r1; allocate(out%r1(size(a%r1)))
   select case (op)
   case (tk_plus); out%r1 = a%r1 + b%r1
   case (tk_minus); out%r1 = a%r1 - b%r1
   case (tk_mul); out%r1 = a%r1 * b%r1
   case (tk_div); out%r1 = a%r1 / b%r1
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if
if (a%tag == vt_r2 .and. b%tag == vt_r2) then
   if (size(a%r2,1) /= size(b%r2,1) .or. size(a%r2,2) /= size(b%r2,2)) then
      ok = .false.; msg = "shape mismatch"; return
   end if
   out%tag = vt_r2; allocate(out%r2(size(a%r2,1), size(a%r2,2)))
   select case (op)
   case (tk_plus); out%r2 = a%r2 + b%r2
   case (tk_minus); out%r2 = a%r2 - b%r2
   case (tk_mul); out%r2 = a%r2 * b%r2
   case (tk_div); out%r2 = a%r2 / b%r2
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if

ok = .false.; msg = "type mismatch"
end subroutine apply_elemwise_real

subroutine apply_elemwise_complex(op, a, b, out, ok, msg)
type(value_t), intent(in) :: a, b
integer, intent(in) :: op
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.
msg = ""
if (a%tag == vt_c .and. b%tag == vt_c) then
   out%tag = vt_c
   select case (op)
   case (tk_plus); out%c = a%c + b%c
   case (tk_minus); out%c = a%c - b%c
   case (tk_mul); out%c = a%c * b%c
   case (tk_div); out%c = a%c / b%c
   case (tk_pow); out%c = a%c ** b%c
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if
if (a%tag == vt_c1 .and. b%tag == vt_c1) then
   if (size(a%c1) /= size(b%c1)) then
      ok = .false.; msg = "shape mismatch"; return
   end if
   out%tag = vt_c1; allocate(out%c1(size(a%c1)))
   select case (op)
   case (tk_plus); out%c1 = a%c1 + b%c1
   case (tk_minus); out%c1 = a%c1 - b%c1
   case (tk_mul); out%c1 = a%c1 * b%c1
   case (tk_div); out%c1 = a%c1 / b%c1
   case default; ok = .false.; msg = "op not supported"
   end select
   return
end if
ok = .false.; msg = "complex op not supported for these shapes"
end subroutine apply_elemwise_complex

subroutine apply_matmul(a, b, out, ok, msg)
use numpy_basic_mod, only: dot1, matmul2
type(value_t), intent(in) :: a, b
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

type(value_t) :: ar, br
real(kind=dp) :: dr
integer :: di
real(kind=dp), allocatable :: cr(:,:)
integer, allocatable :: ci(:,:)

ok = .true.
msg = ""

if (is_complex(a) .or. is_complex(b)) then
   ok = .false.; msg = "matmul with complex not supported"
   return
end if

if (is_real(a) .or. is_real(b)) then
   call to_real(a, ar, ok, msg); if (.not. ok) return
   call to_real(b, br, ok, msg); if (.not. ok) return

   if (ar%tag == vt_r1 .and. br%tag == vt_r1) then
      if (size(ar%r1) /= size(br%r1)) then
         ok = .false.; msg = "shape mismatch"; return
      end if
      call dot1(ar%r1, br%r1, dr)
      out%tag = vt_r; out%r = dr
   else if (ar%tag == vt_r2 .and. br%tag == vt_r2) then
      if (size(ar%r2,2) /= size(br%r2,1)) then
         ok = .false.; msg = "shape mismatch"; return
      end if
      call matmul2(ar%r2, br%r2, cr)
      out%tag = vt_r2; allocate(out%r2(size(cr,1), size(cr,2))); out%r2 = cr
      deallocate(cr)
   else if (ar%tag == vt_r2 .and. br%tag == vt_r1) then
      if (size(ar%r2,2) /= size(br%r1)) then
         ok = .false.; msg = "shape mismatch"; return
      end if
      out%tag = vt_r1; allocate(out%r1(size(ar%r2,1))); out%r1 = matmul(ar%r2, br%r1)
   else if (ar%tag == vt_r1 .and. br%tag == vt_r2) then
      if (size(ar%r1) /= size(br%r2,1)) then
         ok = .false.; msg = "shape mismatch"; return
      end if
      out%tag = vt_r1; allocate(out%r1(size(br%r2,2))); out%r1 = matmul(ar%r1, br%r2)
   else
      ok = .false.; msg = "matmul shapes not supported"
   end if

   call value_free(ar); call value_free(br)

else
   if (a%tag == vt_i1 .and. b%tag == vt_i1) then
      if (size(a%i1) /= size(b%i1)) then
         ok = .false.; msg = "shape mismatch"; return
      end if
      call dot1(a%i1, b%i1, di)
      out%tag = vt_i; out%i = di
   else if (a%tag == vt_i2 .and. b%tag == vt_i2) then
      if (size(a%i2,2) /= size(b%i2,1)) then
         ok = .false.; msg = "shape mismatch"; return
      end if
      call matmul2(a%i2, b%i2, ci)
      out%tag = vt_i2; allocate(out%i2(size(ci,1), size(ci,2))); out%i2 = ci
      deallocate(ci)
   else if (a%tag == vt_i2 .and. b%tag == vt_i1) then
      if (size(a%i2,2) /= size(b%i1)) then
         ok = .false.; msg = "shape mismatch"; return
      end if
      out%tag = vt_i1; allocate(out%i1(size(a%i2,1))); out%i1 = matmul(a%i2, b%i1)
   else if (a%tag == vt_i1 .and. b%tag == vt_i2) then
      if (size(a%i1) /= size(b%i2,1)) then
         ok = .false.; msg = "shape mismatch"; return
      end if
      out%tag = vt_i1; allocate(out%i1(size(b%i2,2))); out%i1 = matmul(a%i1, b%i2)
   else
      ok = .false.; msg = "matmul shapes not supported"
   end if
end if
end subroutine apply_matmul

!=========================== builtins ============================

function lower(s) result(t)
character(len=*), intent(in) :: s
character(len=:), allocatable :: t
integer :: i, c
character(len=len_trim(s)) :: buf
do i = 1, len_trim(s)
   c = iachar(s(i:i))
   if (c >= iachar("A") .and. c <= iachar("Z")) then
      buf(i:i) = achar(c + 32)
   else
      buf(i:i) = s(i:i)
   end if
end do
t = trim(buf)
end function lower

subroutine call_builtin(name, args, out, ok, msg)
use numpy_basic_mod, only: zeros, ones, full, empty, arange, linspace, eye, identity, diag, reshape_1d_to_2d, reshape_c_1d_to_2d, ravel_f, ravel_c, transpose2, concatenate, vstack, hstack, sum1, mean1, std1, var1, min1, max1, argmin1, argmax1, linalg_solve, linalg_inv
use numpy_advanced_mod, only: svd, eig, eigh_real, qr_real, cholesky_real, lstsq, pinv, det_slogdet
character(len=*), intent(in) :: name
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
character(len=:), allocatable :: nm
nm = lower(trim(name))
ok = .true.
msg = ""
out%tag = vt_none

select case (nm)
case ("zeros")
   call builtin_zeros(args, out, ok, msg)
case ("zeros_i")
   call builtin_zeros_i(args, out, ok, msg)
case ("ones")
   call builtin_ones(args, out, ok, msg)
case ("ones_i")
   call builtin_ones_i(args, out, ok, msg)
case ("full")
   call builtin_full(args, out, ok, msg)
case ("full_i")
   call builtin_full_i(args, out, ok, msg)
case ("empty")
   call builtin_empty(args, out, ok, msg)
case ("arange")
   call builtin_arange(args, out, ok, msg)
case ("linspace")
   call builtin_linspace(args, out, ok, msg)
case ("eye")
   call builtin_eye(args, out, ok, msg)
case ("identity")
   call builtin_identity(args, out, ok, msg)
case ("diag")
   call builtin_diag(args, out, ok, msg)
case ("reshape")
   call builtin_reshape(args, out, ok, msg, .false.)
case ("reshape_c")
   call builtin_reshape(args, out, ok, msg, .true.)
case ("ravel")
   call builtin_ravel(args, out, ok, msg, .false.)
case ("ravel_c")
   call builtin_ravel(args, out, ok, msg, .true.)
case ("transpose")
   call builtin_transpose(args(1), out, ok, msg)
case ("concatenate")
   call builtin_concatenate(args, out, ok, msg)
case ("vstack")
   call builtin_vstack(args, out, ok, msg)
case ("hstack")
   call builtin_hstack(args, out, ok, msg)
case ("sum")
   call builtin_sum(args, out, ok, msg)
case ("mean")
   call builtin_mean(args, out, ok, msg)
case ("std")
   call builtin_std(args, out, ok, msg)
case ("var")
   call builtin_var(args, out, ok, msg)
case ("min")
   call builtin_min(args, out, ok, msg)
case ("max")
   call builtin_max(args, out, ok, msg)
case ("argmin")
   call builtin_argmin(args, out, ok, msg)
case ("argmax")
   call builtin_argmax(args, out, ok, msg)
case ("dot")
   call builtin_dot(args, out, ok, msg)
case ("matmul")
   call builtin_matmul(args, out, ok, msg)
case ("solve")
   call builtin_solve(args, out, ok, msg)
case ("inv")
   call builtin_inv(args, out, ok, msg)
case ("svd")
   call builtin_svd(args, out, ok, msg)
case ("qr")
   call builtin_qr(args, out, ok, msg)
case ("cholesky")
   call builtin_cholesky(args, out, ok, msg)
case ("lstsq")
   call builtin_lstsq(args, out, ok, msg)
case ("pinv")
   call builtin_pinv(args, out, ok, msg)
case ("det_slogdet")
   call builtin_det_slogdet(args, out, ok, msg)
case ("eigh")
   call builtin_eigh(args, out, ok, msg)
case ("eig")
   call builtin_eig(args, out, ok, msg)
case ("shape")
   call builtin_shape(args, out, ok, msg)
case default
   ok = .false.
   msg = "unknown function: "//trim(name)
end select
end subroutine call_builtin

subroutine builtin_shape(args, out, ok, msg)
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "shape(x) takes 1 argument"; return
end if
select case (args(1)%tag)
case (vt_i, vt_r, vt_c)
   out%tag = vt_i1; allocate(out%i1(0))
case (vt_i1)
   out%tag = vt_i1; allocate(out%i1(1)); out%i1(1) = size(args(1)%i1)
case (vt_r1)
   out%tag = vt_i1; allocate(out%i1(1)); out%i1(1) = size(args(1)%r1)
case (vt_c1)
   out%tag = vt_i1; allocate(out%i1(1)); out%i1(1) = size(args(1)%c1)
case (vt_i2)
   out%tag = vt_i1; allocate(out%i1(2)); out%i1 = [size(args(1)%i2,1), size(args(1)%i2,2)]
case (vt_r2)
   out%tag = vt_i1; allocate(out%i1(2)); out%i1 = [size(args(1)%r2,1), size(args(1)%r2,2)]
case (vt_c2)
   out%tag = vt_i1; allocate(out%i1(2)); out%i1 = [size(args(1)%c2,1), size(args(1)%c2,2)]
case default
   ok = .false.; msg = "shape not supported"
end select
end subroutine builtin_shape

subroutine builtin_zeros(args, out, ok, msg)
use numpy_basic_mod, only: zeros
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: n1, n2
ok = .true.; msg = ""
if (size(args) == 1 .and. args(1)%tag == vt_i) then
   n1 = args(1)%i
   out%tag = vt_r1
   call zeros(n1, out%r1)
else if (size(args) == 2 .and. args(1)%tag == vt_i .and. args(2)%tag == vt_i) then
   n1 = args(1)%i; n2 = args(2)%i
   out%tag = vt_r2
   call zeros(n1, n2, out%r2)
else
   ok = .false.; msg = "zeros(n) or zeros(n1,n2)"
end if
end subroutine builtin_zeros

subroutine builtin_zeros_i(args, out, ok, msg)
use numpy_basic_mod, only: zeros
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: n1, n2
ok = .true.; msg = ""
if (size(args) == 1 .and. args(1)%tag == vt_i) then
   n1 = args(1)%i
   out%tag = vt_i1
   call zeros(n1, out%i1)
else if (size(args) == 2 .and. args(1)%tag == vt_i .and. args(2)%tag == vt_i) then
   n1 = args(1)%i; n2 = args(2)%i
   out%tag = vt_i2
   call zeros(n1, n2, out%i2)
else
   ok = .false.; msg = "zeros_i(n) or zeros_i(n1,n2)"
end if
end subroutine builtin_zeros_i

subroutine builtin_ones(args, out, ok, msg)
use numpy_basic_mod, only: ones
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: n1, n2
ok = .true.; msg = ""
if (size(args) == 1 .and. args(1)%tag == vt_i) then
   n1 = args(1)%i
   out%tag = vt_r1
   call ones(n1, out%r1)
else if (size(args) == 2 .and. args(1)%tag == vt_i .and. args(2)%tag == vt_i) then
   n1 = args(1)%i; n2 = args(2)%i
   out%tag = vt_r2
   call ones(n1, n2, out%r2)
else
   ok = .false.; msg = "ones(n) or ones(n1,n2)"
end if
end subroutine builtin_ones

subroutine builtin_ones_i(args, out, ok, msg)
use numpy_basic_mod, only: ones
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: n1, n2
ok = .true.; msg = ""
if (size(args) == 1 .and. args(1)%tag == vt_i) then
   n1 = args(1)%i
   out%tag = vt_i1
   call ones(n1, out%i1)
else if (size(args) == 2 .and. args(1)%tag == vt_i .and. args(2)%tag == vt_i) then
   n1 = args(1)%i; n2 = args(2)%i
   out%tag = vt_i2
   call ones(n1, n2, out%i2)
else
   ok = .false.; msg = "ones_i(n) or ones_i(n1,n2)"
end if
end subroutine builtin_ones_i

subroutine builtin_full(args, out, ok, msg)
use numpy_basic_mod, only: full
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: n1, n2
real(kind=dp) :: val
ok = .true.; msg = ""
if (size(args) == 2 .and. args(1)%tag == vt_i .and. (args(2)%tag == vt_i .or. args(2)%tag == vt_r)) then
   n1 = args(1)%i
   if (args(2)%tag == vt_r) val = args(2)%r
   if (args(2)%tag == vt_i) val = real(args(2)%i, kind=dp)
   out%tag = vt_r1
   call full(n1, val, out%r1)
else if (size(args) == 3 .and. args(1)%tag == vt_i .and. args(2)%tag == vt_i .and. (args(3)%tag == vt_i .or. args(3)%tag == vt_r)) then
   n1 = args(1)%i; n2 = args(2)%i
   if (args(3)%tag == vt_r) val = args(3)%r
   if (args(3)%tag == vt_i) val = real(args(3)%i, kind=dp)
   out%tag = vt_r2
   call full(n1, n2, val, out%r2)
else
   ok = .false.; msg = "full(n,val) or full(n1,n2,val)"
end if
end subroutine builtin_full

subroutine builtin_full_i(args, out, ok, msg)
use numpy_basic_mod, only: full
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: n1, n2, val
ok = .true.; msg = ""
if (size(args) == 2 .and. args(1)%tag == vt_i .and. args(2)%tag == vt_i) then
   n1 = args(1)%i; val = args(2)%i
   out%tag = vt_i1
   call full(n1, val, out%i1)
else if (size(args) == 3 .and. args(1)%tag == vt_i .and. args(2)%tag == vt_i .and. args(3)%tag == vt_i) then
   n1 = args(1)%i; n2 = args(2)%i; val = args(3)%i
   out%tag = vt_i2
   call full(n1, n2, val, out%i2)
else
   ok = .false.; msg = "full_i(n,val) or full_i(n1,n2,val)"
end if
end subroutine builtin_full_i

subroutine builtin_empty(args, out, ok, msg)
use numpy_basic_mod, only: empty
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: n1, n2
ok = .true.; msg = ""
if (size(args) == 1 .and. args(1)%tag == vt_i) then
   n1 = args(1)%i
   out%tag = vt_r1
   call empty(n1, out%r1)
else if (size(args) == 2 .and. args(1)%tag == vt_i .and. args(2)%tag == vt_i) then
   n1 = args(1)%i; n2 = args(2)%i
   out%tag = vt_r2
   call empty(n1, n2, out%r2)
else
   ok = .false.; msg = "empty(n) or empty(n1,n2)"
end if
end subroutine builtin_empty

real(kind=dp) function get_real_arg(v) result(r)
type(value_t), intent(in) :: v
if (v%tag == vt_r) then
   r = v%r
else
   r = real(v%i, kind=dp)
end if
end function get_real_arg

subroutine builtin_arange(args, out, ok, msg)
use numpy_basic_mod, only: arange
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: starti, stopi, stepi
real(kind=dp) :: startr, stopr, stepr
logical :: any_real
integer :: i
ok = .true.; msg = ""

if (size(args) < 1 .or. size(args) > 3) then
   ok = .false.; msg = "arange(stop) or arange(start,stop[,step])"; return
end if

any_real = .false.
do i = 1, size(args)
   if (args(i)%tag == vt_r) any_real = .true.
end do

if (size(args) == 1) then
   if (args(1)%tag == vt_i) then
      out%tag = vt_i1
      call arange(args(1)%i, out%i1)
   else
      out%tag = vt_r1
      call arange(int(args(1)%r), out%r1)
   end if
   return
end if

if (any_real) then
   startr = get_real_arg(args(1))
   stopr = get_real_arg(args(2))
   if (size(args) == 3) then
      stepr = get_real_arg(args(3))
   else
      stepr = 1.0_dp
   end if
   out%tag = vt_r1
   call arange(startr, stopr, stepr, out%r1)
else
   starti = args(1)%i
   stopi = args(2)%i
   if (size(args) == 3) then
      stepi = args(3)%i
   else
      stepi = 1
   end if
   out%tag = vt_i1
   call arange(starti, stopi, stepi, out%i1)
end if
end subroutine builtin_arange

subroutine builtin_linspace(args, out, ok, msg)
use numpy_basic_mod, only: linspace
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp) :: a, b
integer :: n
ok = .true.; msg = ""
if (size(args) /= 3) then
   ok = .false.; msg = "linspace(start,stop,num)"; return
end if
a = get_real_arg(args(1))
b = get_real_arg(args(2))
if (args(3)%tag /= vt_i) then
   ok = .false.; msg = "num must be integer"; return
end if
n = args(3)%i
out%tag = vt_r1
call linspace(a, b, n, out%r1)
end subroutine builtin_linspace

subroutine builtin_eye(args, out, ok, msg)
use numpy_basic_mod, only: eye
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: n, m
ok = .true.; msg = ""
if (size(args) == 1 .and. args(1)%tag == vt_i) then
   n = args(1)%i
   out%tag = vt_r2
   call eye(n, n, out%r2)
else if (size(args) == 2 .and. args(1)%tag == vt_i .and. args(2)%tag == vt_i) then
   n = args(1)%i; m = args(2)%i
   out%tag = vt_r2
   call eye(n, m, out%r2)
else
   ok = .false.; msg = "eye(n) or eye(n,m)"
end if
end subroutine builtin_eye

subroutine builtin_identity(args, out, ok, msg)
use numpy_basic_mod, only: identity
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: n
ok = .true.; msg = ""
if (size(args) /= 1 .or. args(1)%tag /= vt_i) then
   ok = .false.; msg = "identity(n)"; return
end if
n = args(1)%i
out%tag = vt_r2
call identity(n, out%r2)
end subroutine builtin_identity

subroutine builtin_diag(args, out, ok, msg)
use numpy_basic_mod, only: diag
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "diag(x)"; return
end if
select case (args(1)%tag)
case (vt_i1)
   out%tag = vt_i2
   call diag(args(1)%i1, out%i2)
case (vt_r1)
   out%tag = vt_r2
   call diag(args(1)%r1, out%r2)
case (vt_i2)
   out%tag = vt_i1
   call diag(args(1)%i2, out%i1)
case (vt_r2)
   out%tag = vt_r1
   call diag(args(1)%r2, out%r1)
case default
   ok = .false.; msg = "diag expects 1d or 2d int/real"
end select
end subroutine builtin_diag

subroutine builtin_reshape(args, out, ok, msg, corder)
use numpy_basic_mod, only: reshape_1d_to_2d, reshape_c_1d_to_2d
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
logical, intent(in) :: corder
integer :: n1, n2
ok = .true.; msg = ""
if (size(args) /= 3) then
   ok = .false.; msg = "reshape(x,n1,n2)"; return
end if
if (args(2)%tag /= vt_i .or. args(3)%tag /= vt_i) then
   ok = .false.; msg = "reshape dims must be integers"; return
end if
n1 = args(2)%i; n2 = args(3)%i
select case (args(1)%tag)
case (vt_i1)
   out%tag = vt_i2
   if (corder) then
      call reshape_c_1d_to_2d(args(1)%i1, n1, n2, out%i2)
   else
      call reshape_1d_to_2d(args(1)%i1, n1, n2, out%i2)
   end if
case (vt_r1)
   out%tag = vt_r2
   if (corder) then
      call reshape_c_1d_to_2d(args(1)%r1, n1, n2, out%r2)
   else
      call reshape_1d_to_2d(args(1)%r1, n1, n2, out%r2)
   end if
case default
   ok = .false.; msg = "reshape expects 1d int/real"
end select
end subroutine builtin_reshape

subroutine builtin_ravel(args, out, ok, msg, corder)
use numpy_basic_mod, only: ravel_f, ravel_c
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
logical, intent(in) :: corder
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "ravel(a)"; return
end if
select case (args(1)%tag)
case (vt_i2)
   out%tag = vt_i1
   if (corder) then
      call ravel_c(args(1)%i2, out%i1)
   else
      call ravel_f(args(1)%i2, out%i1)
   end if
case (vt_r2)
   out%tag = vt_r1
   if (corder) then
      call ravel_c(args(1)%r2, out%r1)
   else
      call ravel_f(args(1)%r2, out%r1)
   end if
case default
   ok = .false.; msg = "ravel expects 2d int/real"
end select
end subroutine builtin_ravel

subroutine builtin_transpose(a, out, ok, msg)
use numpy_basic_mod, only: transpose2
type(value_t), intent(in) :: a
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.; msg = ""
select case (a%tag)
case (vt_i2)
   out%tag = vt_i2
   call transpose2(a%i2, out%i2)
case (vt_r2)
   out%tag = vt_r2
   call transpose2(a%r2, out%r2)
case default
   ok = .false.; msg = "transpose expects 2d int/real"
end select
end subroutine builtin_transpose

subroutine builtin_concatenate(args, out, ok, msg)
use numpy_basic_mod, only: concatenate
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.; msg = ""
if (size(args) /= 2) then
   ok = .false.; msg = "concatenate(a,b)"; return
end if
if (args(1)%tag == vt_i1 .and. args(2)%tag == vt_i1) then
   out%tag = vt_i1
   call concatenate(args(1)%i1, args(2)%i1, out%i1)
else if (args(1)%tag == vt_r1 .and. args(2)%tag == vt_r1) then
   out%tag = vt_r1
   call concatenate(args(1)%r1, args(2)%r1, out%r1)
else
   ok = .false.; msg = "concatenate requires both 1d same type"
end if
end subroutine builtin_concatenate

subroutine builtin_vstack(args, out, ok, msg)
use numpy_basic_mod, only: vstack
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.; msg = ""
if (size(args) /= 2) then
   ok = .false.; msg = "vstack(a,b)"; return
end if
if (args(1)%tag == vt_i2 .and. args(2)%tag == vt_i2) then
   out%tag = vt_i2
   call vstack(args(1)%i2, args(2)%i2, out%i2)
else if (args(1)%tag == vt_r2 .and. args(2)%tag == vt_r2) then
   out%tag = vt_r2
   call vstack(args(1)%r2, args(2)%r2, out%r2)
else
   ok = .false.; msg = "vstack requires both 2d same type"
end if
end subroutine builtin_vstack

subroutine builtin_hstack(args, out, ok, msg)
use numpy_basic_mod, only: hstack
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.; msg = ""
if (size(args) /= 2) then
   ok = .false.; msg = "hstack(a,b)"; return
end if
if (args(1)%tag == vt_i2 .and. args(2)%tag == vt_i2) then
   out%tag = vt_i2
   call hstack(args(1)%i2, args(2)%i2, out%i2)
else if (args(1)%tag == vt_r2 .and. args(2)%tag == vt_r2) then
   out%tag = vt_r2
   call hstack(args(1)%r2, args(2)%r2, out%r2)
else
   ok = .false.; msg = "hstack requires both 2d same type"
end if
end subroutine builtin_hstack

subroutine builtin_sum(args, out, ok, msg)
use numpy_basic_mod, only: sum1
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: x(:)
real(kind=dp) :: s
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "sum(x)"; return
end if
call flatten_to_real1(args(1), x, ok, msg); if (.not. ok) return
call sum1(x, s)
out%tag = vt_r; out%r = s
deallocate(x)
end subroutine builtin_sum

subroutine builtin_mean(args, out, ok, msg)
use numpy_basic_mod, only: mean1
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: x(:)
real(kind=dp) :: m
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "mean(x)"; return
end if
call flatten_to_real1(args(1), x, ok, msg); if (.not. ok) return
call mean1(x, m)
out%tag = vt_r; out%r = m
deallocate(x)
end subroutine builtin_mean

subroutine builtin_std(args, out, ok, msg)
use numpy_basic_mod, only: std1
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: x(:)
real(kind=dp) :: s
integer :: ddof
ok = .true.; msg = ""
ddof = 0
if (size(args) < 1 .or. size(args) > 2) then
   ok = .false.; msg = "std(x[,ddof])"; return
end if
if (size(args) == 2) then
   if (args(2)%tag /= vt_i) then
      ok = .false.; msg = "ddof must be integer"; return
   end if
   ddof = args(2)%i
end if
call flatten_to_real1(args(1), x, ok, msg); if (.not. ok) return
call std1(x, ddof, s)
out%tag = vt_r; out%r = s
deallocate(x)
end subroutine builtin_std

subroutine builtin_var(args, out, ok, msg)
use numpy_basic_mod, only: var1
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: x(:)
real(kind=dp) :: v
integer :: ddof
ok = .true.; msg = ""
ddof = 0
if (size(args) < 1 .or. size(args) > 2) then
   ok = .false.; msg = "var(x[,ddof])"; return
end if
if (size(args) == 2) then
   if (args(2)%tag /= vt_i) then
      ok = .false.; msg = "ddof must be integer"; return
   end if
   ddof = args(2)%i
end if
call flatten_to_real1(args(1), x, ok, msg); if (.not. ok) return
call var1(x, ddof, v)
out%tag = vt_r; out%r = v
deallocate(x)
end subroutine builtin_var

subroutine builtin_min(args, out, ok, msg)
use numpy_basic_mod, only: min1
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: x(:)
real(kind=dp) :: v
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "min(x)"; return
end if
call flatten_to_real1(args(1), x, ok, msg); if (.not. ok) return
call min1(x, v)
out%tag = vt_r; out%r = v
deallocate(x)
end subroutine builtin_min

subroutine builtin_max(args, out, ok, msg)
use numpy_basic_mod, only: max1
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: x(:)
real(kind=dp) :: v
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "max(x)"; return
end if
call flatten_to_real1(args(1), x, ok, msg); if (.not. ok) return
call max1(x, v)
out%tag = vt_r; out%r = v
deallocate(x)
end subroutine builtin_max

subroutine builtin_argmin(args, out, ok, msg)
use numpy_basic_mod, only: argmin1
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: x(:)
integer :: idx
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "argmin(x)"; return
end if
call flatten_to_real1(args(1), x, ok, msg); if (.not. ok) return
call argmin1(x, idx)
out%tag = vt_i; out%i = idx - 1
deallocate(x)
end subroutine builtin_argmin

subroutine builtin_argmax(args, out, ok, msg)
use numpy_basic_mod, only: argmax1
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: x(:)
integer :: idx
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "argmax(x)"; return
end if
call flatten_to_real1(args(1), x, ok, msg); if (.not. ok) return
call argmax1(x, idx)
out%tag = vt_i; out%i = idx - 1
deallocate(x)
end subroutine builtin_argmax

subroutine builtin_dot(args, out, ok, msg)
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.; msg = ""
if (size(args) /= 2) then
   ok = .false.; msg = "dot(x,y)"; return
end if
call apply_matmul(args(1), args(2), out, ok, msg)
end subroutine builtin_dot

subroutine builtin_matmul(args, out, ok, msg)
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
ok = .true.; msg = ""
if (size(args) /= 2) then
   ok = .false.; msg = "matmul(a,b)"; return
end if
call apply_matmul(args(1), args(2), out, ok, msg)
end subroutine builtin_matmul

subroutine builtin_solve(args, out, ok, msg)
use numpy_basic_mod, only: linalg_solve
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: info
ok = .true.; msg = ""
if (size(args) /= 2) then
   ok = .false.; msg = "solve(a,b)"; return
end if
if (args(1)%tag /= vt_r2 .or. args(2)%tag /= vt_r1) then
   ok = .false.; msg = "solve expects (real matrix, real vector)"; return
end if
out%tag = vt_r1
call linalg_solve(args(1)%r2, args(2)%r1, out%r1, info)
if (info /= 0) then
   ok = .false.; msg = "solve failed"
end if
end subroutine builtin_solve

subroutine builtin_inv(args, out, ok, msg)
use numpy_basic_mod, only: linalg_inv
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
integer :: info
ok = .true.; msg = ""
if (size(args) /= 1 .or. args(1)%tag /= vt_r2) then
   ok = .false.; msg = "inv(a) expects real matrix"; return
end if
out%tag = vt_r2
call linalg_inv(args(1)%r2, out%r2, info)
if (info /= 0) then
   ok = .false.; msg = "inv failed"
end if
end subroutine builtin_inv

subroutine builtin_svd(args, out, ok, msg)
use numpy_advanced_mod, only: svd
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: u(:,:), s(:), vt(:,:)
integer :: info
type(value_t) :: v1, v2, v3
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "svd(a)"; return
end if
if (args(1)%tag == vt_r2) then
   call svd(args(1)%r2, u, s, vt, info)
else if (args(1)%tag == vt_i2) then
   call svd(args(1)%i2, u, s, vt, info)
else
   ok = .false.; msg = "svd expects 2d int/real"; return
end if
if (info /= 0) then
   ok = .false.; msg = "svd failed"; return
end if
v1%tag = vt_r2; allocate(v1%r2(size(u,1), size(u,2))); v1%r2 = u
v2%tag = vt_r1; allocate(v2%r1(size(s))); v2%r1 = s
v3%tag = vt_r2; allocate(v3%r2(size(vt,1), size(vt,2))); v3%r2 = vt
out%tag = vt_list
allocate(out%list(3))
out%list(1) = v1
out%list(2) = v2
out%list(3) = v3
deallocate(u, s, vt)
end subroutine builtin_svd

subroutine builtin_qr(args, out, ok, msg)
use numpy_advanced_mod, only: qr_real
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: q(:,:), r(:,:)
integer :: info
type(value_t) :: v1, v2
ok = .true.; msg = ""
if (size(args) /= 1 .or. args(1)%tag /= vt_r2) then
   ok = .false.; msg = "qr(a) expects real matrix"; return
end if
call qr_real(args(1)%r2, q, r, info)
if (info /= 0) then
   ok = .false.; msg = "qr failed"; return
end if
v1%tag = vt_r2; allocate(v1%r2(size(q,1), size(q,2))); v1%r2 = q
v2%tag = vt_r2; allocate(v2%r2(size(r,1), size(r,2))); v2%r2 = r
out%tag = vt_list
allocate(out%list(2))
out%list(1) = v1
out%list(2) = v2
deallocate(q, r)
end subroutine builtin_qr

subroutine builtin_cholesky(args, out, ok, msg)
use numpy_advanced_mod, only: cholesky_real
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: l(:,:)
integer :: info
ok = .true.; msg = ""
if (size(args) /= 1 .or. args(1)%tag /= vt_r2) then
   ok = .false.; msg = "cholesky(a) expects real matrix"; return
end if
call cholesky_real(args(1)%r2, l, info)
if (info /= 0) then
   ok = .false.; msg = "cholesky failed"; return
end if
out%tag = vt_r2; allocate(out%r2(size(l,1), size(l,2))); out%r2 = l
deallocate(l)
end subroutine builtin_cholesky

subroutine builtin_lstsq(args, out, ok, msg)
use numpy_advanced_mod, only: lstsq
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: x(:)
real(kind=dp) :: resn2
integer :: info
type(value_t) :: v1, v2
ok = .true.; msg = ""
if (size(args) /= 2 .or. args(1)%tag /= vt_r2 .or. args(2)%tag /= vt_r1) then
   ok = .false.; msg = "lstsq(a,b) expects (real matrix, real vector)"; return
end if
call lstsq(args(1)%r2, args(2)%r1, x, resn2, info)
if (info /= 0) then
   ok = .false.; msg = "lstsq failed"; return
end if
v1%tag = vt_r1; allocate(v1%r1(size(x))); v1%r1 = x
v2%tag = vt_r; v2%r = resn2
out%tag = vt_list; allocate(out%list(2)); out%list(1) = v1; out%list(2) = v2
deallocate(x)
end subroutine builtin_lstsq

subroutine builtin_pinv(args, out, ok, msg)
use numpy_advanced_mod, only: pinv
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: ap(:,:)
integer :: info
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "pinv(a)"; return
end if
if (args(1)%tag == vt_r2) then
   call pinv(args(1)%r2, ap, info)
else if (args(1)%tag == vt_i2) then
   call pinv(args(1)%i2, ap, info)
else
   ok = .false.; msg = "pinv expects 2d int/real"; return
end if
if (info /= 0) then
   ok = .false.; msg = "pinv failed"; return
end if
out%tag = vt_r2; allocate(out%r2(size(ap,1), size(ap,2))); out%r2 = ap
deallocate(ap)
end subroutine builtin_pinv

subroutine builtin_det_slogdet(args, out, ok, msg)
use numpy_advanced_mod, only: det_slogdet
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp) :: det, sign, lad
integer :: info
type(value_t) :: v1, v2, v3
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "det_slogdet(a)"; return
end if
if (args(1)%tag == vt_r2) then
   call det_slogdet(args(1)%r2, det, sign, lad, info)
else if (args(1)%tag == vt_i2) then
   call det_slogdet(args(1)%i2, det, sign, lad, info)
else
   ok = .false.; msg = "det_slogdet expects 2d int/real"; return
end if
if (info /= 0) then
   ok = .false.; msg = "det_slogdet failed"; return
end if
v1%tag = vt_r; v1%r = det
v2%tag = vt_r; v2%r = sign
v3%tag = vt_r; v3%r = lad
out%tag = vt_list
allocate(out%list(3))
out%list(1) = v1
out%list(2) = v2
out%list(3) = v3
end subroutine builtin_det_slogdet

subroutine builtin_eigh(args, out, ok, msg)
use numpy_advanced_mod, only: eigh_real
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
real(kind=dp), allocatable :: w(:), z(:,:)
integer :: info
type(value_t) :: v1, v2
ok = .true.; msg = ""
if (size(args) /= 1 .or. args(1)%tag /= vt_r2) then
   ok = .false.; msg = "eigh(a) expects real matrix"; return
end if
call eigh_real(args(1)%r2, w, z, info)
if (info /= 0) then
   ok = .false.; msg = "eigh failed"; return
end if
v1%tag = vt_r1; allocate(v1%r1(size(w))); v1%r1 = w
v2%tag = vt_r2; allocate(v2%r2(size(z,1), size(z,2))); v2%r2 = z
out%tag = vt_list; allocate(out%list(2)); out%list(1) = v1; out%list(2) = v2
deallocate(w, z)
end subroutine builtin_eigh

subroutine builtin_eig(args, out, ok, msg)
use numpy_advanced_mod, only: eig
type(value_t), intent(in) :: args(:)
type(value_t), intent(out) :: out
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
complex(kind=dp), allocatable :: w(:)
real(kind=dp), allocatable :: vr(:,:)
integer :: info
type(value_t) :: v1, v2
ok = .true.; msg = ""
if (size(args) /= 1) then
   ok = .false.; msg = "eig(a)"; return
end if
if (args(1)%tag == vt_r2) then
   call eig(args(1)%r2, w, vr, info)
else if (args(1)%tag == vt_i2) then
   call eig(args(1)%i2, w, vr, info)
else
   ok = .false.; msg = "eig expects 2d int/real"; return
end if
if (info /= 0) then
   ok = .false.; msg = "eig failed"; return
end if
v1%tag = vt_c1; allocate(v1%c1(size(w))); v1%c1 = w
v2%tag = vt_r2; allocate(v2%r2(size(vr,1), size(vr,2))); v2%r2 = vr
out%tag = vt_list; allocate(out%list(2)); out%list(1) = v1; out%list(2) = v2
deallocate(w, vr)
end subroutine builtin_eig

subroutine flatten_to_real1(a, x, ok, msg)
type(value_t), intent(in) :: a
real(kind=dp), allocatable, intent(out) :: x(:)
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
type(value_t) :: ar
ok = .true.; msg = ""
call to_real(a, ar, ok, msg)
if (.not. ok) return
select case (ar%tag)
case (vt_r)
   allocate(x(1)); x(1) = ar%r
case (vt_r1)
   allocate(x(size(ar%r1))); x = ar%r1
case (vt_r2)
   allocate(x(size(ar%r2)))
   x = reshape(ar%r2, [size(ar%r2)])
case default
   ok = .false.; msg = "cannot flatten"
end select
call value_free(ar)
end subroutine flatten_to_real1

!=========================== printing ============================

recursive subroutine print_value(v, prefix)
type(value_t), intent(in) :: v
character(len=*), intent(in) :: prefix
integer :: i
character(len=256) :: buf
select case (v%tag)
case (vt_none)
   if (len_trim(prefix) > 0) print *, trim(prefix)
case (vt_i)
   if (len_trim(prefix) > 0) then
      write(buf,'(A,I0)') trim(prefix), v%i
      print *, trim(buf)
   else
      print *, v%i
   end if
case (vt_r)
   if (len_trim(prefix) > 0) then
      write(buf,'(A,ES24.16)') trim(prefix), v%r
      print *, trim(buf)
   else
      print *, v%r
   end if
case (vt_c)
   if (len_trim(prefix) > 0) then
      write(buf,'(A,"(",ES16.8,",",ES16.8,")")') trim(prefix), real(v%c,kind=dp), aimag(v%c)
      print *, trim(buf)
   else
      print *, v%c
   end if
case (vt_i1)
   call print_vec_int(v%i1, prefix)
case (vt_r1)
   call print_vec_real(v%r1, prefix)
case (vt_c1)
   call print_vec_cplx(v%c1, prefix)
case (vt_i2)
   call print_mat_int(v%i2, prefix)
case (vt_r2)
   call print_mat_real(v%r2, prefix)
case (vt_c2)
   call print_mat_cplx(v%c2, prefix)
case (vt_list)
   if (len_trim(prefix) > 0) print *, trim(prefix)
   do i = 1, size(v%list)
      write(buf,'(" item ",I0,":")') i
      print *, trim(buf)
      call print_value(v%list(i), "")
   end do
case default
   print *, "<?>"
end select
end subroutine print_value

subroutine print_vec_int(x, prefix)
integer, intent(in) :: x(:)
character(len=*), intent(in) :: prefix
integer :: i
character(len=1024) :: line
line = "["
do i = 1, size(x)
   line = trim(line)//trim(itoa(x(i)))
   if (i < size(x)) line = trim(line)//" "
end do
line = trim(line)//"]"
if (len_trim(prefix) > 0) then
   print *, trim(prefix)//trim(line)
else
   print *, trim(line)
end if
end subroutine print_vec_int

subroutine print_vec_real(x, prefix)
real(kind=dp), intent(in) :: x(:)
character(len=*), intent(in) :: prefix
integer :: i
character(len=1024) :: line
character(len=64) :: buf
line = "["
do i = 1, size(x)
   write(buf,'(ES16.8)') x(i)
   line = trim(line)//trim(buf)
   if (i < size(x)) line = trim(line)//" "
end do
line = trim(line)//"]"
if (len_trim(prefix) > 0) then
   print *, trim(prefix)//trim(line)
else
   print *, trim(line)
end if
end subroutine print_vec_real

subroutine print_vec_cplx(x, prefix)
complex(kind=dp), intent(in) :: x(:)
character(len=*), intent(in) :: prefix
integer :: i
character(len=2048) :: line
character(len=80) :: buf
line = "["
do i = 1, size(x)
   write(buf,'("(",ES12.4,",",ES12.4,")")') real(x(i),kind=dp), aimag(x(i))
   line = trim(line)//trim(buf)
   if (i < size(x)) line = trim(line)//" "
end do
line = trim(line)//"]"
if (len_trim(prefix) > 0) then
   print *, trim(prefix)//trim(line)
else
   print *, trim(line)
end if
end subroutine print_vec_cplx

subroutine print_mat_int(a, prefix)
integer, intent(in) :: a(:,:)
character(len=*), intent(in) :: prefix
integer :: i, j
character(len=2048) :: line
if (len_trim(prefix) > 0) print *, trim(prefix)
print *, "["
do i = 1, size(a,1)
   line = "["
   do j = 1, size(a,2)
      line = trim(line)//trim(itoa(a(i,j)))
      if (j < size(a,2)) line = trim(line)//" "
   end do
   line = trim(line)//"]"
   print *, " "//trim(line)
end do
print *, "]"
end subroutine print_mat_int

subroutine print_mat_real(a, prefix)
real(kind=dp), intent(in) :: a(:,:)
character(len=*), intent(in) :: prefix
integer :: i, j
character(len=4096) :: line
character(len=64) :: buf
if (len_trim(prefix) > 0) print *, trim(prefix)
print *, "["
do i = 1, size(a,1)
   line = "["
   do j = 1, size(a,2)
      write(buf,'(ES16.8)') a(i,j)
      line = trim(line)//trim(buf)
      if (j < size(a,2)) line = trim(line)//" "
   end do
   line = trim(line)//"]"
   print *, " "//trim(line)
end do
print *, "]"
end subroutine print_mat_real

subroutine print_mat_cplx(a, prefix)
complex(kind=dp), intent(in) :: a(:,:)
character(len=*), intent(in) :: prefix
integer :: i, j
character(len=4096) :: line
character(len=80) :: buf
if (len_trim(prefix) > 0) print *, trim(prefix)
print *, "["
do i = 1, size(a,1)
   line = "["
   do j = 1, size(a,2)
      write(buf,'("(",ES12.4,",",ES12.4,")")') real(a(i,j),kind=dp), aimag(a(i,j))
      line = trim(line)//trim(buf)
      if (j < size(a,2)) line = trim(line)//" "
   end do
   line = trim(line)//"]"
   print *, " "//trim(line)
end do
print *, "]"
end subroutine print_mat_cplx

!=========================== top-level interpret ============================

subroutine interpret_line(line, env, ok, msg)
character(len=*), intent(in) :: line
type(env_t), intent(inout) :: env
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

character(len=:), allocatable :: s, lhs, rhs
integer :: peq, nlh, i
character(len=name_len), allocatable :: names(:)
type(value_t) :: v
type(parser_t) :: p
type(token_t), allocatable :: toks(:)

ok = .true.
msg = ""
s = trim(line)
if (len_trim(s) == 0) return

peq = find_top_level_eq(s)
if (peq > 0) then
   lhs = adjustl(s(1:peq-1))
   rhs = adjustl(s(peq+1:))
   call split_names(lhs, names, nlh, ok, msg)
   if (.not. ok) return

   call tokenize(rhs, toks, ok, msg)
   if (.not. ok) return
   p%tok = toks
   p%pos = 1

   call parse_expr(p, env, v, ok, msg)
   if (.not. ok) return

   if (nlh == 1) then
      call env_set(env, names(1), v)
      call print_value(v, trim(names(1))//" = ")
   else
      if (v%tag /= vt_list) then
         ok = .false.; msg = "multiple assignment requires list return"; call value_free(v); return
      end if
      if (size(v%list) /= nlh) then
         ok = .false.; msg = "lhs/rhs length mismatch"; call value_free(v); return
      end if
      do i = 1, nlh
         call env_set(env, names(i), v%list(i))
         call print_value(v%list(i), trim(names(i))//" = ")
      end do
      call value_free(v)
   end if

   deallocate(toks)
   deallocate(names)

else
   call tokenize(s, toks, ok, msg)
   if (.not. ok) return
   p%tok = toks
   p%pos = 1
   call parse_expr(p, env, v, ok, msg)
   if (.not. ok) return
   call print_value(v, "")
   call value_free(v)
   deallocate(toks)
end if
end subroutine interpret_line

integer function find_top_level_eq(s) result(pos)
character(len=*), intent(in) :: s
integer :: i, n, lvlp, lvlb
n = len_trim(s)
lvlp = 0
lvlb = 0
pos = 0
do i = 1, n
   select case (s(i:i))
   case ("("); lvlp = lvlp + 1
   case (")"); if (lvlp > 0) lvlp = lvlp - 1
   case ("["); lvlb = lvlb + 1
   case ("]"); if (lvlb > 0) lvlb = lvlb - 1
   case ("=")
      if (lvlp == 0 .and. lvlb == 0) then
         if (i < n) then
            if (s(i+1:i+1) == "=") cycle
         end if
         if (i > 1) then
            if (s(i-1:i-1) == "=") cycle
         end if
         pos = i
         return
      end if
   end select
end do
end function find_top_level_eq

subroutine split_names(lhs, names, n, ok, msg)
character(len=*), intent(in) :: lhs
character(len=name_len), allocatable, intent(out) :: names(:)
integer, intent(out) :: n
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg

integer :: i, j
character(len=:), allocatable :: s, part

ok = .true.
msg = ""
s = trim(lhs)
if (len_trim(s) == 0) then
   ok = .false.; msg = "missing lhs"; return
end if

allocate(names(0))
i = 1
do
   j = index(s(i:), ",")
   if (j == 0) then
      part = trim(s(i:))
      call add_name(part, names, ok, msg)
      exit
   else
      part = trim(s(i:i+j-2))
      call add_name(part, names, ok, msg)
      if (.not. ok) return
      i = i + j
      if (i > len_trim(s)) exit
   end if
end do
n = size(names)
end subroutine split_names

subroutine add_name(part, names, ok, msg)
character(len=*), intent(in) :: part
character(len=name_len), allocatable, intent(inout) :: names(:)
logical, intent(out) :: ok
character(len=:), allocatable, intent(out) :: msg
character(len=:), allocatable :: nm
character(len=name_len), allocatable :: n2(:)
integer :: n, i

ok = .true.
msg = ""
nm = trim(part)
if (.not. valid_ident(nm)) then
   ok = .false.; msg = "invalid variable name: "//nm; return
end if
n = size(names)
allocate(n2(n+1))
do i = 1, n
   n2(i) = names(i)
end do
n2(n+1) = nm
deallocate(names)
call move_alloc(n2, names)
end subroutine add_name

logical function valid_ident(s) result(b)
character(len=*), intent(in) :: s
integer :: i
if (len_trim(s) == 0) then
   b = .false.; return
end if
if (.not. (is_alpha(s(1:1)) .or. s(1:1) == "_")) then
   b = .false.; return
end if
do i = 2, len_trim(s)
   if (.not. (is_alpha(s(i:i)) .or. is_digit(s(i:i)) .or. s(i:i) == "_")) then
      b = .false.; return
   end if
end do
b = .true.
end function valid_ident

end module numpy_repl_mod
