program numpy_repl
use numpy_repl_mod
implicit none

type(env_t) :: env
character(len=4096) :: line
integer :: ios
logical :: ok
character(len=:), allocatable :: msg

call env_init(env)

print *, "numpy-like repl (fortran). type 'exit' or 'quit' to stop."
print *, "indices are 0-based: a[0], a[1,2]. transpose: a.T"
print *, "builtins: zeros, zeros_i, ones, ones_i, full, full_i, empty"
print *, "         arange, linspace, eye, identity, diag, reshape, reshape_c, ravel, ravel_c"
print *, "         sum, mean, std, var, min, max, argmin, argmax, dot, matmul, solve, inv"
print *, "advanced: svd, qr, cholesky, lstsq, pinv, det_slogdet, eigh, eig"
print *, "vars prints variable names"

do
   write(*,'(A)', advance='no') ">>> "
   read(*,'(A)', iostat=ios) line
   if (ios /= 0) exit
   if (trim(line) == "exit" .or. trim(line) == "quit") exit
   call interpret_line(line, env, ok, msg)
   if (.not. ok) then
      print *, "error: "//msg
   end if
end do

end program numpy_repl
