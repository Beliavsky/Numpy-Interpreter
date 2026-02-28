# Numpy-Interpreter
Partial NumPy interpreter coded in Fortran. No loops, conditions, or user-defined functions, just expressions and assignments

Compile with `gfortran lapack_d.f90 numpy_basic_mod.f90 numpy_advanced_mod.f90 numpy_repl_mod.f90 numpy_repl.f90 -o numpy_repl.exe`
