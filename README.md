# Learning Fortran
tasks from ["Einführung in wissenschaftlische Programmierung mit
Fortran"](http://www.math.uni-leipzig.de/~hellmund/fortran.html)

---

## diagonal matrix form stencil
function to get diagonal matrix from stencil  
f2py:

    python3 -m numpy.f2py -c -m stencil_diag_lib stdiag.f90


## sierpinski, fractal (aufg 3)
compile with  

    gfortran -I \path\to\ogpf.mod \path\to\ogpf.o sierpinski.f90
    a.exe
    gnuplot -p ogpf_temp_script.gp


## plotting with fortran
["kookma/ogpf"](https://github.com/kookma/ogpf)
