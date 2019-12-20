from distutils.core import setup, Extension
setup(name = 'CuBLAS', version = '1.0', ext_modules = [Extension('CuBLAS', ['interface.c'], extra_objects=['blas_interv.o'], extra_link_args=['-I /opt/OpenBLAS/include/', '-L/opt/OpenBLAS/lib/', '-lopenblas', '-L/usr/local/cuda-10.1/lib64', '-lcublas','-lcudart', '-std=c99'])])

