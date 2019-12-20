#include <Python.h>


void blas_fct(void);


static PyObject* py_main(PyObject* self, PyObject *args, PyObject *kwargs)
{
	int argc_1;
	float argc_2;
	static char *kwlist[] = {"argA", "argB", NULL};
	
	if(!PyArg_ParseTupleAndKeywords(args, kwargs, "|if", kwlist, &argc_1, &argc_2))
	    return Py_None;
	
	blas_fct();
	
    return Py_None;
}

static PyMethodDef CuBLASMethods[] = {
    { "blas_function", (PyCFunction)py_main, METH_VARARGS | METH_KEYWORDS, "Informations about the function" },
    { NULL, NULL, 0, NULL }
};


static struct PyModuleDef CuBLAS = {
    PyModuleDef_HEAD_INIT,
    "CuBLAS",
    "Module description",
    -1,
    CuBLASMethods
};


PyMODINIT_FUNC PyInit_CuBLAS(void)
{
	PyObject *m;
	
    m = PyModule_Create(&CuBLAS);
    if (m == NULL)
        return NULL;
	
    return m;
}


