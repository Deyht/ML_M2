#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <cblas.h>
#include <cublas_v2.h>

//##########################################################
// In code timer function
//##########################################################


void init_timing(struct timeval* tstart)
{
    gettimeofday(tstart, NULL);
}

float ellapsed_time(struct timeval tstart)
{
    struct timeval tmp;
    long long diff;
    gettimeofday(&tmp, NULL);
    diff = tmp.tv_usec - tstart.tv_usec;
    diff += (tmp.tv_sec - tstart.tv_sec) * 1000000;
    return ((float)diff*1.0e-6);
}


//##########################################################

extern "C"
{
void blas_fct(void);
}

void blas_fct(void)
{
	int i;
	int N = 8192;
	float *A, *B, *C;
	float *d_A, *d_B, *d_C;
	float alpha = 1.0, beta=0.0;
	
	struct timeval timer;
	cublasHandle_t cu_handle;
	
	if(cublasCreate(&cu_handle) != CUBLAS_STATUS_SUCCESS) 
	{
		printf("GPU handle create fail\n");
		exit(EXIT_FAILURE);
	}
	
	
	A = (float*) malloc(N*N*sizeof(float));
	B = (float*) malloc(N*N*sizeof(float));
	C = (float*) malloc(N*N*sizeof(float));
	
	for(i = 0; i < N*N; i++)
	{
		A[i] = (i%50) * 0.1;
		B[i] = (i%25) * 1.3;
		C[i] = 0.0;
	}
	
	cudaMalloc(&d_A, N*N*sizeof(float));
	cudaMalloc(&d_B, N*N*sizeof(float));
	cudaMalloc(&d_C, N*N*sizeof(float));
	
	cudaMemcpy(d_A, A, N*N*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(d_B, B, N*N*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(d_C, C, N*N*sizeof(float), cudaMemcpyHostToDevice);
	
	
	init_timing(&timer);
	
	cblas_sgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, N, N, N, alpha, A, N, B, N, beta, C, N);
	
	printf("CPU time : %f\n", ellapsed_time(timer));
	
	printf("%f\n", C[N*N/2 + N/2]);

	for(i = 0; i < N*N; i++)
	{
		C[i] = 0.0;
	}

	init_timing(&timer);
	
	cublasSgemm(cu_handle, CUBLAS_OP_N, CUBLAS_OP_N, N, N, N, &alpha, d_A, N, d_B, N, &beta, d_C, N);
	cudaDeviceSynchronize();
	printf("CUDA time : %f\n", ellapsed_time(timer));

	cudaMemcpy(C, d_C, N*N*sizeof(float), cudaMemcpyDeviceToHost);
	printf("%f\n", C[N*N/2 + N/2]);
	
	free(A);
	free(B);
	free(C);
}


