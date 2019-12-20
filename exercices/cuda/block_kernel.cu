#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

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

__global__ void loop_workshare_kernel(float *A, float *B, int size, int rad)
{
	//int i = threadIdx.x;
	int i = blockIdx.z * blockDim.z + threadIdx.z;
	int j = blockIdx.y * blockDim.y + threadIdx.y;
	int k = blockIdx.x * blockDim.x + threadIdx.x;
	
	int dx, dy, dz;
	
	float temp_val = 0.0;
	
	if(i >= rad && i < size - rad)
	{
		if(j >= rad && j < size - rad)
		{
			if(k >= rad && k < size - rad)
			{	
				for(dx = -rad; dx <= rad; dx++)
				{
					for(dy = -rad; dy <= rad; dy++)
					{
						for(dz = -rad; dz <= rad; dz++)
						{
								temp_val += A[(i+dx)*size*size + (j+dy)*size + (k+dz)];
						}
					}
				}
				
				B[i*size*size + j*size + k] = temp_val;
			}
		}
	}
	
}


int main()
{
	int i, j, k;
	int N = 512, rad = 2;
	float *A, *B, *device_A, *device_B;
	
	struct timeval timer;
	
	A = (float*) malloc(N*N*N*sizeof(float));
	B = (float*) malloc(N*N*N*sizeof(float));
	
	for(i = 0; i < N; i++)
	{
		for(j = 0; j < N; j++)
		{
			for(k = 0; k < N; k++)
			{
				A[i*N*N + j*N + k] = i + j + k;
				B[i*N*N + j*N + k] = 0.0;
			}
		}
	}
	
	cudaMalloc(&device_A, N*N*N*sizeof(float));
	cudaMalloc(&device_B, N*N*N*sizeof(float));
	cudaMemcpy(device_A, A, N*N*N*sizeof(float), cudaMemcpyHostToDevice);
	cudaMemcpy(device_B, B, N*N*N*sizeof(float), cudaMemcpyHostToDevice);
	
	//WARNNG : dim_a * dim_b * dim_c < 1024
	// AND dim_a < 1024, dimb_b < 1024 & dim_c < 64
	//Must try to mixmise gpu occupancy
	dim3 threadsPerBlock(32,4,2);
	dim3 numBlocks((N + threadsPerBlock.x - 1) / threadsPerBlock.x, 
					(N + threadsPerBlock.y - 1) / threadsPerBlock.y,
		 			(N + threadsPerBlock.z - 1) / threadsPerBlock.z);
	
	init_timing(&timer);
	
	loop_workshare_kernel<<< numBlocks, threadsPerBlock >>>(device_A, device_B, N, rad);
	cudaDeviceSynchronize();
	
	printf("Kernel time : %f\n", ellapsed_time(timer));
	
	cudaMemcpy(B, device_B, N*N*N*sizeof(float), cudaMemcpyDeviceToHost);
	
	printf("%f\n", B[N*N*N/2 + N*N/2 + N/2]);
	
	
	free(A);
	free(B);
	cudaFree(device_A);
	cudaFree(device_B);
	
	exit(EXIT_SUCCESS);
}
