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

int cu_threads = 128;

__global__ void op_kernel(float *tab, int size)
{
	//int i = threadIdx.x;
	int i = blockIdx.x * blockDim.x + threadIdx.x;
	float prev, next;
	
	if(i < size)
	{
		if(i == 0)
			prev = 0;
		else
			prev = tab[i-1];
			
		if(i == size-1)
			next = 0;
		else
			next = tab[i+1];
		
		tab[i] = tab[i] * prev / next;
	}
	
}


int main()
{
	int i;
	int N = 65536;
	float *table, *device_table;
	float prev, next;
	
	struct timeval timer;
	
	int cu_blocks;
	
	table = (float*) malloc(N*sizeof(float));
	
	for(i = 0; i < N; i++)
	{
		table[i] = i;
	}
	
	cudaMalloc(&device_table, N*sizeof(float));
	cudaMemcpy(device_table, table, N*sizeof(float), cudaMemcpyHostToDevice);
	
	cu_blocks = (N + cu_threads - 1) / cu_threads;
	
	init_timing(&timer);
	
	op_kernel<<< cu_blocks, cu_threads >>>(device_table, N);
	cudaDeviceSynchronize();
	
	printf("Kernel time : %f\n", ellapsed_time(timer));
	
	cudaMemcpy(table, device_table, N*sizeof(float), cudaMemcpyDeviceToHost);
	
	init_timing(&timer);
	
	for(i = 0; i < N; i++)
	{
		if(i == 0)
			prev = 0;
		else
			prev = table[i-1];
			
		if(i == N-1)
			next = 0;
		else
			next = table[i+1];
		
		table[i] = table[i] * prev / next;
	}
	
	printf("CPU time : %f\n", ellapsed_time(timer));
	for(i = 0; i < N; i++)
		printf("%d\n",table[i]);
	
	free(table);
	cudaFree(device_table);
	
	exit(EXIT_SUCCESS);
}
