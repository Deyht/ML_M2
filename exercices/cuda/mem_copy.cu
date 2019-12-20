#include <stdio.h>
#include <stdlib.h>


__global__ void add_kernel(int *tab, int size)
{
	//int i = threadIdx.x;
	int i = blockIdx.x * blockDim.x + threadIdx.x;
	
	if(i < size)
	{
		tab[i]*=2;
	}
	
}


int main()
{
	int i;
	int N = 64;
	int *table, *device_table;
	
	table = (int*) malloc(N*sizeof(int));
	
	for(i = 0; i < N; i++)
	{
		table[i] = i;
	}
	
	cudaMalloc(&device_table, N*sizeof(int));
	cudaMemcpy(device_table, table, N*sizeof(int), cudaMemcpyHostToDevice);
	
	add_kernel<<< 1, N>>>(device_table, N);
	
	cudaMemcpy(table, device_table, N*sizeof(int), cudaMemcpyDeviceToHost);
	
	for(i = 0; i < N; i++)
	{
		printf("%d\n",table[i]);
	}
	
	
	free(table);
	cudaFree(device_table);
	
	exit(EXIT_SUCCESS);
}
