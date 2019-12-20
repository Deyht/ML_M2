#include <stdio.h>
#include <stdlib.h>


__global__ void hello_kernel(void)
{
	//int i = threadIdx.x;
	int i = blockIdx.x * blockDim.x + threadIdx.x;
	int b = blockIdx.x;
	
	printf("Hello from block : %d, threadId : %d\n", b, i);
}


int main()
{
	
	hello_kernel<<< 4, 16>>>();
	
	//printf from device are not automatically flushed
	cudaDeviceSynchronize();
	
	exit(EXIT_SUCCESS);
}
