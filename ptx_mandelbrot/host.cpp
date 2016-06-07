#include <iostream>
#include <fstream>
#include <cassert>
#include "cuda.h"


#define WIDTH 800
#define HEIGHT 640

#define CUDA_CHECK_ERR(func){\
  CUresult err = (func);\
  const char *err_str;\
  if(err!=CUDA_SUCCESS){\
    cuGetErrorString(err, &err_str);\
    printf("%s\n", err_str);\
    return 1;\
  }\
}

void print_result(int *result){
  for(int j = 0; j < HEIGHT; j+=10){
    for(int i = 0; i < WIDTH; i+=5){
      if(result[j * WIDTH + i] == -1)
        printf("#");
      else
        printf("%d", result[j * WIDTH + i] % 9);
    }
    printf("\n");
  }
  return;
}

int main(int argc, char **argv) {
  CUdevice    device;
  CUmodule    cudaModule;
  CUcontext   context;
  CUfunction  function;
  CUlinkState linker;
  int         devCount;

  // CUDA initialization
  CUDA_CHECK_ERR(cuInit(0));
  CUDA_CHECK_ERR(cuDeviceGetCount(&devCount));
  CUDA_CHECK_ERR(cuDeviceGet(&device, 0));

  char name[128];
  CUDA_CHECK_ERR(cuDeviceGetName(name, 128, device));
  std::cout << "Using CUDA Device [0]: " << name << "\n";

  int devMajor, devMinor;
  CUDA_CHECK_ERR(cuDeviceComputeCapability(&devMajor, &devMinor, device));
  std::cout << "Device Compute Capability: "
            << devMajor << "." << devMinor << "\n";
  if (devMajor < 3) {
    std::cerr << "ERROR: Device 0 is not SM 3.0 or greater\n";
    return 1;
  }

  std::ifstream t("obj/kernel.ptx");
  if (!t.is_open()) {
    std::cerr << "kernel.ptx not found\n";
    return 1;
  }
  std::string str((std::istreambuf_iterator<char>(t)),
                    std::istreambuf_iterator<char>());

  // create driver context
  CUDA_CHECK_ERR(cuCtxCreate(&context, 0, device));

  // create module for object
  CUDA_CHECK_ERR(cuModuleLoadDataEx(&cudaModule, str.c_str(), 0, 0, 0));

  // get kernel function
  CUDA_CHECK_ERR(cuModuleGetFunction(&function, cudaModule, "mandelbrot"));

  // device data
  CUdeviceptr devResultBuffer;

  CUDA_CHECK_ERR(cuMemAlloc(&devResultBuffer, sizeof(int)*WIDTH*HEIGHT));

  int hostResult = new int[WIDTH*HEIGHT];

  for (unsigned i = 0; i != WIDTH*HEIGHT; ++i) {
    hostResult[i] = 0;
  }

  CUDA_CHECK_ERR(cuMemcpyHtoD(devResultBuffer, &hostResult[0], sizeof(int)*WIDTH*HEIGHT));


  // size of grid/block/thread
  unsigned blockSizeX = 16;
  unsigned blockSizeY = 16;
  unsigned blockSizeZ = 1;
  unsigned gridSizeX  = 50;
  unsigned gridSizeY  = 40;
  unsigned gridSizeZ  = 1;

  // Kernel parameters
  void *KernelParams[] = { &devResultBuffer };

  // launch kernel code
  CUDA_CHECK_ERR(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ,
                                 blockSizeX, blockSizeY, blockSizeZ,
                                 0, NULL, KernelParams, NULL));

  CUDA_CHECK_ERR(cuMemcpyDtoH(&hostResult[0], devResultBuffer, sizeof(int)*WIDTH*HEIGHT));

  print_result(hostResult);

  // Clean-up
  delete [] hostResult;
  CUDA_CHECK_ERR(cuMemFree(devResultBuffer));
  CUDA_CHECK_ERR(cuModuleUnload(cudaModule));
  CUDA_CHECK_ERR(cuCtxDestroy(context));

  return 0;
}
