#include <iostream>
#include <fstream>
#include <cassert>
#include "cuda.h"


#define WIDTH 800
#define HEIGHT 640

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

void checkCudaErrors(CUresult err) {
  assert(err == CUDA_SUCCESS);
}

int main(int argc, char **argv) {
  CUdevice    device;
  CUmodule    cudaModule;
  CUcontext   context;
  CUfunction  function;
  CUlinkState linker;
  int         devCount;

  // CUDA initialization
  checkCudaErrors(cuInit(0));
  checkCudaErrors(cuDeviceGetCount(&devCount));
  checkCudaErrors(cuDeviceGet(&device, 0));

  char name[128];
  checkCudaErrors(cuDeviceGetName(name, 128, device));
  std::cout << "Using CUDA Device [0]: " << name << "\n";

  int devMajor, devMinor;
  checkCudaErrors(cuDeviceComputeCapability(&devMajor, &devMinor, device));
  std::cout << "Device Compute Capability: "
            << devMajor << "." << devMinor << "\n";
  if (devMajor < 2) {
    std::cerr << "ERROR: Device 0 is not SM 2.0 or greater\n";
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
  checkCudaErrors(cuCtxCreate(&context, 0, device));

  // create module for object
  checkCudaErrors(cuModuleLoadDataEx(&cudaModule, str.c_str(), 0, 0, 0));

  // get kernel function
  checkCudaErrors(cuModuleGetFunction(&function, cudaModule, "mandelbrot"));

  // device data
  CUdeviceptr devResultBuffer;

  checkCudaErrors(cuMemAlloc(&devResultBuffer, sizeof(int)*WIDTH*HEIGHT));

  int hostResult = new int[WIDTH*HEIGHT];

  for (unsigned i = 0; i != WIDTH*HEIGHT; ++i) {
    hostResult[i] = 0;
  }

  checkCudaErrors(cuMemcpyHtoD(devResultBuffer, &hostResult[0], sizeof(int)*WIDTH*HEIGHT));


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
  checkCudaErrors(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ,
                                 blockSizeX, blockSizeY, blockSizeZ,
                                 0, NULL, KernelParams, NULL));

  checkCudaErrors(cuMemcpyDtoH(&hostResult[0], devResultBuffer, sizeof(int)*WIDTH*HEIGHT));

  print_result(hostResult);

  // Clean-up
  delete [] hostResult;
  checkCudaErrors(cuMemFree(devResultBuffer));
  checkCudaErrors(cuModuleUnload(cudaModule));
  checkCudaErrors(cuCtxDestroy(context));

  return 0;
}
