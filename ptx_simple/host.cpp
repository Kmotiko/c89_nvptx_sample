#include <iostream>
#include <fstream>
#include <cassert>
#include "cuda.h"

void checkCudaErrors(CUresult err) {
  const char *err_str;
  if(err!=CUDA_SUCCESS){
    cuGetErrorString(err, &err_str);
    printf("%s\n", err_str);
    exit(1);
  }
}

void print_result(int *result){
  for(int i=0; i < 16; ++i){
    printf("%d\n", result[i]);
  }
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

  // Create driver context
  CUDA_CHECK_ERR(cuCtxCreate(&context, 0, device));

  // Create module for object
  CUDA_CHECK_ERR(cuModuleLoadDataEx(&cudaModule, str.c_str(), 0, 0, 0));

  // Get kernel function
  CUDA_CHECK_ERR(cuModuleGetFunction(&function, cudaModule, "kernel"));

  // Device data
  CUdeviceptr devResultBuffer;

  CUDA_CHECK_ERR(cuMemAlloc(&devResultBuffer, sizeof(float)*16));

  float *hostResult = new float[16];

  // Populate input
  for (int i = 0; i != 16; ++i) {
    hostResult[i] = 0.0f;
  }

  CUDA_CHECK_ERR(cuMemcpyHtoD(devResultBuffer, &hostResult[0], sizeof(float)*16));


  unsigned gridSizeX  = 1;
  unsigned gridSizeY  = 1;
  unsigned gridSizeZ  = 1;
  unsigned blockSizeX = 16;
  unsigned blockSizeY = 1;
  unsigned blockSizeZ = 1;

  // Kernel parameters
  void *KernelParams[] = { &devResultBuffer };

  std::cout << "Launching kernel\n";

  // Kernel launch
  CUDA_CHECK_ERR(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ,
                                 blockSizeX, blockSizeY, blockSizeZ,
                                 0, NULL, KernelParams, NULL));

  // Retrieve device data
  CUDA_CHECK_ERR(cuMemcpyDtoH(&hostResult[0], devResultBuffer, sizeof(float)*16));

  std::cout << "Results:\n";
  print_result(hostResult);


  // Clean up after ourselves
  delete[] hostResult;

  // Clean-up
  CUDA_CHECK_ERR(cuMemFree(devResultBuffer));
  CUDA_CHECK_ERR(cuModuleUnload(cudaModule));
  CUDA_CHECK_ERR(cuCtxDestroy(context));

  return 0;
}
