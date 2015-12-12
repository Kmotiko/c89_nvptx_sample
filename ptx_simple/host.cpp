#include <iostream>
#include <fstream>
#include <cassert>
#include "cuda.h"
#include "cutil.h"

int main(int argc, char **argv) {
  CUdevice    device;
  CUmodule    cudaModule;
  CUcontext   context;
  CUfunction  function;
  CUlinkState linker;
  int         devCount;

  // CUDA initialization
  CUDA_SAFE_CALL(cuInit(0));
  CUDA_SAFE_CALL(cuDeviceGetCount(&devCount));
  CUDA_SAFE_CALL(cuDeviceGet(&device, 0));

  char name[128];
  CUDA_SAFE_CALL(cuDeviceGetName(name, 128, device));
  std::cout << "Using CUDA Device [0]: " << name << "\n";

  int devMajor, devMinor;
  CUDA_SAFE_CALL(cuDeviceComputeCapability(&devMajor, &devMinor, device));
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

  // Create driver context
  CUDA_SAFE_CALL(cuCtxCreate(&context, 0, device));

  // Create module for object
  CUDA_SAFE_CALL(cuModuleLoadDataEx(&cudaModule, str.c_str(), 0, 0, 0));

  // Get kernel function
  CUDA_SAFE_CALL(cuModuleGetFunction(&function, cudaModule, "kernel"));

  // Device data
  CUdeviceptr devResultBuffer;

  CUDA_SAFE_CALL(cuMemAlloc(&devResultBuffer, sizeof(int)*16));

  int hostResult = new int[16];

  // Populate input
  for (unsigned i = 0; i != 16; ++i) {
    hostResult[i] = 0;
  }

  CUDA_SAFE_CALL(cuMemcpyHtoD(devResultBuffer, &hostResult[0], sizeof(int)*16));


  unsigned blockSizeX = 16;
  unsigned blockSizeY = 1;
  unsigned blockSizeZ = 1;
  unsigned gridSizeX  = 1;
  unsigned gridSizeY  = 1;
  unsigned gridSizeZ  = 1;

  // Kernel parameters
  void *KernelParams[] = { &devResultBuffer };

  std::cout << "Launching kernel\n";

  // Kernel launch
  CUDA_SAFE_CALL(cuLaunchKernel(function, gridSizeX, gridSizeY, gridSizeZ,
                                 blockSizeX, blockSizeY, blockSizeZ,
                                 0, NULL, KernelParams, NULL));

  // Retrieve device data
  CUDA_SAFE_CALL(cuMemcpyDtoH(&hostResult[0], devResultBuffer, sizeof(int)*16));

  std::cout << "Results:\n";
  print_result(hostResult);


  // Clean up after ourselves
  delete [] hostResult;

  // Clean-up
  CUDA_SAFE_CALL(cuMemFree(devResultBuffer));
  CUDA_SAFE_CALL(cuModuleUnload(cudaModule));
  CUDA_SAFE_CALL(cuCtxDestroy(context));

  return 0;
}
