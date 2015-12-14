#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/FileSystem.h"

#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include "llvm/Target/TargetMachine.h"
using namespace llvm;


void create_intrinsic_function(llvm::IRBuilder<> *builder, llvm::Module *module){
  std::vector<llvm::Type *> arg_types;
  arg_types.clear();
  llvm::FunctionType *func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  llvm::Function *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.tid.x", module);
}


void create_kernel_function(llvm::IRBuilder<> *builder, llvm::Module *module){
  // add printf Function
  std::vector<llvm::Type *> arg_types;
  arg_types.clear();
  arg_types.push_back(llvm::PointerType::get(
       llvm::Type::getFloatTy(llvm::getGlobalContext()), 1));
  llvm::FunctionType *func_type = llvm::FunctionType::get(llvm::Type::getVoidTy(llvm::getGlobalContext()), arg_types, false);
  llvm::Function *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "kernel", module);
  auto arg_it = func->arg_begin();
  arg_it->setName("A");

  // create named metadata
  llvm::NamedMDNode *named_mdn = module->getOrInsertNamedMetadata("nvvm.annotations");
  std::vector<llvm::Metadata*> vals;
  vals.push_back( llvm::ValueAsMetadata::get(func));
  vals.push_back(llvm::MDString::get(llvm::getGlobalContext(), "kernel"));
  vals.push_back(
      llvm::ValueAsMetadata::get(
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 1)
      )
      );
  named_mdn->addOperand(llvm::MDNode::get(llvm::getGlobalContext(), vals));

  // Create BasicBlock(entry)
  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", func);
  builder->SetInsertPoint(bb);

  // get thread x
  llvm::Value *tid_x = builder->CreateCall(module->getValueSymbolTable().lookup("llvm.nvvm.read.ptx.sreg.tid.x"));

  // GEP
  llvm::Value *gep_v = builder->CreateInBoundsGEP(arg_it, tid_x);

  // sitofp
  llvm::Value *idf = builder->CreateSIToFP(tid_x,llvm::Type::getFloatTy(llvm::getGlobalContext()));
  llvm::Value *result = builder->CreateFAdd(
    builder->CreateLoad(gep_v),
    idf
  );

  builder->CreateStore(result, gep_v);

  builder->CreateRetVoid();
  return;
}


int main(int argc, char **argv) {
  //std::string file_name = "test.ll";
  std::string file_name = argv[argc-1];
  // std::string error;
  std::error_code error;
  llvm::raw_fd_ostream raw_stream(file_name.c_str(), error,
                                  llvm::sys::fs::OpenFlags::F_RW);
  llvm::IRBuilder<> *builder = new llvm::IRBuilder<>(llvm::getGlobalContext());

  llvm::InitializeAllTargets();
  std::string err_str;
  llvm::Triple triple(
      llvm::Triple::getArchTypeName(llvm::Triple::ArchType::nvptx64),
      llvm::Triple::getVendorTypeName(llvm::Triple::VendorType::NVIDIA),
      llvm::Triple::getOSTypeName(llvm::Triple::OSType::CUDA)
      );
  llvm::Target const *target = llvm::TargetRegistry::lookupTarget(triple.getTriple(), err_str);
  llvm::TargetOptions options;
  llvm::TargetMachine *target_machine = target->createTargetMachine(triple.getTriple(), "sm_30", "", options);

  // getDataLayout is deleted in LLVM 3.6 ?
  llvm::DataLayout data_layout = target_machine->createDataLayout();

  // Create Module
  llvm::Module *module = new llvm::Module("kernel", llvm::getGlobalContext());
  module->setTargetTriple(triple.getTriple());
  module->setDataLayout(data_layout.getStringRepresentation());

  // add functions
  create_intrinsic_function(builder, module);
  create_kernel_function(builder, module);

  llvm::legacy::PassManager pm;
  pm.add(createPrintModulePass(raw_stream));
  pm.run(*module);
  raw_stream.close();

  delete builder;
  return 0;
}
