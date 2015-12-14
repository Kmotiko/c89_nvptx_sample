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

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.tid.y", module);

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.tid.z", module);

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.ntid.x", module);

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.ntid.y", module);

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.ntid.z", module);

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.ctaid.x", module);

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.ctaid.y", module);

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.ctaid.z", module);

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.nctaid.x", module);

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.nctaid.y", module);

  arg_types.clear();
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "llvm.nvvm.read.ptx.sreg.nctaid.z", module);
}


void create_mandelbrot_function(llvm::IRBuilder<> *builder, llvm::Module *module){
  // add printf Function
  std::vector<llvm::Type *> arg_types;
  arg_types.clear();
  arg_types.push_back(llvm::PointerType::get(
       llvm::Type::getInt32Ty(llvm::getGlobalContext()), 1));
  llvm::FunctionType *func_type = llvm::FunctionType::get(llvm::Type::getVoidTy(llvm::getGlobalContext()), arg_types, false);
  llvm::Function *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "mandelbrot", module);
  auto arg_it = func->arg_begin();
  arg_it->setName("result");

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

  // create variable
  llvm::Value *xpv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "x_pitch");
  llvm::Value *ypv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "y_pitch");
  llvm::Value *sxv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "s_x");
  llvm::Value *syv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "s_y");
  llvm::Value *xv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "x");
  llvm::Value *yv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "y");
  llvm::Value *result_v = func->getValueSymbolTable().lookup("result");


  builder->CreateStore(
      builder->CreateFDiv(
        llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(4.0f)), 
        llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(800.0f))
        ),
  xpv);
  builder->CreateStore(
      builder->CreateFDiv(
        llvm::ConstantFP::get(llvm::getGlobalContext(),llvm::APFloat(4.0f)), 
        llvm::ConstantFP::get(llvm::getGlobalContext(),llvm::APFloat(640.0f))
        ),
  ypv);
  builder->CreateStore(llvm::ConstantFP::get(llvm::getGlobalContext(),llvm::APFloat(-2.0f)), sxv);
  builder->CreateStore(llvm::ConstantFP::get(llvm::getGlobalContext(),llvm::APFloat(2.0f)), syv);

  // get thread x
  llvm::Value *tid_x = builder->CreateCall(module->getValueSymbolTable().lookup("llvm.nvvm.read.ptx.sreg.tid.x"));
  // get thread y
  llvm::Value *tid_y = builder->CreateCall(module->getValueSymbolTable().lookup("llvm.nvvm.read.ptx.sreg.tid.y"));

  // get block x
  llvm::Value *bid_x = builder->CreateCall(module->getValueSymbolTable().lookup("llvm.nvvm.read.ptx.sreg.ctaid.x"));
  // get block y
  llvm::Value *bid_y = builder->CreateCall(module->getValueSymbolTable().lookup("llvm.nvvm.read.ptx.sreg.ctaid.y"));

  // get block dimx
  llvm::Value *dim_x = builder->CreateCall(module->getValueSymbolTable().lookup("llvm.nvvm.read.ptx.sreg.ntid.y"));
  // get block dimy
  llvm::Value *dim_y = builder->CreateCall(module->getValueSymbolTable().lookup("llvm.nvvm.read.ptx.sreg.ntid.y"));

  // x = dimx * block_x + x
  llvm::Value *id_x = builder->CreateAdd(
      builder->CreateMul(dim_x, bid_x),
      tid_x
      );
  // y = dimy * block_y + y
  llvm::Value *id_y = builder->CreateAdd(
      builder->CreateMul(dim_y, bid_y),
      tid_y
      );

  // x
  builder->CreateStore(
      builder->CreateFAdd(
        builder->CreateFMul(
          builder->CreateSIToFP(id_x,llvm::Type::getFloatTy(llvm::getGlobalContext())),
          builder->CreateLoad(xpv)
        ),
          builder->CreateLoad(sxv)
      ),
      xv);
  // y
  builder->CreateStore(
      builder->CreateFSub(
        builder->CreateLoad(syv),
        builder->CreateFMul(
          builder->CreateLoad(ypv),
          builder->CreateSIToFP(id_y,llvm::Type::getFloatTy(llvm::getGlobalContext()))
          )
        ), 
      yv);
  std::vector<llvm::Value*> arg_vec;
  arg_vec.push_back(builder->CreateLoad(xv));
  arg_vec.push_back(builder->CreateLoad(yv));

  // mul i* 800
  llvm::Value *tmp_v = builder->CreateMul(id_y, llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 800));
  // load j and add
  tmp_v = builder->CreateAdd(tmp_v, id_x);
  // GEP
  llvm::Value *gep_v = builder->CreateInBoundsGEP(result_v, tmp_v);

  builder->CreateStore(
    builder->CreateCall(module->getValueSymbolTable().lookup("calc_mandelbrot"), arg_vec), gep_v
  );

  builder->CreateRetVoid();
  return;
}




void create_calc_mandelbrot_function(llvm::IRBuilder<> *builder, llvm::Module *module){
  // add main Function
  std::vector<llvm::Type *> arg_types;
  arg_types.clear();
  arg_types.push_back(llvm::Type::getFloatTy(llvm::getGlobalContext()));
  arg_types.push_back(llvm::Type::getFloatTy(llvm::getGlobalContext()));
  llvm::FunctionType *func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  llvm::Function *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "calc_mandelbrot", module);
  auto arg_it = func->arg_begin();
  arg_it->setName("a");
  arg_it++;
  arg_it->setName("b");

  // Create BasicBlock(entry)
  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", func);
  builder->SetInsertPoint(bb);

  // create variable
  llvm::Value *iv = builder->CreateAlloca(llvm::Type::getInt32Ty(llvm::getGlobalContext()), nullptr, "i");
  llvm::Value *av = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "a_v");
  llvm::Value *bv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "b_v");
  llvm::Value *xv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "x");
  llvm::Value *yv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "y");
  llvm::Value *xxv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "xx");
  llvm::Value *yyv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "yy");
  builder->CreateStore(llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(0.0f)), xv);
  builder->CreateStore(llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(0.0f)), yv);
  builder->CreateStore(llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(0.0f)), xxv);
  builder->CreateStore(llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(0.0f)), yyv);
  llvm::Value *arga_v = func->getValueSymbolTable().lookup("a");
  llvm::Value *argb_v = func->getValueSymbolTable().lookup("b");
  builder->CreateStore(arga_v, av);
  builder->CreateStore(argb_v, bv);
  builder->CreateStore(llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), iv);

  // Add For Stmt Create BasicBlock(loop cond)
  llvm::BasicBlock *cond_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.cond", func);
  builder->CreateBr(cond_bb);
  builder->SetInsertPoint(cond_bb);
  llvm::PHINode *phi = llvm::PHINode::Create(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), 2, "condv", cond_bb );

  // Create BasicBlock(loop body)
  llvm::BasicBlock *body_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.body", func);
  builder->SetInsertPoint(body_bb);


  //  x = xx * xx - yy * yy + a;
  builder->CreateStore(
    builder->CreateFAdd(
      builder->CreateFSub(
        builder->CreateFMul(
          builder->CreateLoad(xxv), 
          builder->CreateLoad(xxv)
          ),
        builder->CreateFMul(
          builder->CreateLoad(yyv), 
          builder->CreateLoad(yyv)
          )
      ),
      builder->CreateLoad(av)
    ),
    xv
  );
  //  y = 2 * xx * yy + b;
  builder->CreateStore(
    builder->CreateFAdd(
      builder->CreateFMul(
        builder->CreateFMul(
          llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(2.0f)),
          builder->CreateLoad(xxv) 
        ),
        builder->CreateLoad(yyv)
      ),
      builder->CreateLoad(bv)
    ),
    yv
  );

  //  if( (x * x + y * y) > 4.0f ){
  //    return i;
  //  }
  llvm::Value *condv = builder->CreateFCmpOGT(
    builder->CreateFAdd(
      builder->CreateFMul(
        builder->CreateLoad(xv), 
        builder->CreateLoad(xv)
      ),
      builder->CreateFMul(
        builder->CreateLoad(yv), 
        builder->CreateLoad(yv)
      )
    ),
    llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(4.0f))
  );


  // Create BasicBlock(loop latch)
  llvm::BasicBlock *latch_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.latch", func);

  // Create BasicBlock(ret i)
  llvm::BasicBlock *ret_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "ret", func);
  builder->CreateCondBr(condv, ret_bb, latch_bb);
  builder->SetInsertPoint(ret_bb);
  builder->CreateRet(builder->CreateLoad(iv));
  //builder->CreateRet(phi);

  // builder->CreateBr(latch_bb);
  builder->SetInsertPoint(latch_bb);
  //  xx = x;
  //  yy = y;
  builder->CreateStore(builder->CreateLoad(xv), xxv);
  builder->CreateStore(builder->CreateLoad(yv), yyv);
  // increment cond value
  condv = builder->CreateAdd(
      phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 1));
  builder->CreateStore(condv, iv);
  builder->CreateBr(cond_bb);

  // Create BasicBlock(exit)
  llvm::BasicBlock *exit_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "exit", func);
  builder->SetInsertPoint(exit_bb);
  builder->CreateRet(llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), -1));

  // add loop branch instruction
  builder->SetInsertPoint(cond_bb);
  phi->addIncoming(llvm::ConstantInt::get(
                       llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), bb);
  phi->addIncoming(condv, latch_bb);
  condv = builder->CreateICmpSLT(
      phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 1024));
  builder->CreateCondBr(condv, body_bb, exit_bb);

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
  llvm::Module *module = new llvm::Module("mandelbrot", llvm::getGlobalContext());
  module->setTargetTriple(triple.getTriple());
  module->setDataLayout(data_layout.getStringRepresentation());

  // add functions
  create_intrinsic_function(builder, module);
  create_calc_mandelbrot_function(builder, module);
  create_mandelbrot_function(builder, module);

  llvm::legacy::PassManager pm;
  pm.add(createPrintModulePass(raw_stream));
  pm.run(*module);
  raw_stream.close();

  delete builder;
  return 0;
}
