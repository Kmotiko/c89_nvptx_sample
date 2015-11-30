#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/FileSystem.h"
using namespace llvm;


void create_gvalue(llvm::IRBuilder<> *builder, llvm::Module *module){
  std::vector<llvm::Constant *> const_vec;
  const_vec.push_back(
      llvm::ConstantInt::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()), 0x25
      )
  );
  const_vec.push_back(
      llvm::ConstantInt::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()), 0x64
      )
  );
  const_vec.push_back(
      llvm::ConstantInt::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()), 0x00
      )
  );
  llvm::Constant *const_array = llvm::ConstantArray::get(
      llvm::ArrayType::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()) ,3),
      const_vec
  );
  new llvm::GlobalVariable::GlobalVariable(
      *module,
      llvm::ArrayType::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()) ,3),
      true, // isConstant
      llvm::GlobalValue::PrivateLinkage,
      const_array,
      "str"
      );

  const_vec.clear();
  const_vec.push_back(
      llvm::ConstantInt::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()), 0x0A
      )
  );
  const_vec.push_back(
      llvm::ConstantInt::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()), 0x00
      )
  );
  const_array = llvm::ConstantArray::get(
      llvm::ArrayType::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()) ,2),
      const_vec
  );
  new llvm::GlobalVariable::GlobalVariable(
      *module,
      llvm::ArrayType::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()) ,2),
      true, // isConstant
      llvm::GlobalValue::PrivateLinkage,
      const_array,
      "str2"
      );


  const_vec.clear();
  const_vec.push_back(
      llvm::ConstantInt::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()), 0x23
      )
  );
  const_vec.push_back(
      llvm::ConstantInt::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()), 0x00
      )
  );
  const_array = llvm::ConstantArray::get(
      llvm::ArrayType::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()) ,2),
      const_vec
  );
  new llvm::GlobalVariable::GlobalVariable(
      *module,
      llvm::ArrayType::get(
        llvm::Type::getInt8Ty(llvm::getGlobalContext()) ,2),
      true, // isConstant
      llvm::GlobalValue::PrivateLinkage,
      const_array,
      "str3"
      );
  return;
}



void create_mandelbrot_function(llvm::IRBuilder<> *builder, llvm::Module *module){
  // add printf Function
  std::vector<llvm::Type *> arg_types;
  arg_types.clear();
  arg_types.push_back(llvm::PointerType::get(
       llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0));
  arg_types.push_back(llvm::Type::getFloatTy(llvm::getGlobalContext()));
  arg_types.push_back(llvm::Type::getFloatTy(llvm::getGlobalContext()));
  llvm::FunctionType *func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  llvm::Function *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "mandelbrot", module);
  auto arg_it = func->arg_begin();
  arg_it->setName("result");
  arg_it++;
  arg_it->setName("c_x");
  arg_it++;
  arg_it->setName("c_y");

  // Create BasicBlock(entry)
  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", func);
  builder->SetInsertPoint(bb);

  // create variable
  llvm::Value *result_v = builder->CreateAlloca(
      llvm::PointerType::get(
        llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), nullptr, "result_v");
  llvm::Value *iv = builder->CreateAlloca(llvm::Type::getInt32Ty(llvm::getGlobalContext()), nullptr, "i");
  llvm::Value *jv = builder->CreateAlloca(llvm::Type::getInt32Ty(llvm::getGlobalContext()), nullptr, "j");
  llvm::Value *xpv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "x_pitch");
  llvm::Value *ypv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "y_pitch");
  llvm::Value *sxv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "s_x");
  llvm::Value *syv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "s_y");
  llvm::Value *xv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "x");
  llvm::Value *yv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "y");
  llvm::Value *cxv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "c_x");
  llvm::Value *cyv = builder->CreateAlloca(llvm::Type::getFloatTy(llvm::getGlobalContext()), nullptr, "c_y");
  llvm::Value *arg_v = func->getValueSymbolTable().lookup("result");
  builder->CreateStore(arg_v, result_v);
  arg_v = func->getValueSymbolTable().lookup("c_x");
  builder->CreateStore(arg_v, cxv);
  arg_v = func->getValueSymbolTable().lookup("c_y");
  builder->CreateStore(arg_v, cyv);
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
  builder->CreateStore(
      builder->CreateFAdd(
        llvm::ConstantFP::get(llvm::getGlobalContext(),llvm::APFloat(-2.0f)), 
        builder->CreateLoad(cxv)
        ),
  sxv);
  builder->CreateStore(
      builder->CreateFAdd(
        llvm::ConstantFP::get(llvm::getGlobalContext(),llvm::APFloat(2.0f)), 
        builder->CreateLoad(cyv)
        ),
  syv);
  builder->CreateStore(llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), jv);
  builder->CreateStore(llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), iv);

  // Add For Stmt Create BasicBlock(loop cond)
  llvm::BasicBlock *cond_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.cond", func);
  builder->CreateBr(cond_bb);
  builder->SetInsertPoint(cond_bb);
  llvm::PHINode *phi = llvm::PHINode::Create(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), 2, "condv", cond_bb );


  llvm::BasicBlock *outer_body_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.body", func);
  builder->SetInsertPoint(outer_body_bb);
  builder->CreateStore(
      builder->CreateFSub(
        builder->CreateLoad(syv),
        builder->CreateFMul(
          builder->CreateLoad(ypv),
          builder->CreateSIToFP(builder->CreateLoad(iv),llvm::Type::getFloatTy(llvm::getGlobalContext()))
          )
        ), 
      yv);
  builder->CreateStore(llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), jv);

  // Add inner loop BasicBlock(loop cond)
  llvm::BasicBlock *inner_cond_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "inner.cond", func);
  builder->CreateBr(inner_cond_bb);
  builder->SetInsertPoint(inner_cond_bb);
  llvm::PHINode *inner_phi = llvm::PHINode::Create(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), 2, "i_condv", inner_cond_bb );

  // Create BasicBlock(loop body)
  llvm::BasicBlock *body_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.body", func);
  builder->SetInsertPoint(body_bb);
  builder->CreateStore(
      builder->CreateFAdd(
        builder->CreateFMul(
          builder->CreateSIToFP(builder->CreateLoad(jv),llvm::Type::getFloatTy(llvm::getGlobalContext())),
          builder->CreateLoad(xpv)
        ),
          builder->CreateLoad(sxv)
      ),
      xv);

  // load i
  llvm::Value *tmp_v = builder->CreateLoad(iv);
  // mul i* 800
  tmp_v = builder->CreateMul(tmp_v, llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 800));
  // load j and add
  tmp_v = builder->CreateAdd(tmp_v, builder->CreateLoad(jv));
  // GEP
  llvm::Value *gep_v = builder->CreateInBoundsGEP(
      builder->CreateLoad(result_v), tmp_v);

  std::vector<llvm::Value*> arg_vec;
  arg_vec.push_back(builder->CreateLoad(xv));
  arg_vec.push_back(builder->CreateLoad(yv));
  builder->CreateStore(
    builder->CreateCall(module->getValueSymbolTable().lookup("calc_mandelbrot"), arg_vec), gep_v
  );


  // Create Inner BasicBlock(loop latch)
  llvm::BasicBlock *inner_latch_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "inner.latch", func);
  builder->CreateBr(inner_latch_bb);
  builder->SetInsertPoint(inner_latch_bb);
  // increment cond value
  llvm::Value *inner_condv = builder->CreateAdd(
      inner_phi, llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 1));
  builder->CreateStore(inner_condv, jv);
  builder->CreateBr(inner_cond_bb);

  // Create Inner BasicBlock(exit)
  llvm::BasicBlock *inner_exit_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "inner.exit", func);
  builder->SetInsertPoint(inner_exit_bb);


  // Create BasicBlock(loop latch)
  llvm::BasicBlock *latch_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.latch", func);
  builder->CreateBr(latch_bb);
  builder->SetInsertPoint(latch_bb);
  // increment cond value
  llvm::Value *condv = builder->CreateAdd(
      phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 1));
  builder->CreateStore(condv, iv);
  builder->CreateBr(cond_bb);


  // add loop branch instruction
  builder->SetInsertPoint(inner_cond_bb);
  inner_phi->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), outer_body_bb);
  inner_phi->addIncoming(inner_condv, inner_latch_bb);
  inner_condv = builder->CreateICmpSLT(
      inner_phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 800));
  builder->CreateCondBr(inner_condv, body_bb, inner_exit_bb);


  // Create BasicBlock(exit)
  llvm::BasicBlock *exit_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "exit", func);
  builder->SetInsertPoint(exit_bb);
  builder->CreateRet(llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0));

  // add loop branch instruction
  builder->SetInsertPoint(cond_bb);
  phi->addIncoming(llvm::ConstantInt::get(
                       llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), bb);
  phi->addIncoming(condv, latch_bb);
  condv = builder->CreateICmpSLT(
      phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 640));
  builder->CreateCondBr(condv, outer_body_bb, exit_bb);

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




void create_print_loop(llvm::IRBuilder<> *builder, llvm::Module *module){
  // add printf Function
  std::vector<llvm::Type *> arg_types;
  arg_types.clear();
  arg_types.push_back(llvm::PointerType::get(
       llvm::Type::getInt8Ty(llvm::getGlobalContext()), 0));
  llvm::FunctionType *func_type = llvm::FunctionType::get(
      llvm::Type::getInt8Ty(llvm::getGlobalContext()), arg_types, true);
  llvm::Function *print_func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "printf", module);

  // add main Function
  arg_types.clear();
  arg_types.push_back(llvm::PointerType::get(
       llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0));
  func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  llvm::Function *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "print_loop", module);
  auto arg_it = func->arg_begin();
  for (; arg_it != func->arg_end(); ++arg_it) {
    arg_it->setName("result");
  }

  // Create BasicBlock(entry)
  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", func);
  builder->SetInsertPoint(bb);

  // create variable
  llvm::Value *result_v = builder->CreateAlloca(
      llvm::PointerType::get(
        llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), nullptr, "result_v");
  llvm::Value *iv = builder->CreateAlloca(llvm::Type::getInt32Ty(llvm::getGlobalContext()), nullptr, "i");
  llvm::Value *jv = builder->CreateAlloca(llvm::Type::getInt32Ty(llvm::getGlobalContext()), nullptr, "j");
  llvm::Value *arg_v = func->getValueSymbolTable().lookup("result");
  builder->CreateStore(arg_v, result_v);
  builder->CreateStore(llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), jv);
  builder->CreateStore(llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), iv);

  // Add For Stmt Create BasicBlock(loop cond)
  llvm::BasicBlock *cond_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.cond", func);
  builder->CreateBr(cond_bb);
  builder->SetInsertPoint(cond_bb);
  llvm::PHINode *phi = llvm::PHINode::Create(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), 2, "condv", cond_bb );
  builder->CreateStore(llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), jv);

  // Add inner loop BasicBlock(loop cond)
  llvm::BasicBlock *inner_cond_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "inner.cond", func);
  builder->SetInsertPoint(inner_cond_bb);
  llvm::PHINode *inner_phi = llvm::PHINode::Create(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), 2, "i_condv", inner_cond_bb );

  // Create BasicBlock(loop body)
  llvm::BasicBlock *body_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.body", func);

  llvm::Value *global_v;
  std::vector<llvm::Value*> idxs;
  std::vector<llvm::Value*> arg_vec;

  // create if block
  llvm::BasicBlock *if_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "if.true", func);
  builder->SetInsertPoint(if_bb);
  global_v = module->getValueSymbolTable().lookup("str3");
  idxs.clear();
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  arg_vec.clear();
  arg_vec.push_back(builder->CreateInBoundsGEP( global_v, idxs));
  builder->CreateCall(print_func, arg_vec);

  // create else block
  llvm::BasicBlock *else_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "else", func);
  builder->SetInsertPoint(else_bb);
  // load i
  llvm::Value *tmp_v = builder->CreateLoad(iv);
  // mul i* 800
  tmp_v = builder->CreateMul(tmp_v, llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 800));
  // load j and add
  tmp_v = builder->CreateAdd(tmp_v, builder->CreateLoad(jv));
  // GEP
  llvm::Value *gep_v = builder->CreateInBoundsGEP(
      builder->CreateLoad(result_v), tmp_v);

  global_v = module->getValueSymbolTable().lookup("str");
  idxs.clear();
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  arg_vec.clear();
  arg_vec.push_back(builder->CreateInBoundsGEP( global_v, idxs));
  arg_vec.push_back(builder->CreateSRem(builder->CreateLoad(gep_v), 
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 9)));
  builder->CreateCall(print_func, arg_vec);

  // create branch inst in body
  builder->SetInsertPoint(body_bb);
  // load i
  tmp_v = builder->CreateLoad(iv);
  // mul i* 800
  tmp_v = builder->CreateMul(tmp_v, llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 800));
  // load j and add
  tmp_v = builder->CreateAdd(tmp_v, builder->CreateLoad(jv));
  // GEP
  gep_v = builder->CreateInBoundsGEP(
      builder->CreateLoad(result_v), tmp_v);
  tmp_v = builder->CreateICmpEQ(
        builder->CreateLoad(gep_v), 
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), -1)
      );
  builder->CreateCondBr(tmp_v, if_bb, else_bb);


  // Create Inner BasicBlock(loop latch)
  llvm::BasicBlock *inner_latch_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "inner.latch", func);
  builder->SetInsertPoint(if_bb);
  builder->CreateBr(inner_latch_bb);
  builder->SetInsertPoint(else_bb);
  builder->CreateBr(inner_latch_bb);
  builder->SetInsertPoint(inner_latch_bb);

  // increment cond value
  llvm::Value *inner_condv = builder->CreateAdd(
      inner_phi, llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 5));
  builder->CreateStore(inner_condv, jv);
  builder->CreateBr(inner_cond_bb);

  // Create Inner BasicBlock(exit)
  llvm::BasicBlock *inner_exit_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "inner.exit", func);
  builder->SetInsertPoint(inner_exit_bb);
  // print LF
  global_v = module->getValueSymbolTable().lookup("str2");
  idxs.clear();
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  arg_vec.clear();
  arg_vec.push_back(builder->CreateInBoundsGEP( global_v, idxs));
  builder->CreateCall(print_func, arg_vec);


  // Create BasicBlock(loop latch)
  llvm::BasicBlock *latch_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.latch", func);
  builder->CreateBr(latch_bb);
  builder->SetInsertPoint(latch_bb);
  // increment cond value
  llvm::Value *condv = builder->CreateAdd(
      phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 10));
  builder->CreateStore(condv, iv);
  builder->CreateBr(cond_bb);


  // add loop branch instruction
  builder->SetInsertPoint(inner_cond_bb);
  inner_phi->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), cond_bb);
  inner_phi->addIncoming(inner_condv, inner_latch_bb);
  inner_condv = builder->CreateICmpSLT(
      inner_phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 800));
  builder->CreateCondBr(inner_condv, body_bb, inner_exit_bb);


  // Create BasicBlock(exit)
  llvm::BasicBlock *exit_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "exit", func);
  builder->SetInsertPoint(exit_bb);
  builder->CreateRet(llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0));

  // add loop branch instruction
  builder->SetInsertPoint(cond_bb);
  phi->addIncoming(llvm::ConstantInt::get(
                       llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), bb);
  phi->addIncoming(condv, latch_bb);
  condv = builder->CreateICmpSLT(
      phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 640));
  builder->CreateCondBr(condv, inner_cond_bb, exit_bb);

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

  // Create Module
  llvm::Module *module = new llvm::Module("mandelbrot", llvm::getGlobalContext());

  // add print loop
  create_gvalue(builder, module);
  create_print_loop(builder, module);
  create_calc_mandelbrot_function(builder, module);
  create_mandelbrot_function(builder, module);

  // add main Function
  std::vector<llvm::Type *> arg_types;
  arg_types.clear();
  llvm::FunctionType *func_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), arg_types, false);
  llvm::Function *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "main", module);


  // Create BasicBlock(entry)
  llvm::BasicBlock *bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", func);
  builder->SetInsertPoint(bb);

  // create variable
  llvm::Value *iv = builder->CreateAlloca(llvm::Type::getInt32Ty(llvm::getGlobalContext()), nullptr, "i");
  llvm::Value *jv = builder->CreateAlloca(llvm::Type::getInt32Ty(llvm::getGlobalContext()), nullptr, "j");
  llvm::Value *array_v = builder->CreateAlloca(llvm::ArrayType::get(
        llvm::Type::getInt32Ty(llvm::getGlobalContext()), 512000));
  builder->CreateStore(llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), iv);
  builder->CreateStore(llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), jv);

  // Add For Stmt Create BasicBlock(loop cond)
  llvm::BasicBlock *cond_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.cond", func);
  builder->CreateBr(cond_bb);
  builder->SetInsertPoint(cond_bb);
  llvm::PHINode *phi = llvm::PHINode::Create(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), 2, "condv", cond_bb);
  builder->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), jv);

  // Add inner loop BasicBlock(loop cond)
  llvm::BasicBlock *inner_cond_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "inner.cond", func);
  //builder->CreateBr(inner_cond_bb);
  builder->SetInsertPoint(inner_cond_bb);
  llvm::PHINode *inner_phi = llvm::PHINode::Create(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), 2, "i_condv", inner_cond_bb );

  // Create BasicBlock(loop body)
  llvm::BasicBlock *body_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.body", func);
  builder->SetInsertPoint(body_bb);
  // load j
  llvm::Value *tmp_v = builder->CreateLoad(jv);
  // mul j* 800
  tmp_v = builder->CreateMul(tmp_v, llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 800));
  // load i and add
  tmp_v = builder->CreateAdd(tmp_v, builder->CreateLoad(iv));
  // GEP
  std::vector<llvm::Value*> idxs;
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  idxs.push_back(tmp_v);
  llvm::Value *gep_v = builder->CreateInBoundsGEP(array_v, idxs);
  // store 0
  builder->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), gep_v);

  // Create Inner BasicBlock(loop latch)
  llvm::BasicBlock *inner_latch_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "inner.latch", func);
  builder->CreateBr(inner_latch_bb);
  builder->SetInsertPoint(inner_latch_bb);
  // increment cond value
  llvm::Value *inner_condv = builder->CreateAdd(
      inner_phi, llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 1));
  builder->CreateBr(inner_cond_bb);

  // Create Inner BasicBlock(exit)
  llvm::BasicBlock *inner_exit_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "inner.exit", func);
  builder->SetInsertPoint(inner_exit_bb);


  // Create BasicBlock(loop latch)
  llvm::BasicBlock *latch_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop.latch", func);
  builder->CreateBr(latch_bb);
  builder->SetInsertPoint(latch_bb);
  // increment cond value
  llvm::Value *condv = builder->CreateAdd(
      phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 1));
  builder->CreateBr(cond_bb);


  // add loop branch instruction
  builder->SetInsertPoint(inner_cond_bb);
  inner_phi->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), cond_bb);
  inner_phi->addIncoming(inner_condv, inner_latch_bb);
  inner_condv = builder->CreateICmpSLT(
      inner_phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 640));
  builder->CreateCondBr(inner_condv, body_bb, inner_exit_bb);


  // Create BasicBlock(exit)
  llvm::BasicBlock *exit_bb =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "exit", func);
  builder->SetInsertPoint(exit_bb);

  // create call
  std::vector<llvm::Value*> arg_vec;
  idxs.clear();
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  arg_vec.clear();
  arg_vec.push_back(builder->CreateInBoundsGEP(array_v, idxs));
  arg_vec.push_back(llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(0.0f)));
  arg_vec.push_back(llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(0.0f)));
  builder->CreateCall(
      module->getValueSymbolTable().lookup("mandelbrot"), 
      arg_vec);

  // Dump
  arg_vec.clear();
  idxs.clear();
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0) );
  arg_vec.push_back(builder->CreateInBoundsGEP(array_v, idxs));
  builder->CreateCall(
      module->getValueSymbolTable().lookup("print_loop"), 
      arg_vec);
  //create ret
  builder->CreateRet(llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0));

  // add loop branch instruction
  builder->SetInsertPoint(cond_bb);
  phi->addIncoming(llvm::ConstantInt::get(
                       llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0), bb);
  phi->addIncoming(condv, latch_bb);
  condv = builder->CreateICmpSLT(
      phi, llvm::ConstantInt::get(
               llvm::Type::getInt32Ty(llvm::getGlobalContext()), 800));
  builder->CreateCondBr(condv, inner_cond_bb, exit_bb);


  llvm::legacy::PassManager pm;
  pm.add(createPrintModulePass(raw_stream));
  pm.run(*module);
  raw_stream.close();

  delete builder;
  return 0;
}
