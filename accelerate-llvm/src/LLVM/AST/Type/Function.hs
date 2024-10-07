{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE CPP                   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Function
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Function
  where

import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import qualified LLVM.AST.Attribute                                 as LLVM
import qualified LLVM.AST.Global                                    as LLVM
import qualified LLVM.AST.Instruction                               as LLVM

#if MIN_VERSION_llvm_hs(16,0,0)
import qualified LLVM.AST.FunctionAttribute                         as LLVM
#endif


-- | Attributes for the function call instruction
--
data FunctionAttribute
  = NoReturn
  | NoUnwind
  | ReadOnly
  | ReadNone
  | AlwaysInline
  | NoDuplicate
  | Convergent
  | InaccessibleMemOnly

-- | Tail call kind for function call instruction
--
data TailCall
  = Tail
  | NoTail
  | MustTail

-- | Parameters for functions
--
data Parameter a where
  Parameter :: PrimType a -> Name a -> Parameter a

-- | Attribute groups are groups of attributes that are referenced by
-- objects within the IR. To use an attribute group, an object must
-- reference its GroupID.
--
data GroupID = GroupID !Word


-- | Functions are arguments to the 'call' instruction; either global
-- functions or inline assembly.
--
data Function kind args t where
  Body :: Type r -> Maybe TailCall -> kind                -> Function kind '[]         r
  Lam  :: PrimType a -> Operand a -> Function kind args t -> Function kind (a ': args) t


instance Downcast FunctionAttribute LLVM.FunctionAttribute where
  downcast NoReturn            = LLVM.NoReturn
  downcast NoUnwind            = LLVM.NoUnwind
  downcast ReadOnly            = LLVM.ReadOnly
  downcast ReadNone            = LLVM.ReadNone
  downcast AlwaysInline        = LLVM.AlwaysInline
  downcast NoDuplicate         = LLVM.NoDuplicate
  downcast Convergent          = LLVM.Convergent
#if MIN_VERSION_llvm_hs(16,0,0)
  downcast InaccessibleMemOnly = LLVM.Memory (LLVM.Exact (LLVM.Inaccessiblemem LLVM.None))
#else
  downcast InaccessibleMemOnly = LLVM.InaccessibleMemOnly
#endif

instance Downcast (Parameter a) LLVM.Parameter where
  downcast (Parameter t n) = LLVM.Parameter (downcast t) (downcast n) attrs
    where
      attrs | PtrPrimType{} <- t = [LLVM.NoAlias, LLVM.NoCapture] -- XXX: alignment
            | otherwise          = []

instance Downcast TailCall LLVM.TailCallKind where
  downcast Tail     = LLVM.Tail
  downcast NoTail   = LLVM.NoTail
  downcast MustTail = LLVM.MustTail

instance Downcast GroupID LLVM.GroupID where
  downcast (GroupID n) = LLVM.GroupID n

