{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : (c) 2010-2012 Benedikt Schmidt
-- License     : GPL v3 (see LICENSE)
-- 
-- Maintainer  : Benedikt Schmidt <beschmi@gmail.com>
--
-- Builtin function symbols and signatures.
module Term.Builtin.Signature where

import Term.LTerm
import qualified Data.Set as S

----------------------------------------------------------------------
-- Builtin symbols (pair and inv are defined in Term.Term)
----------------------------------------------------------------------

-- | Binary builtin function symbols.
sdecSym, sencSym, adecSym, aencSym, signSym :: NoEqSym
sdecSym = ("sdec",2)
sencSym = ("senc",2)
adecSym = ("adec",2)
aencSym = ("aenc",2)
signSym = ("sign",2)

-- | Ternary builtin function symbols.
verifySym :: NoEqSym
verifySym = ("verify",3)

-- | Unary builtin function symbols.
pkSym, hashSym :: NoEqSym
pkSym = ("pk",1)
hashSym = ("h",1)

-- | Nullary builtin function symbols.
trueSym :: NoEqSym
trueSym = ("true",0)

----------------------------------------------------------------------
-- Builtin signatures
----------------------------------------------------------------------

-- | The signature for symmetric encryption.
symEncFunSig :: NoEqFunSig
symEncFunSig = S.fromList $ [ sdecSym, sencSym ]

-- | The signature for asymmetric encryption.
asymEncFunSig :: NoEqFunSig
asymEncFunSig = S.fromList $ [ adecSym, aencSym, pkSym ]

-- | The signature for cryptographic signatures.
signatureFunSig :: NoEqFunSig
signatureFunSig = S.fromList $ [ signSym, verifySym, trueSym, pkSym ]

-- | The signature for hashing.
hashFunSig :: NoEqFunSig
hashFunSig = S.fromList [ hashSym ]
