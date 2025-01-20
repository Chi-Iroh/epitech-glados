{-# LANGUAGE NumericUnderscores #-}

module TestSymbolTable (testSymbolTable) where

import Test.HUnit
import Utils (Safe(..))
import UtilsForTests
import SymbolTable

testEmptySymbolTable :: Test
testEmptySymbolTable = TestList     [ myAssertEqual "Encoding empty symbol table" symbolTableEnd (writeSymbolTable [])
                                    , myAssertEqual "Decoding empty symbol table (nothing after)" (Value ([], [])) (readSymbolTable symbolTableEnd)
                                    , myAssertEqual "Decoding empty symbol table (code after)" (Value ([], [0x78, 0x45])) (readSymbolTable (symbolTableEnd ++ [0x78, 0x45]))]

testHello0x37_SymbolTable :: Test
testHello0x37_SymbolTable = TestList    [ myAssertEqual ("Encoding " ++ title) encoded (writeSymbolTable symtab)
                                        , myAssertEqual ("Decoding " ++ title ++ " (nothing after)") (Value (symtab, [])) (readSymbolTable encoded)
                                        , myAssertEqual ("Decoding " ++ title ++ " (code after)") (Value (symtab, [0x78, 0x45])) (readSymbolTable (encoded ++ [0x78, 0x45]))]
    where symtab = [("hello", 0x37)]
          encoded = [0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x00] ++ [0x00, 0x00, 0x00, 0x37] ++ symbolTableEnd
          title = "symbol table with [(hello, 0x00'00'00'37)]"

testMain0xA4E5FF8E_MaP00_0xAB417500_Qsort___0xEE41DCB9 :: Test
testMain0xA4E5FF8E_MaP00_0xAB417500_Qsort___0xEE41DCB9 = TestList   [ myAssertEqual ("Encoding " ++ title) encoded (writeSymbolTable symtab)
                                                                    , myAssertEqual ("Decoding " ++ title ++ " (nothing after)") (Value (symtab, [])) (readSymbolTable encoded)
                                                                    , myAssertEqual ("Decoding " ++ title ++ " (code after)") (Value (symtab, [0x78, 0x45])) (readSymbolTable (encoded ++ [0x78, 0x45])) ]
    where title = "symbol table with [(Main, 0xA4'E5'FF'8E), (MaP00, 0xAB'41'75'00), (Qsort__, 0xEE'41'DC'B9)]"
          encodedMain = [0x4D, 0x61, 0x69, 0x6E, 0x00] ++ [0xA4, 0xE5, 0xFF, 0x8E]
          encodedMaP00 = [0x4D, 0x61, 0x50, 0x30, 0x30, 0x00] ++ [0xAB, 0x41, 0x75, 0x00]
          encodedQsort__ = [0x51, 0x73, 0x6F, 0x72, 0x74, 0x5F, 0x5F, 0x00] ++ [0xEE, 0x41, 0xDC, 0xB9]
          encoded = encodedMain ++ encodedMaP00 ++ encodedQsort__ ++ symbolTableEnd
          symtab = [("Main", 0xA4_E5_FF_8E), ("MaP00", 0xAB_41_75_00), ("Qsort__", 0xEE_41_DC_B9)]

testNoSymbolTable :: Test
testNoSymbolTable = myAssertEqual "No symbol table when reading" (Error noSymbolTableErrorMessage) (readSymbolTable [0x74, 0x85])

testNoBytes :: Test
testNoBytes = myAssertEqual "No symbol table when reading" (Error noSymbolTableErrorMessage) (readSymbolTable [])

testSymbolTable :: Test
testSymbolTable = TestList  [ {-TestLabel "testSymbolTable" testEmptySymbolTable
                        --     , TestLabel "testSymbolTable" testHello0x37_SymbolTable
                        --     , TestLabel "testSymbolTable" testMain0xA4E5FF8E_MaP00_0xAB417500_Qsort___0xEE41DCB9
                        -}      TestLabel "testSymbolTable" testNoSymbolTable]
                        --     , TestLabel "testSymbolTable" testNoBytes]