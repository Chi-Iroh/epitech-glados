{-# LANGUAGE NumericUnderscores #-}

module TestSymbolTable (testSymbolTable) where

import Test.HUnit
import UtilsForTests
import SymbolTable

testEmptySymbolTable :: Test
testEmptySymbolTable = myAssertEqual "Empty symbol table" symbolTableEnd got
    where got = writeSymbolTable []

testHello0x37_SymbolTable :: Test
testHello0x37_SymbolTable = myAssertEqual "Symbol table with hello function at byte 0x37" expected got
    where expected = [0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x00] ++ [0x00, 0x00, 0x00, 0x37] ++ symbolTableEnd
          got = writeSymbolTable [("hello", 0x37)]

testMain0xA4E5FF8E_MaP00_0xAB417500_Qsort___0xEE41DCB9 :: Test
testMain0xA4E5FF8E_MaP00_0xAB417500_Qsort___0xEE41DCB9 = myAssertEqual title expected got
    where title = "Symbol table with [(Main, 0xA4E5FF8E), (MaP00, 0xAB417500), (Qsort__, 0xEE41DCB9)]"
          expectedMain = [0x4D, 0x61, 0x69, 0x6E, 0x00] ++ [0xA4, 0xE5, 0xFF, 0x8E]
          expectedMaP00 = [0x4D, 0x61, 0x50, 0x30, 0x30, 0x00] ++ [0xAB, 0x41, 0x75, 0x00]
          expectedQsort__ = [0x51, 0x73, 0x6F, 0x72, 0x74, 0x5F, 0x5F, 0x00] ++ [0xEE, 0x41, 0xDC, 0xB9]
          expected = expectedMain ++ expectedMaP00 ++ expectedQsort__ ++ symbolTableEnd
          got = writeSymbolTable [("Main", 0xA4_E5_FF_8E), ("MaP00", 0xAB_41_75_00), ("Qsort__", 0xEE_41_DC_B9)]

testSymbolTable :: Test
testSymbolTable = TestList  [ TestLabel "testSymbolTable" testEmptySymbolTable
                            , TestLabel "testSymbolTable" testHello0x37_SymbolTable
                            , TestLabel "testSymbolTable" testMain0xA4E5FF8E_MaP00_0xAB417500_Qsort___0xEE41DCB9]