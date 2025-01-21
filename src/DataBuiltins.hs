module DataBuiltins where
import Utils(Safe)
import VMData ( Any ) 

data BuiltinsSymbol = BackendBuiltins (String, [Any] -> Safe Any)
type Symbols = [BuiltinsSymbol]