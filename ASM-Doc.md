# Pseudo-ASM bytecode

The compiler will compile a PDP file into pseudo-ASM bytecode, described by this document.  
This bytecode will be then interpreted by the VM.  

# General assembly knowledge
```x86asm
add:
    push int 1
    call +
    pop int r0
    ret r0
```
`1` is called an `immediate value`, like any other hardcoded value.  
`add:` is called a label, it's a text describing what a section of assembly does.  
Labels are used to represent functions names and `if`'s conditional branches.  

# Registers & stack

Each <ins>scope</ins> has its own 16 registers, `r0, r1, ..., r15`.  
Consider a simple program which calls a function F1, which calls another function F2.  
Before calling F1, we are in the root scope with its 16 registers.  
When going into F1, F1's `r0 ... r15` are different from root scope's `r0 ... r15`.  
If F1 sets its `r0` to 8 for instance, when going into F2, we enter a new scope and thus F2's `r0` won't be worth 8, as F2 has its own 16 registers.  
That's why we talk about <ins>scopes</ins>.  

The whole program has a stack, used when calling / returning from a function.  
When calling a function, arguments are pushed into the stack in reverse order.  
When a function needs to access its argument, it pops them from the stack in order.  
When a function returns, its return value is pushed onto the stack.  
When the caller needs to get the returned value, it pops it from the stack.  

# Branching

The VM has an internal flag BF (stands for boolean flag), used by conditional branching instructions.  
This flag is either true or false, and is unset when the VM starts until the first time it is set.  
Even if BF is used by conditional branching instructions, its raw value is not exposed to any other instructions and thus it cannot be read/set by unrelated instructions.  

# Instructions

| Instructions family      | Instructions list |
|--------------------------|-------------------|
| Stack manipulators       | push, pop         |
| Conditional control flow | test, jt, jf      |
| Function control flow    | call, ret         |
| Register manipulator     | mov               |

## Stack manipulators

As introduced above, the stack is used when dealing with functions.  
2 instructions alter the stack.  

### push

```x86asm
push type value
```
This instruction pushes a value on top of the stack.  
The type must be a valid PDP type, as described in the language specification.  

Example :
```x86asm
push int 4
push [bool] [#t, #f]
push {uint, [float]} {4, [8, 6]}
```

### pop

```x86asm
pop type register
```
This instruction pops the top value of the stack into the specified register.  
If the stack is empty, the VM will throw a runtime error.  
The type must be a valid PDP type, as described in the language specification.  

Example :
```x86asm
pop int r0
pop [bool] r1
pop {uint, [float]} r2
```

## Conditional control flow

These 3 instructions allow conditional branching.  

### test
```x86asm
test register
```

If the register doesn't contain a boolean value, the VM will cause a runtime error.  
This instruction sets the internal flag BF to true if the register contains true, false otherwise.  

### jt / jf

```x86asm
test r0
jt label_true
jf label_false

label_true:
label_false:
```

These instructions perform a conditional branching jump, depending on BF.  
If BF is unset (if no test performed before jt/jf), the VM will throw a runtime error.  
jt (<ins>j</ins>ump <ins>t</ins>rue) jumps if BF is true, otherwise does nothing.  
jf (<ins>j</ins>ump <ins>f</ins>alse) jumps if BF is false, otherwise does nothing.  

## Function control flow

These 2 instructions allow calling and returning from a procedure.  
To track the call stack, the VM has a hidden internal stack of addresses simply called CT (acronym of call stack) in this section.  

### call
```x86asm
call function_label

function_label:
```

This instructions calls a function, it means it saves the current address in an internal and hidden stack, and then jumps to the function address.  

### ret
```x86asm
ret type value

; example
func:
    ret int 4
```

This instruction pushes a value of a certain type onto the stack (exactly the same as `push type value`), then pops the top address of CT and jumps to it.  
If CT is empty, the VM will throw a runtime error.  

## Register manipulator

### mov
```x86asm
mov register (destination), type register (source)
mov register, type value
```

This instruction puts a value in a register, it can be either another register of the given type or an immediate value of the given type.  
If the source register isn't of the given type, the VM will throw a runtime error.  