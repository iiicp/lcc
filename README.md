# lcc: A Based Of LLVM C Compiler

This is the source code Tutorial. Lcc is based llvm c99 compiler. You can click [this][1] to visit my video and this compiler tutorial.

This project will implement almost all ISO C99 syntax and can work on Windows, Linux and Mac OS X.

ISO C 99 Doc link (pdf file): [C99](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf)

## Current Status: 

|  Module     | Status     |     
|  ----       |------------|   
| Preprocess  | working... |   
| Lexer       | 100%       |      
| Parser      | 99.99%     |       
| Semantics   | working... |       
| CodeGen     | working... |    

## Test Status:

catch2 will be integrated in the future


## Compile and Run

### Install the latest LLVM library or Build LLVM manually

一：Install the latest LLVM library

```   
Mac:
----------------------
1, install homebrew 
reference: https://brew.sh/
or reference: https://brew.idayer.com/

2, brew install llvm

Linux:
----------------------
1, sudo apt-get update
2, sudo apt-get -y install llvm-12

Windows:
----------------------
reference:  https://blog.csdn.net/kingfox/article/details/117450533
```

二：Build LLVM manually

```
git clone https://github.com/llvm/llvm-project.git
cd llvm-project/llvm  && mkdir llvm_install_dir && mkdir build
cd build
cmake -GNinja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=../llvm_install_dir ../llvm
```


### How to use lcc

一：Compile

```
1, git clone git@github.com:iiicp/lcc.git
2, cd lcc && mkdir build && cd build 
3, cmake -GNinja -DLLVM_DIR="Path to Your LLVM CMake dir" ..
4, ninja 
```

二：Run

lcc support --dumpTokens and --dumpAst command line options
```  
./lcc --help
./lcc --dumpTokens ../tests/c/stmt.c
./lcc --dumpAst ../tests/c/stmt.c 
```

## lcc unsupported c99 language features

1. unicode char     
2. _Complex      
3. direct-declarator ( identifier-list{opt} )     
eg: int f(a, b) int a,b {return a+b;}


## lcc develop history

- 2022.11.03  [v0.1 history](https://github.com/iiicp/lcc/wiki/lcc-v0.1)
- 2023.01.29  [v0.2 history](https://github.com/iiicp/lcc/wiki/lcc-v0.2)


## Reference   

- [cld](https://github.com/zero9178/cld)        
- [chibicc](https://github.com/rui314/chibicc)        
- [ucc](https://github.com/sheisc/ucc162.3)   

[1]: https://space.bilibili.com/181099947