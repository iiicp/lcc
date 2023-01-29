

### 基于LLVM的C编译器--lcc

#### 编译步骤

一，安装LLVM12 or LLVM14 or LLVM15

Mac平台

```bash
1, 安装homebrew
可以使用国内源: https://brew.idayer.com/

2, 安装LLVM14
brew install llvm@14
```

Linux平台

```bash
1, sudo apt-get update
2, sudo apt-get -y install llvm-12
```

Windows平台

```
// 安装过程都是默认操作
1, 安装 msys2 x64，官网 https://www.msys2.org/
2, 打开msys2-shell，运行 pacman -Sy
3，运行 pacman -Syu
4, 重新运行 msys2-shell，运行 pacman -Su
5，安装64bit clang, 运行 pacman -S mingw-w64-clang-x86_64-toolchain
6，安装32bit clang, 运行 pacman -S mingw-w64-clang-i686-toolchain
7，命令行检查clang, llvm版本

// windows LLVM_DIR PATH
eg: C:\msys64\clang64\lib\cmake\llvm
```



二，安装CMake工具

Mac平台

```
brew install cmake
```

Linux平台

```
sudo apt install cmake
```

Windows平台

```
https://cmake.org/download/
// 安装ninja
https://github.com/ninja-build/ninja/releases
```



三，Clone仓库

```
1, git clone git@github.com:iiicp/lcc.git

2, CMake进行构建
cd lcc
mkdir build
cd build 
cmake .. -DLLVM_DIR="Path to Your LLVM CMake dir"
// eg: cmake ..-DLLVM_DIR=/opt/homebrew/opt/llvm@14/lib/cmake/llvm
make 

3, 使用lcc
./lcc ../tests/c/stmt.c
```



### lcc开发效果

现在lcc可以通过jit解释运行如下代码.

```c
int fib(int n) {
  if (n == 1 || n == 2)
    return 1;
  return fib(n-1) + fib(n-2);
}

int main() {
  return fib(5);
}
```

也可以解释运行如下代码.

```c
int main() {
  int a = 3;
  int *b = &a;
  *b = 100;
  return a;
}
```

### lcc开发进展

- [x] 2022.11.03  v0.1
- [x] 2023.01.29  v0.2

[v0.1](https://github.com/iiicp/lcc/wiki/lcc-v0.1)   
[v0.2](https://github.com/iiicp/lcc/wiki/lcc-v0.2-todo)



