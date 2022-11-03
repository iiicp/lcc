### 基于LLVM的C编译器--lcc

#### 编译步骤

Mac平台编译:

```
1, 安装homebrew
可以使用国内源: https://brew.idayer.com/

2, 安装LLVM14
brew install llvm@14

3，clone仓库
git clone git@github.com:iiicp/lcc.git

4，使用CMake进行构建
cd lcc
mkdir build
cd build 
cmake .. -DLLVM_DIR="Path to Your LLVM CMake dir"
eg: cmake ..-DLLVM_DIR=/opt/homebrew/opt/llvm@14/lib/cmake/llvm
make 

5, 使用lcc
./lcc test.c
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
