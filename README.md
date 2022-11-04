

### 基于LLVM的C编译器--lcc

#### 编译步骤

一，安装LLVM12或者LLVM14

Mac平台

```bash
1, 安装homebrew
可以使用国内源: https://brew.idayer.com/

2, 安装LLVM14
brew install llvm@14
```

Ubuntu 20.04

```
1, sudo apt-get update
2, sudo apt-get -y install llvm-12
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

## lcc目前功能列表

### 一：预处理器

- [ ]  #include
- [ ]  #define
- [ ]  #if/#else/#elif/#endif

### 二：词法

- [x]  标识符
- [x]  关键字-所有
- [x]  分界符和运算符-所有
- [x]  字符常量
- [x]  数值常量
- [x]  字符串字面值
- [ ]  utf-8

### 三：语法

#### 声明部分

- [x]  基础类型
- [x]  指针类型
- [ ]  数组类型
- [ ]  结构体类型
- [x]  函数
- [x]  全局变量

#### 语句部分

- [x]  if语句
- [x]  while语句
- [x]  do-while语句
- [x]  for语句
- [x]  for-declaration语句
- [x]  return语句
- [x]  break语句
- [x]  continue语句
- [x]  声明语句
- [x]  表达式语句
- [x]  复合语句
- [ ]  goto语句
- [ ]  label语句
- [ ]  switch语句
- [ ]  case语句
- [ ]  多层if嵌套

#### 表达式

- [x]  赋值表达式
- [x]  条件表达式
- [x]  逻辑或表达式
- [x]  逻辑与表达式
- [x]  按位或表达式
- [x]  按位异或表达式
- [x]  按位与表达式
- [x]  关系表达式
- [x]  移位表达式
- [x]  加法表达式
- [x]  乘法表达式
- [ ]  强转表达式
- [x]  取地址表达式
- [x]  解引用表达式
- [x]  一元+表达式
- [x]  一元-表达式
- [x]  按位取反表达式
- [x]  逻辑取反表达式
- [x]  前置++
- [x]  前置—
- [ ]  sizeof表达式
- [ ]  数组调用表达式
- [ ]  函数调用表达式
- [ ]  结构体成员调用表达式
- [x]  后置++
- [x]  后置—
- [x]  变量表达式
- [x]  常量表达式
- [ ]  字符串表达式
- [x]  括号表达式

### 四:   测试驱动

### 五：语义分析

### 六：错误处理

### 七：Driver

### 八：跨平台



