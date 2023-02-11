int testAssign() {
  int a;
  int b;
  a = 100, b = 10;
  return a+b;
}

int testConditionExpr() {
  int a = 10;
  int b = 20;
  return a > b ? a : b;
}

int testLogOr() {
  int a = 0;
  int b = 10;
  if (a || b) {
    return a + b;
  }else {
    return a - b;
  }
}

int testLogAnd() {
  int a = 10;
  int b = 20;
  return a && b;
}

int testBitOr() {
  int a = 1;
  int b = 2;
  return a | b;
}

int testBitXor() {
  int a = 1;
  int b = 1;
  return a ^ b;
}

int testBitAnd() {
  int a = 1;
  int b = 2;
  return a & b;
}

int testEqual() {
  int a = 1;
  int b = 2;
  if (a == 1) {
    b += 2;
  }
  if (b != a) {
    b += a;
  }
  return b;
}

int testRelation() {
  int a = 1;
  int b = 2;

// todo if else code gen

//  int c;
//  if (a < b) {
//    c = b;
//  }else if (a > b) {
//    c = a;
//  }else {
//    c = 1000;
//  }

//  int d;
//  if (c >= 10) {
//    d = c;
//  }else {
//    d = 1024;
//  }
  return a < b;
}

int testShift() {
  int a = 1024;
  return (a << 2) + (a >> 2);
}

int testAdd() {
  int a = 3;
  int b = 5;
  return (a+b)+(a-b)+a*b+a/b;
}

int testUnary() {
  int a = 3;
  int *p = &a;
  *p = 100;
  return ~a + !a + (-a) + (+a);
}

int main() {
  return testUnary();
}