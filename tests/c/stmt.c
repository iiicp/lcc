int testIf(int a, int b) {
  if (a > b) {
    return a;
  }else if (a < b){
    return b;
  }else {
    return a;
  }
}

int testWhile(int a) {
  int sum = 0;
  while (a < 100) {
    sum += a;
    a++;
  }
  return sum;
}

int testDoWhile() {
  int a = 10;
  int b = 20;
  int sum = 0;
  do {
    sum += a++;
  }while(a < b);
  return sum;
}

int testFor() {
  int i;
  int sum = 0;
  for (i = 0; i <= 100; ++i) {
    sum += i;
  }
  return sum;
}

int testForDeclaration() {
  int sum = 0;
  for (int i = 0; i <= 100; ++i) {
    sum += i;
  }
  return sum;
}

int testBreakContinue() {
  int sum = 0;
  for (int i = 0; i <= 100; ++i) {
    if (i == 50)
      break;
    if (i == 10)
      continue;
    sum += i;
  }
  return sum;
}

int testBlockStmt() {
  int a = 10;
  {
    int a = 20;
    a++;
  }
  {
    a++;
  }
  return a;
}

int main() {
  return testBreakContinue();
}