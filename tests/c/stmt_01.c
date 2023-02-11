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

int testSwitchCase() {
  int a = 10;
  int b = 3;
  switch (a) {
  case 0: {
    b+=0;
    break;
  }
  case 1: {
    b+=1;
    break;
  }
  case 2: {
    b+=2;
    break;
  }
  case 3: {
    b+=3;
    break;
  }
  case 4: {
    b+=4;
    break;
  }
  case 5: {
    b+=5;
    break;
  }
  case 10: {
    b+=10;
    break;
  }
  default:{
    b+=100;
    break;
  }
  }
  return b;
}

int testGotoLabel() {
  int a = 10, b = 20;
  int c;
  if (a < b) {
    c = b;
    goto exit;
  }else {
    c = a;
    goto exit;
  }
exit:
  return c;
}

int testExprStmt() {
  int a = 10;
  a = (unsigned short)(a < 20?a:30);
  return a;
}

int main() {
  return testBlockStmt();
}