int f(int a) {
  return a;
}

int (*func(int a, int b))(int a) {
  return f;
}

int main() {
  return func(10,-10)(11);
}