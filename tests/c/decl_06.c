/// symbol LONG in the typedef define in global scope
typedef long LONG;
/// symbol LONG in the global scope
//int LONG; // error
int main() {
  typedef int INT;
  { int INT; }
  return 0;
}