/// abstract declarator
/// 1, param type list (in function declaration)
/// 2, type name (in case-expr, in sizeof-expr, in post-fix-expr)

struct S {
  int a,b:3;
  long c;
};

float f(int, float, struct S s);

struct s g1(int);
struct s g2();

typedef struct Person{
  const char *name;
  int age;
}Person;

Person constructPerson(const char *name, int age);

Person copyPerson(Person other);

Person copyPerson(Person other) {
  Person  p;
  p.name = other.name;
  p.age = other.age;
  return p;
}

Person *copyPerson2(Person *other) {
  Person  *p = (Person *)malloc(sizeof(Person));
  p->name = other->name;
  p->age = other->age;
  return p;
}
int main() {
  return 0;
}