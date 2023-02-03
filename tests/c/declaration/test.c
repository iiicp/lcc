typedef int INTA,INTB;

INTA f(INTA a) {
 return a;
}

int (*func(INTB a, INTB b))(INTA a) {
	return f;
}

int main() {
	return func(10,-10)(11);
}