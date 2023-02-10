/// symbol LONG in the typedef define in global scope
typedef long LONG;

/// symbol LONG in the global scope
//int LONG;

//enum E{
///// symbol LONG in the global scope
//LONG,
//};

/// symbol S in the struct/enum tag scope
struct S{
/// symbol LONG in the local(variable) scope
int LONG;
};

/// symbol AAA in the struct/enum tag scope
enum AAA{
/// symbol AAA in the global scope, does not conflict with tag scope
AAA
};

/// symbol main in the global scope
int main() {
/// symbol LONG is the typedef define in local(variable) scope
typedef long LONG;
    
enum {
/// symbol LONG in the local(variable) scope, some as character a and front typedef(LONG)
LONG
};
    
/// symbol a in the local(variable) sope
int a = 4;

/// search symbol a in the label scope
goto a;
/// symbol a in the label scope
a:
return 0;
}


/// summary
/// 1, global scope (typedef)
/// 2, local scope (struct declaration)
/// 3, struct/enum tag scope
/// 4, label scope
///
/// Symbol conflict, you need to search in the current scope
