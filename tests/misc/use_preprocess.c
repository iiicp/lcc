/***********************************
* File:     use_preprocess.c
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/10/5
*
* Sign:     不管几岁，快乐万岁
***********************************/

# define M(x)        M ## x
#define MM(M, y)    M = # y

#define NUMBER_OF_TAPE_DIVES = 5 ssssss dafasdf/*adddd*/ adsf adf adf

        # // null指令
# if 0
#   define AAA a
# else
#     define AAA b
#endif
#define A(a,b) a + b
/// 出现在宏展开中的自身的宏，不会继续被展开
#define AA BB
#define BB AA

#define TOKS "11111
#pragma ddddd
#pragma BB
int main() {
//    BB
//    ddddd
    return 0;
}