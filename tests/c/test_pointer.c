/***********************************
 * File:     test_swap.c
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/11/3
 *
 * Sign:     enjoy life
 ***********************************/

int main() {
  int a = 3;
  int *b = &a;
  *b = 100;
  return a;
}
