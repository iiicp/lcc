/***********************************
 * File:     main.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/2/22
 *
 * Sign:     enjoy life
 ***********************************/

#include "catch2/catch_session.hpp"
int main( int argc, char* argv[] ) {
  // your setup ...

  int result = Catch::Session().run( argc, argv );

  // your clean-up...

  return result;
}