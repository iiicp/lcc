/***********************************
 * File:     parser_test.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/2/22
 *
 * Sign:     enjoy life
 ***********************************/

#include "catch2/catch_amalgamated.hpp"

SCENARIO("111") {
  GIVEN("a thing") {
    int a = 3;
    WHEN("a == 3") {
      a += 1;
      THEN("check") {
        REQUIRE(a == 4);
      }
      AND_THEN("check2") {
        CHECK(a >= 3);
      }
    }
    WHEN("a+=3") {
      a += 3;
      THEN("check3") {
        REQUIRE(a >= 4);
      }
    }
  }
}
