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

#include "catch2/catch_all.hpp"
SCENARIO("111") {
  GIVEN("a thing") {
    int a = 3;
    WHEN("a += 1") {
      a += 1;
      AND_WHEN("a += 2") {
        a += 2;
        THEN("check a == 6") { REQUIRE(a == 6); }
        AND_THEN("check a >= 3") { CHECK(a >= 3); }
      }
      AND_THEN("check a >= 4") {
        REQUIRE(a >= 4);
      }
      THEN("check a == 4") {
        REQUIRE(a == 4);
      }
    }
    AND_WHEN("a+=3") {
      a += 3;
      THEN("check3") {
        REQUIRE(a >= 4);
      }
    }
  }
}
