#include <iostream>
#include <variant>

#include "ParserCombinator.hpp"

using namespace Parsec;

constexpr auto eatWhitespace() {
  return many(oneOf(" \t\r\n"), std::monostate{},
              [](auto m, auto) { return m; });
};

constexpr auto constant() {
  constexpr auto Digit = oneOf("0123456789");
  return atLeast(Digit, 0,
                 [](int acc, char c) { return acc * 10 + (c - '0'); });
};

constexpr auto expr() {
  constexpr auto Op = eatWhitespace() < oneOf("+*") > eatWhitespace();
  return bind(Op, [](char o, ParserInput result) -> ParserResult<int> {
    if (o == '+') {
      return separatedBy(constant(), eatWhitespace(), 0,
                         std::plus<int>{})(result);
    }
    return std::nullopt;
  });
};

constexpr auto lisp() {
  return [](ParserInput s) {
    auto p = makeCharParser('(') < expr() > makeCharParser(')');
    return (eatWhitespace() < p > eatWhitespace())(s);
  };
};

auto main() -> int {
  const char* text = R"(
    (+ 123 456)
    )";
  auto r = lisp()(text);
  if (r) {
    std::cout << "(" << r->first << ", " << r->second << ")";
  }

  return 0;
}