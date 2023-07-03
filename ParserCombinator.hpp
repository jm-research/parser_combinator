#ifndef PARSER_COMBINATOR_HPP
#define PARSER_COMBINATOR_HPP

#include <cstddef>
#include <optional>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>

namespace Parsec {

//-----------------------------------------------------------------------------
// Parser type define

using ParserInput = std::string_view;

template <typename T>
using ParserResult = std::optional<std::pair<T, ParserInput>>;

template <typename P>
using OptPairParserType = std::invoke_result_t<P, ParserInput>;

template <typename P>
using PairParserType = typename OptPairParserType<P>::value_type;

/// Parser a :: String -> Maybe (a, String)
template <typename T>
using Parser = auto (*)(ParserInput) -> ParserResult<T>;

template <typename P>
using ParserType = typename PairParserType<P>::first_type;

//-----------------------------------------------------------------------------
// trait define

// clang-format off
template <typename T>
struct CoercionTrait {
 public:
  using InputType = 
    typename CoercionTrait<decltype(&std::decay_t<T>::operator())>::InputType;
  using OutputType = 
    typename CoercionTrait<decltype(&std::decay_t<T>::operator())>::OutputType;
};

template <typename U, typename V>
struct CoercionTrait<auto (*)(U) -> V> {
  using InputType = U;
  using OutputType = V;
};

template <typename T, typename U, typename V>
struct CoercionTrait<auto (T::*)(U) -> V> {
  using InputType = U;
  using ResultType = V;
};

template <typename T, typename U, typename V>
struct CoercionTrait<auto (T::*)(U) const -> V>
    : CoercionTrait<auto (T::*)(U) -> V> {};
// clang-format on

//-----------------------------------------------------------------------------

/// fmap :: (a -> b) -> Parser a -> Parser b
template <typename F, typename P,
          typename R = std::invoke_result_t<F, ParserType<P>>>
constexpr auto fmap(F&& f, P&& p) {
  static_assert(
      std::is_same_v<typename CoercionTrait<F>::InputType, ParserType<P>> &&
      "type mismatch");
  return [=](ParserInput s) -> ParserResult<R> {
    auto result = p(s);
    if (!result) {
      return std::nullopt;
    }
    return {f(result->first), result->second};
  };
}

/// bind :: Parser a -> (a -> Parser b) -> Parser b
template <typename P, typename F,
          typename R = std::invoke_result_t<F, ParserType<P>, ParserInput>>
constexpr auto bind(P&& p, F&& f) {
  static_assert(
      std::is_same_v<typename CoercionTrait<P>::InputType, ParserType<P>> &&
      std::is_same_v<typename CoercionTrait<F>::OutputType,
                     OptPairParserType<P>> &&
      "type mismatch");
  return [=](ParserInput s) -> R {
    auto result = p(s);
    if (!result) {
      return std::nullopt;
    }
    return f(result->first, result->second);
  };
}

/// operator | :: Parser a -> Parser a -> Parser a
template <
    typename P1, typename P2,
    typename = std::enable_if_t<std::is_same_v<ParserType<P1>, ParserType<P2>>>>
constexpr auto operator|(P1&& p1, P2&& p2) {
  return [=](ParserInput s) {
    auto r1 = p1(s);
    if (r1) {
      return r1;
    }
    return p2(s);
  };
}

/// combine :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
template <typename P1, typename P2, typename F,
          typename R = std::invoke_result_t<F, ParserType<P1>, ParserType<P2>>>
constexpr auto combine(P1&& p1, P2&& p2, F&& f) {
  return [=](ParserInput s) -> ParserResult<R> {
    auto r1 = p1(s);
    if (!r1) {
      return std::nullopt;
    }
    auto r2 = p2(r1->second);
    if (!r2) {
      return std::nullopt;
    }
    return {f(r1->first, r2->first), r2->second};
  };
}

/// operator > :: Parser a -> Parser b -> Parser a
template <typename P1, typename P2, typename = ParserType<P1>,
          typename = ParserType<P2>>
constexpr auto operator>(P1&& p1, P2&& p2) {
  return combine(std::forward<P1>(p1), std::forward<P2>(p2),
                 [](auto&& l, auto) { return l; });
}

/// operator < : Parser a -> Parser b -> Parser b
template <typename P1, typename P2, typename = ParserType<P1>,
          typename = ParserType<P2>>
constexpr auto operator<(P1&& p1, P2&& p2) {
  return combine(std::forward<P1>(p1), std::forward<P2>(p2),
                 [](auto, auto&& r) { return r; });
}

}  // namespace Parsec

#endif  // PARSER_COMBINATOR_HPP