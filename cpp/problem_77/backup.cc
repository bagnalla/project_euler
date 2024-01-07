#include <iostream>
#include <memory>
#include <optional>
#include <variant>
using namespace std;

// using Nat = variant<void, Nat>;

// struct Nat {
//   enum { Zero, Succ } tag;
//   union {
//     // unique_ptr<Nat> n;
//     Nat *n;
//   };
//   Nat() {
//     this->tag = Zero;
//   }
//   Nat(const Nat& n){
//     this->tag = Succ;
//     // this->n = make_unique<Nat>(n);
//     this->n = (Nat*)malloc(sizeof(Nat));
//     this->n->tag = n.tag;
//     this->n->n = n.n;
//   }
//   // ~Nat() {
//   //   switch (this->tag) {
//   //   case Zero:
//   //     break;
//   //   case Succ:
//   //     delete this->n;
//   //   }
//   // }
// };

// template <typename N>
// struct Succ{};

// using Zero = void;

// template <typename N>
// constexpr N pred(Succ<N> n) {
//   return n;
// }

// struct Nat {
//   enum { Zero, Succ } tag;
//   unique_ptr<Nat> n;
//     // Nat *n;
//   Nat() {
//     this->tag = Zero;
//   }
//   Nat(const Nat& n){
//     this->tag = Succ;
//     this->n = make_unique<Nat>(n);
//     // this->n = (Nat*)malloc(sizeof(Nat));
//     // this->n->tag = n.tag;
//     // this->n->n = n.n;
//   }
//   // ~Nat() {
//   //   switch (this->tag) {
//   //   case Zero:
//   //     break;
//   //   case Succ:
//   //     delete this->n;
//   //   }
//   // }
// };

// constexpr Nat pred(Nat n) {
//   switch (n.tag) {
//   case Nat::Zero:
//     throw "";
//   case Nat::Succ:
//     return *n.n;
//   }
// }

using Nat = enum { Zero, One, Two, Three, Four, Five };

constexpr Nat pred(const Nat& n) {
  switch (n) {
  case Zero: throw runtime_error{"pred of Zero"};
  case One: return Zero;
  case Two: return One;
  case Three: return Two;
  case Four: return Three;
  case Five: return Four;
  }
}

template <typename T, Nat n>
struct Vector {
  T hd;
  optional<Vector<T, pred(n)>> tl;
};

int main() {
  // Vector<uint, One> v{1, {}};
  Vector<uint, One> v = Vector<uint, One>(1, optional<Vector<uint, pred(One)>>{});
}
