#include <functional>
#include <iostream>
#include <set>
#include <vector>
using namespace std;

// vector<uint> f(uint n) {
//   vector<uint> result;
//   for (uint i = 0; i < n; i++) {
//     auto v = f(n-i);
//     v.push_back(i);    
//   }
//   return v;
// }

// uint f(uint n) {
//   // cout << n << endl;
//   if (n == 1) {
//     return 1;
//   }
//   uint sum = 0;
//   for (uint i = 1; i < n; i++) {
//     sum += f(n-i);
//   }
//   return sum;
// }

template <typename T>
constexpr vector<T> concat(vector<T> a, const vector<T>& b) {
  a.insert(a.end(), b.cbegin(), b.cend());
  return a;
}

// vector<vector<uint>> f(uint n) {
//   // cout << n << endl;
//   if (n == 1) {
//     return { { 1 } };
//   }
//   vector<vector<uint>> result;
//   for (uint i = 1; i < n; i++) {
//     vector<vector<uint>> vs = f(n-i);
//     for (auto& v : vs) {
//       v.insert(v.begin(), i);
//     }
//     result = concat(result, vs);
//   }
//   return result;
// }

template <typename A, typename B>
set<B> set_map(const set<A>& a, function<B(const A&)> f) {
  set<B> result;
  for (const A x : a) {
    result.insert(f(x));
  }
  return result;
}

template <typename A>
void set_union(set<A>& a, const set<A>& b) {
  for (const auto y : b) {
    a.insert(y);
  }
}

template <typename A>
set<A> set_union(const set<A>& a, const set<A>& b) {
  set<A> result(a);
  for (const auto y : b) {
    result.insert(y);
  }
  return result;
}

// set<vector<uint>> f(uint n) {
//   function<set<vector<uint>>(uint)> go = [&go](uint n){
//     set<vector<uint>> result{{n}};
//     for (uint i = 1; i < n; i++) {
//       set_union(result, set_map<vector<uint>, vector<uint>>
//                 (go(n-i), [i](const vector<uint>& v){
//                   vector<uint> result(v);
//                   result.push_back(i);
//                   // result.insert(result.end(), v.cbegin(), v.cend());
//                   sort(result.begin(), result.end());
//                   return result;
//                 }));
//     }
//     return result;
//   };
//   auto result = go(n);
//   result.erase({n});
//   return result;
// }

vector<vector<uint>> f(uint n) {
  // Compute ways to sum n with integers <= ub.
  function<vector<vector<uint>>(uint, uint)> go = [&go](uint n, uint ub){
    vector<vector<uint>> result;
    if (n <= ub) {
      result.push_back({n});
    }
    for (uint i = 1; i <= min(n-1, ub); i++) {
      auto vs = go(n-i, i);
      for (auto& v : vs) {
        v.insert(v.begin(), i);
        // v.push_back(i);
        result.push_back(v);
      }
    }
    return result;
  };
  return go(n, n);
}

uint g(uint n) {
  // Compute number of ways to sum n with integers <= ub.
  function<uint(uint, uint)> go = [&go](uint n, uint ub){
    uint sum = n <= ub;
    for (uint i = 1; i <= min(n-1, ub); i++) {
      sum += go(n-i, i);
    }
    return sum;
  };
  return go(n, n);
}

int main() {
  // cout << f(5) << endl;
  // auto vs = f(90);
  // cout << vs.size()-1 << endl;

  auto sum = g(100);
  cout << sum-1 << endl;
  
  // for (const auto v : vs) {
  //   for (const auto n : v) {
  //     cout << n << " ";
  //   }
  //   cout << endl;
  // }

}
