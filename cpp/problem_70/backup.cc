#include <iomanip>
#include <iostream>
#include <map>
#include <vector>
using namespace std;

// constexpr uint N = 20;

uint gcd(uint a, uint b) {
  while (b != 0) {
    auto t = b;
    b = a % b;
    a = t;
  }
  return a;
}

static vector<uint> primes;

using factors = map<uint, uint>;

// inline void mul(factors& fs, const factors& gs) {
//   for (auto p : gs) {
//     fs[p.first] += p.second;
//   }
// }

inline void mul(factors& fs, uint n) {
  fs[n]++;
}

// Prime factorizations.
// Generate prime factorizations for all integers up to N using only
// factors from the first n primes.
vector<map<uint, uint>> pfs(uint n, uint N) {
  vector<factors> result{{{1, 1}}};
  
  if (n <= 0) {
    return result;
  }

  // for (uint i = 0; i < n; i++) {
  auto ps = pfs(n-1, N / primes[n]);
  for (auto& p : ps) {
    // TODO: loop here, adding new factors to the result
    // vector. Basically, for each p in ps, loop over powers of n
    // adding them to result until they exceed N.
    mul(p, primes[n]);
  }
  // }

  // TODO: might be more natural to do this with a loop instead of
  // recursion. Also could make a struct/class for factorizations and
  // overload the multiplication operator for it.

  return result;
}

int main() {
  
  // for (uint n = 2; n <= N; n++) {
  //   cout << "phi(" << n << ") = ";
  //   uint phi = 0;
  //   for (uint i = 1; i < n; i++) {
  //     if (gcd(i, n) == 1) {
  //       // cout << i << " ";
  //       phi++;
  //     }
  //   }
  //   cout << endl << "phi(" << n << ") = " << phi << endl;
  // }
  
  // for (uint n = 0; n <= N; n++) {
  //   uint square = n*n*n;
  //   uint phi = 0;
  //   for (uint i = 1; i < n; i++) {
  //     if (gcd(i, n) == 1) {
  //       phi++;
  //     }
  //   }
  //   cout << n*n << " * phi(" << n << ") = " << n*n * phi << endl;
  //   phi = 0;
  //   for (uint i = 1; i < square; i++) {
  //     if (gcd(i, square) == 1) {
  //       phi++;
  //     }
  //   }
  //   cout << "phi(" << square << ") = " << phi << endl;
  // }
  
}
