#include <cmath>
#include <iomanip>
#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;

constexpr uint N = 1000000;

// int main() {
//   unordered_map<uint, vector<uint>> coprime;
  
//   // 0/1, 1/n
//   uint a = 0;
//   uint b = 1;
//   uint c = 1;
//   uint d = N;

//   while (c < N) {
//     uint p = floor((N + b) / d) * c - a;
//     uint q = floor((N + b) / d) * d - b;
//     // cout << p << " " << q << endl;

//     // coprime[p].push_back(q);
//     // coprime[q].push_back(p);

//     a = c;
//     b = d;
//     c = p;
//     d = q;

//     cout << c << endl;
//   }

//   // for (auto x : coprime[88]) {
//   //   cout << x << " ";
//   // }
// }

bool isPrime(uint n) {
  if (n == 2) {
    return true;
  }
  for (uint i = 3; i*i <= n; i += 2) {
    if (n % i == 0) {
      return false;
    }
  }
  return true;
}

uint gcd(uint a, uint b) {
  while (b != 0) {
    auto t = b;
    b = a % b;
    a = t;
  }
  return a;
}

int main() {
  
  // cout << left;
  // double max_n_over_phi = 0.0;
  // uint max_n;
  // for (uint n = 2; n <= N; n++) {
  //   cout << setw(2) << n << " ";
  //   uint phi = 0;
  //   for (uint i = 1; i < n; i++) {
  //     if (gcd(i, n) == 1) {
  //       phi++;
  //     }
  //   }
  //   cout << setw(2) << phi << " " << n / (double)phi << endl;
  //   if (n / (double)phi > max_n_over_phi) {
  //     max_n_over_phi = n / (double)phi;
  //     max_n = n;
  //   }
  // }
  // cout << max_n << endl;

  uint n = 2;
  for (uint i = 3;; i += 2) {
    if (n * i > N) {
      break;
    }
    if (isPrime(i)) {
      n *= i;
    }
  }
  cout << n << endl;
}
