#include <cmath>
#include <iostream>
#include <vector>
using namespace std;

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

inline int concat(int a, int b) {
  return a * pow(10, ceil(log10(b))) + b;
}

// Are all pairwise concatenations of numbers in 'xs' prime?
bool prop(const vector<int>& xs) {
  for (size_t i = 0; i < xs.size(); i++) {
    for (size_t j = i + 1; j < xs.size(); j++) {
      if (!isPrime(concat(xs[i], xs[j])) ||
          !isPrime(concat(xs[j], xs[i]))) {
        return false;
      }
    }
  }
  return true;
}

int main() {
  // cout << prop({3, 7, 109, 673}) << endl;
  // cout << ceil(log10(109)) << endl;
  // cout << concat(7, 109) << endl;
  
  for (int a = 3;; a += 2) {
    if (!isPrime(a)) {
      continue;
    }
    // cout << "a: " << a << endl;
    for (int b = 3; b < a; b += 2) {
      if (!isPrime(b) || !prop({a, b})) {
        continue;
      }
      for (int c = 3; c < b; c += 2) {
        if (!isPrime(c) || !prop({a, b, c})) {
          continue;
        }
        for (int d = 3; d < c; d += 2) {
          if (!isPrime(d) || !prop({a, b, c, d})) {
            continue;
          }
          for (int e = 3; e < d; e += 2) {
            if (!isPrime(e) || !prop({a, b, c, d, e})) {
              continue;
            }
            cout << a << " " << b << " " << c << " " << d << " " << e << endl;
            cout << "sum: " << a + b + c + d + e << endl;
            exit(0);
          }
          // cout << a << " " << b << " " << c << " " << d << " " << endl;
        }
      }
    }
  }
}
