// The answer ends up being just the product of the first seven
// primes: 2*3*5*7*11*13*17. The code here is generalized to any N.

#include <cmath>
#include <iomanip>
#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;

constexpr uint N = 1000000;

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

int main() {
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
