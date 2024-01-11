// Simple brute force solution.

#include <cmath>
#include <iostream>
#include <vector>
#include <unordered_set>
using namespace std;

constexpr uint N = 50000000;

constexpr bool isPrime(uint n) {
  if (n % 2 == 0) {
    return n == 2;
  }
  for (uint i = 3; i*i <= n; i += 2) {
    if (n % i == 0) {
      return false;
    }
  }
  return n > 1;
}

vector<uint> primes;

// Compute and store all primes p such that p² + 2³ + 2⁴ < N. Any
// prime beyond that is bigger than what we need to check.
void init_primes() {
  primes.push_back(2);
  for (uint p = 3; p*p + 24 < N; p += 2) {
    if (isPrime(p)) {
      primes.push_back(p);
    }
  }
}

int main() {
  init_primes();

  uint count = 0;
  unordered_set<uint> seen;

  for (uint i = 0; i < primes.size(); i++) {
    uint square = pow(primes[i], 2);
    for (uint j = 0;; j++) {
      uint cube = pow(primes[j], 3);
      if (square + cube + 16 >= N) {
        break;
      }
      for (uint k = 0;; k++) {
	uint fourth_power = pow(primes[k], 4);
        uint sum = square + cube + fourth_power;
	if (sum >= N) {
	  break;
	}
        if (!seen.contains(sum)) {
          count++;
          seen.insert(sum);
        }
      }
    }
  }
  
  cout << count << endl;
}
