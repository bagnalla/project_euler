// Slightly clever brute force. We observe an inverse relationship
// between the number of solutions for [n] and the number of coprime
// numbers less than n (given by phi(n)). So, we expect the ratio of
// [count_solutions(n) / n] to be maximized then the number of
// divisors of [n] is maximized. E.g., every prime number has only two
// solutions because it has no non-trivial divisors at all (and large
// [phi(n)]). This means basically that we're looking for an [n] that
// is as "nonprime" as possible, i.e., has as many distinct prime
// factors as possible. This leads to the following algorithm:

// 1) Find the first product of primes whose solution count exceeds
// the target value. For our problem, we find that [2*3*5*7*11*13*17] has
// over 1000 solutions, so we stop there.

// 2) Remove the largest prime from the product, and start over from
// the beginning adding new prime factors until reaching a product
// whose solution count exceeds the target value. We end up with
// [2^2*3^2*5*7*11*13 = 180180].

// This approach doesn't scale well enough for problem 110 so we will
// have to 1) find a closed form expression for count_solutions (which
// appears to be given here: https://oeis.org/A018892) and 2) use big
// integers, so OCaml here we come!

#include <cmath>
#include <iostream>
#include <vector>

using namespace std;

uint count_solutions(ulong n) {
  uint count = 0;
  for (ulong y = n + 1; y <= n*n + n; y++) {
    if ((y * n) % (y - n) == 0) {
      ulong x = y * n / (y - n);
      count++;
      if (x == y) {
        break;
      }
    }
  }
  return count;
}

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

constexpr uint N = 60;
static vector<uint> primes;
void init_primes() {
  primes.push_back(2);
  for (uint i = 3; i <= N; i += 2) {
    if (isPrime(i)) {
      primes.push_back(i);
    }
  }
}

constexpr uint TARGET = 1000;

int main() {
  init_primes();

  ulong n = 1;
  for (uint i = 0; i < primes.size(); i++) {
    n *= primes[i];
    uint count = count_solutions(n);
    if (count > TARGET) {
      n /= primes[i];
      for (uint j = 0; j < i; j++) {
        n *= primes[j];
        if (count_solutions(n) >= TARGET) {
          cout << n << endl;
          exit(0);
        }
      }
      cout << ":(" << endl;
      exit(-1);
    }
  }

  cout << "ran out of primes!" << endl;
  exit(-1);
}
