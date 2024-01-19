// Relatively straightforward brute force. The only real trick is
// computing the sums of factors of all numbers up to 1000000 up front
// using somethinglike the sieve of Eratosthenes. There are various
// further optimizations that could be done, like marking elements of
// failed chains (those with elements exceeding one million or ending
// at 1) to be skipped but this already finishes in under a second.

#include <iostream>
#include <unordered_set>
#include <vector>
using namespace std;

constexpr uint N = 1000000;

// Store the sums of proper divisors of all numbers up to N.
vector<uint> divisor_sums(N+1, 1);

void init_divisor_sums() {
  for (uint k = 2; k < N; k++) {
    for (uint i = k+1; i*k <= N; i++) {
      divisor_sums[i*k] += i + k;
    }
  }
}

// A number (up to N) is prime iff its only proper divisor is 1.
inline bool is_prime(uint n) {
  return divisor_sums[n] == 1;
}

// Attempt to compute an amicable chain starting from n. Returns the
// minimum element of the chain and the length of the chain, or {N+1,
// 0} if a prime element (thus the next element will be 1), an element
// exceeding N, or an element already seen but not equal to n is encountered.
pair<uint, uint> chain(uint n) {
  uint min = n;
  uint k = divisor_sums[n];
  uint count = 1;
  unordered_set<uint> seen{n};
  
  while (k != n) {
    if (k > N || is_prime(k) || seen.contains(k)) {
      return { N+1, 0 };
    }
    if (k < min) {
      min = k;
    }
    seen.insert(k);
    k = divisor_sums[k];
    count++;
  }
  
  return pair{min, count};
}

int main() {
  init_divisor_sums();
  
  uint smallest_element = N+1;
  uint longest_chain = 0;
  for (uint n = 1; n <= N; n++) {
    auto p = chain(n);
    if (p.second > longest_chain) {
      smallest_element = p.first;
      longest_chain = p.second;
    }
  }
  
  cout << "smallest element: " << smallest_element <<
    ", chain length: " << longest_chain << endl;
}
