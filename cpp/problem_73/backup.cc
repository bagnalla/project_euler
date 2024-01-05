#include <cmath>
#include <iostream>
#include <set>
#include <vector>
using namespace std;

constexpr uint N = 12000;

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

constexpr uint gcd(uint a, uint b) {
  while (b != 0) {
    auto t = b;
    b = a % b;
    a = t;
  }
  return a;
}

uint f(uint N) {
  // 0/1, 1/n
  uint a = 0;
  uint b = 1;
  uint c = 1;
  uint d = N;

  uint count = 0;

  while (c < N) {
    if (c / (double)d > 1/3.0 && c / (double)d < 1/2.0) {
      // cout << c << "/" << d << " ";
      count++;
    }
    
    uint p = floor((N + b) / d) * c - a;
    uint q = floor((N + b) / d) * d - b;

    a = c;
    b = d;
    c = p;
    d = q;
  }

  return count;
}

uint g(uint N) {
  // 0/1, 1/n
  uint a = 0;
  uint b = 1;
  uint c = 1;
  uint d = N;

  uint count = 0;

  while (c < N) {
    // cout << c << "/" << d << " ";
    count++;
    
    uint p = floor((N + b) / d) * c - a;
    uint q = floor((N + b) / d) * d - b;

    a = c;
    b = d;
    c = p;
    d = q;
  }

  return count;
}

uint h(uint d) {
  uint count = 0;
  for (uint i = 2*d+1; i < 3*d; i++) {
    if (i % 6 == 0 && gcd(i/6, d) == 1) {
      count++;
    }
  }
  return count;
}

uint hh(uint d) {
  uint sum = 0;
  for (uint i = 5; i <= d; i++) {
    sum += h(i);
  }
  return sum;
}

static vector<uint> primes;

void init_primes() {
  primes.push_back(2);
  for (uint i = 3; i <= N; i += 2) {
    if (isPrime(i)) {
      primes.push_back(i);
    }
  }
}

// static vector<set<uint>> prime_factors(N, set<uint>{});
static vector<vector<uint>> prime_factors(N, vector<uint>{});

void init_prime_factors() {
  for (uint i = 0; i < primes.size(); i++) {
    for (uint j = 0; j * primes[i] < N; j++) {
      prime_factors[j*primes[i]].push_back(primes[i]);
    }
  }
}

uint phi(uint n) {
  double result = n;
  for (const auto p : prime_factors[n]) {
    result *= 1 - 1.0 / p;
  }  
  return result;
}

int main() {
  cout << "Initializing primes..." << endl;
  init_primes();
  cout << "Initializing prime factors..." << endl;
  init_prime_factors();

  // for (const auto pfs : prime_factors) {
  //   for (const auto p : pfs) {
  //     cout << p << " ";
  //   }
  //   cout << endl;
  // }

  // cout << "Computing answer..." << endl;
  // uint prev = 0;
  // for (uint i = 2; i < N; i++) {
  //   // cout << i << ": ";
    
  //   // uint x = f(i);
  //   // uint y = g(i);
  //   // uint delta = x - prev;
  //   // uint phii = phi(i);
  //   // prev = x;
    
  //   // cout << " phi = " << phii << ", delta = " << delta <<
  //   //   ", phi/delta = " << phii / (double)delta << endl;
  //   // cout << "-- " << x << ", " << y << ", " << x/(float)y << ", " << (y - 3) / 6 << endl;
  //   // uint z = (y - 3) / 6;
  //   // if (z != x) {
  //   //   cout << i << ": " << x << " " << z << ", " << y / 6.0 << endl;
  //   // }
  //   // cout << i << ": " << delta << ", " << phii / 6.0 << endl;
  //   // cout << i << ": " << x << ", " << (y-5) / 6.0 << endl;
  //   // cout << i << ": " << x << " " << hh(i) << endl;
  // }

  cout << f(N) << endl;
  // cout << hh(N) << endl;
}
