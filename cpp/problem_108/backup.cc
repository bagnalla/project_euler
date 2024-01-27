// https://oeis.org/A007051

// Solution lies here:
// https://oeis.org/A018892
// https://en.wikipedia.org/wiki/Divisor_function

// Need to bust out the big numbers I think... (OCaml here we come).

#include <cmath>
#include <iostream>
#include <vector>

using namespace std;

constexpr uint N = 60;

uint count_solutions(ulong n) {
  uint count = 0;
  // for (ulong y = 2 * n; y <= n*n + n; y += n) {
  for (ulong y = n + 1; y <= n*n + n; y++) {
    if ((y * n) % (y - n) == 0) {
      ulong x = y * n / (y - n);
      // cout << "x = " << x << ", y = " << y << endl;
      count++;
      if (x == y) {
        break;
      }
    }
  }
  return count;
}

uint num_solutions(ulong n) {
  return (pow(3, n) + 1) / 2;
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

static vector<uint> primes;

void init_primes() {
  primes.push_back(2);
  for (uint i = 3; i <= N; i += 2) {
    if (isPrime(i)) {
      primes.push_back(i);
    }
  }
}

static vector<vector<uint>> prime_factors(N, vector<uint>{});

// This could probably be optimized but it's good enough as is.
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

uint target = 1000;

int main() {

  init_primes();

  ulong n = 1;
  for (uint i = 0; i < primes.size(); i++) {
    n *= primes[i];
    uint count = count_solutions(n);
    if (count > target) {
      n /= primes[i];
      for (uint j = 0; j < i; j++) {
        n *= primes[j];
        if (count_solutions(n) >= target) {
          cout << n << endl;
          exit(0);
        }
      }
      cout << "sum ting wong" << endl;
      exit(-1);
    }
  }

  cout << "ran out of primes!" << endl;

  // for (uint i = 0; i < 100; i++) {
  //   cout << count_solutions(i) << ",";
  // }
  // cout << endl;

  // uint max = 0;

  // ulong n = 2;
  // for (uint k = 3;; k += 2) {
  //   if (isPrime(k)) {
  //     n *= k;
  //   } else {
  //     continue;
  //   }
  //   // cout << count_solutions(n) << endl;
  //   // break;
  //   cout << "k = " << k << ", n = " << n << endl;
  //   // if (count_solutions(n) > 1000) {
  //   //   cout << n << endl;
  //   //   break;
  //   // }
    
  //   uint c = count_solutions(n);
  //   cout << "count: " << c << endl;
  //   if (c > max) {
  //     max = c;
  //     // cout << n << ": " << c << endl;
  //   }
  // }

  // ulong n = 1;
  // for (uint i = 0; i < 10; i++) {
  //   n *= primes[i];
  // }

  // for (const auto p : primes) {
  //   cout << p << " ";
  // }
  // cout << endl;
  
  // ulong n = 2*2*3*3*5*7*11*13;
  // uint count = count_solutions(n);
  // cout << n << " " << count << endl;
  
  // << " " << phi(n) << " " <<
  //   phi(n) / (float)count << " " << n / (float)phi(n) << endl;

  // for (uint n = 1; n <= 210; n++) {
  //   cout << "n = " << n << ", count = " << count_solutions(n) << ", phi(n) = " << phi(n) << endl;
  // }
  
}
