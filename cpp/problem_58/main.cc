#include <iostream>
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

int main() {
  uint n = 1;
  uint prime_count = 0;
  uint total_count = 1;
  for (uint r = 0;;) {
    r++;
    for (uint i = 1; i <= 4; i++) {
      if (isPrime(n + 2 * r * i)) {
        prime_count++;
      }
    }
    total_count += 4;
    n += 8 * r;
    if (prime_count / (double)total_count < 0.1) {
      cout << 2 * r + 1 << endl;
      break;
    }
  }
  cout << "prime_count: " << prime_count << endl;
  cout << "total_count: " << total_count << endl;
  cout << "ratio: " << prime_count / (double)total_count << endl;
}
