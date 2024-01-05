// Bit of a pea-brain brute force solution but it works and only takes
// a second or two to compute the answer.

#include <iostream>
#include <string>
using namespace std;

constexpr uint N = 1000000;

constexpr uint fact(uint n) {
  uint product = 1;
  for (uint i = 2; i <= n; i++) {
    product *= i;
  }
  return product;
}

uint f(uint n) {
  uint sum = 0;
  string s = to_string(n);
  for (const char c : s) {
    sum += fact(c - '0');
  }
  return sum;
}

uint chain_length(uint n) {
  uint count = 1;
  while (true) {
    if (n == 2 || n == 145 || n == 40585) {
      return count;
    }
    if (n == 169 || n == 363601 || n == 1454) {
      return count + 2;
    }
    if (n == 871 || n == 45361 || n == 872 || n == 45362) {
      return count + 1;
    }
    count++;
    n = f(n);
  }
  return count;
}

int main() {
  // cout << f(40585) << endl;
  // cout << chain_length(14558) << endl;

  uint count = 0;
  for (uint i = 3; i < N; i++) {
    if (chain_length(i) == 60) {
      count++;
    }
  }
  cout << count << endl;
  
}
