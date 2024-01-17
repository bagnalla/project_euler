// Simple brute force.

#include <cmath>
#include <iostream>
#include <string>
using namespace std;

bool f(uint n) {
  while (true) {
    if (n == 1) {
      return false;
    }
    if (n == 89) {
      return true;
    }

    string s = to_string(n);
    n = 0;
    for (uint i = 0; i < s.length(); i++) {
      n += pow(s[i] - '0', 2);
    }
  }
}

constexpr uint N = 10000000;

int main() {
  uint count = 0;
  for (uint i = 1; i < N; i++) {
    count += f(i);
  }
  cout << count << endl;
}
