#include <cmath>
#include <iostream>
using namespace std;

void f(uint N) {
  cout << N << ": ";
  
  // 0/1, 1/n
  uint a = 0;
  uint b = 1;
  uint c = 1;
  uint d = N;

  while (c < N) {
    uint p = floor((N + b) / d) * c - a;
    uint q = floor((N + b) / d) * d - b;

    if (p == 3 && q == 7) {
      cout << c << "/" << d << endl;
      break;
    }
    
    a = c;
    b = d;
    c = p;
    d = q;
  }
}

uint numerator(uint N) {
  return 2 + 3 * ((N - 5) / 7);
}

uint denominator(uint N) {
  return 5 + 7 * ((N - 5) / 7);
}

int main() {
  // for (uint i = 1; i < 20; i++) {
  //   f(i);
  //   cout << numerator(i) << "/" << denominator(i) << endl;
  // }

  constexpr uint N = 1000000;
  cout << numerator(N) << "/" << denominator(N) << endl;
}
