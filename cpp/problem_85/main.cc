// We notice that the number of rectangles in an n×m grid is simply
// the product of the nth and mth triangular numbers, which is
// calculated by the function 'f(n, m)' below. We search all
// combinations of n×m where m ≤ n and n×1 ≤ 2000000 (since any n
// beyond that will be too large) and take the value of f(n, m) that
// is closest to 2000000. Computes the answer instantly.

#include <cmath>
#include <iostream>
using namespace std;

constexpr uint f(uint n, uint m) {
  return n * m * (n+1) * (m+1) / 4;
}

int main() {
  uint max_i = sqrt(4000000.25) - 0.5 + 1;

  pair<uint, uint> closest = {1, 1};
  uint closest_dist = 2000000 - f(1, 1);
  
  for (uint i = 1; i <= max_i; i++) {
    for (uint j = 1; j <= i; j++) {
      uint dist = abs(2000000 - (int)f(i, j));
      if (dist < closest_dist) {
	closest = {i, j};
	closest_dist = dist;
      }
    }
  }

  cout << closest.first * closest.second << endl;
}
