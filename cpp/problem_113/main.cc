// Fairly straightforward dynamic programming solution.

// A bit of pencil+paper work shows that the number of 2-digit
// increasing numbers is equal to 1+2+3+4+5+6+7+8+9 = 45, the partial
// sums of which are 1, 3, 6, 10, 15, 21, 28, 36, and 45. The number
// of 3-digit increasing numbers is then 1+3+6+10+15+21+28+36+45=165,
// the sum of all the partial sums of the 2-digit increasing
// numbers. It turns out that we can build a table that looks like the
// following:

// 1 1 1  1  1  1  1  1  1
// 1 2 3  4  5  6  7  8  9
// 1 3 6 10 15 21 28 36 45
// ...

// where the first element of each row is 1 and the rest are equal to
// the sum of the two elements above and to the left. The rightmost
// element of the ith row gives the total number of (i-1)-digit
// increasing numbers (ignoring the first row), so to get the overall
// total number of increasing number under 10^100 we take the sum over
// the whole rightmost column minus 1.

// A similar table can be built for the decreasing numbers:

// 1 1  1  1  1  1  1  1  1
// 2 3  4  5  6  7  8  9 10
// 3 6 10 15 21 28 36 45 55
// ...

// where the first element of each row is 1 + the element above, and
// the rest are determined by the same rule as for increasing
// numbers. The rightmost element of row i then gives the total number
// of (i-1)-digit decreasing numbers PLUS 1 (so we have to subtract 1
// to get the real value). We sum over them just as for increasing
// numbers to get the overall total.

// The number of non-bouncy numbers is the sum of these two
// totals. The only caveat remaining is that some numbers are both
// increasing and decreasing, so we have to take care not to double
// count them. These numbers are just those consisting of all the same
// digit: 1, 11, 111, 2, 22, 222, etc. There are n*9 of them when we
// are counting the number of non-bouncy numbers less than 10^n, so
// for n=100 we must subtract 900 from the final count.

// Runs in about 4ms in my Linux VM.

#include <iostream>

using namespace std;

constexpr uint N = 101;

int main() {
  ulong increasing[N][9];
  ulong decreasing[N][9];

  for (uint j = 0; j < 9; j++) {
    increasing[0][j] = 1;
    decreasing[0][j] = 1;
  }

  for (uint i = 1; i < N; i++) {
    increasing[i][0] = 1;
    decreasing[i][0] = decreasing[i-1][0] + 1;
    for (uint j = 1; j < 9; j++) {
      increasing[i][j] = increasing[i][j-1] + increasing[i-1][j];
      decreasing[i][j] = decreasing[i][j-1] + decreasing[i-1][j];
    }
  }

  ulong count = 0;
  for (uint i = 1; i < N; i++) {
    count += increasing[i][8] + decreasing[i][8] - 1;
  }
  cout << count - (N-1)*9 << endl;
}
