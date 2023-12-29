// First we observe that 10^k has k+1 digits for any k, and is
// actually the smallest integer with that number of digits. We then
// ask: is there a k such that 9^k has *fewer* than k digits? If so,
// then we know for that k that there is no power of k with exactly k
// digits (because there is no power of k between 9^k and 10^k). We
// also know that the same will be true for all j >= k, because as j
// increases the gap between 9^j and 10^j only becomes larger, and
// 10^j is always the smallest number with j+1 digits, so 9^j moves
// down relatively (and thus always has j-1 or fewer digits).

#include <cmath>
#include <iostream>
using namespace std;

int main() {
  // First we find the first power n for which 9^n has fewer than n
  // digits (it turns out to be n=22).
  unsigned long long int n = 1;
  while (n++) {
    if (log10(pow(9, n)) + 1 < n) {
      break;
    }
  }

  // Then we count the number of k-digit integers which are kth powers
  // for all k < n.
  uint count = 0;
  for (unsigned long long int k = 1; k < n; k++) {
    for (uint i = 1;; i++) {
      const auto digits = (uint)(log10(pow(i, k)) + 1);
      count += digits == k;
      if (digits > k) {
        break;
      }
    }
  }
  cout << count << endl;
}
