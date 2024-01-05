// We exploit Euclid's formula for generating primitive Pythagorean
// triples by deriving them from terms in a Farey sequence (where the
// upper bound on the denominator is derived below). We look at all
// multiples of the primitive triples less than or equal to 1500000.

// Given integers [m > n > 0] such that m and n are coprime and one of
// them is even, we can derive the unique corresponding primitive
// triple [a, b, c] as follows:

// a = m² - n²
// b = 2mn
// c = m² + n²

// A Farey sequence will give us such [n/m] pairs (after filtering out
// those where n and m are both odd), but we need to determine a value
// N that is guaranteed to generate all the triples with sums less
// than L=1500000.

// To do that, we set [a + b + c = L] and unfold the definitions of a,
// b, and c to arrive at [m² - n² + 2mn + m² + n²] which simplifies to
// [2m² + 2mn - L = 0]. We let [n = 1] and substitute N for m (because
// we know that the maximum sum obtained will have [m = d] and [n ≥ 1])
// to get [2d² + 2m - L = 0].

// Solving this quadratic equation gives [d=865], so we will generate
// the Farey sequence with denominators less than or equal to 865. We
// validate this choice experimentally by noticing any [d ≥ 865]
// results in the same answer but any [d ≤ 850] does not. So, we just
// generate the Farey sequence (filtering out double-odd pairs), and
// derive from each numerator/denominator pair the corresponding
// primitive Pythagorean triple (and all of its multiples that are ≤
// L) and record its sum (actually, just increment a counter variable
// associated with the sum value).

#include <cmath>
#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;

vector<pair<uint, uint>> farey(uint N) {
  vector<pair<uint, uint>> result;
  
  // 0/1, 1/n
  uint a = 0;
  uint b = 1;
  uint c = 1;
  uint d = N;

  while (c < N) {
    if (c % 2 == 0 || d % 2 == 0) {
      result.push_back({c, d});
    }
    
    uint p = floor((N + b) / d) * c - a;
    uint q = floor((N + b) / d) * d - b;

    a = c;
    b = d;
    c = p;
    d = q;
  }

  result.pop_back();
  return result;
}

constexpr uint L = 1500000;

int main() {
  // cout << f(100000) << endl;
  auto coprime_pairs = farey(865);
  // auto coprime_pairs = f(10);

  // Map sums to the number of Pythagorean triples that sum to them.
  unordered_map<uint, uint> sum_counts;
  uint count = 0;
  
  for (const auto p : coprime_pairs) {
    // cout << p.first << ", " << p.second << endl;
    uint n = p.first;
    uint m = p.second;
    
    uint a0 = m*m - n*n;
    uint b0 = 2 * m * n;
    uint c0 = m*m + n*n;
    // cout << a << " + " << b << " + " << c << " = " << a+b+c << endl;

    uint a = a0;
    uint b = b0;
    uint c = c0;

    for (uint i = 1; a+b+c <= L; i++) {
      // cout << a << " + " << b << " + " << c << " = " << a+b+c << endl;
      count++;
      sum_counts[a+b+c]++;
      
      a += a0;
      b += b0;
      c += c0;
    }
  }

  // cout << coprime_pairs.size() << endl;
  // cout << count << endl;

  count = 0;
  for (const auto p : sum_counts) {
    if (p.second == 1) {
      // cout << p.first << endl;
      count++;
    }
  }
  cout << count << endl;

  cout << sum_counts[120] << endl;
}
