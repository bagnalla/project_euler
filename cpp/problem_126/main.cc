// This one took me a while. First I implemented a brute force
// construction that built the layers atop one another as points
// stored in unordered_sets. This was correct but *way* too slow. By
// inspecting some output I could tell that after the first two
// layers, the delta between subsequent layers was increasing by
// exactly 8.

// I worked out by hand closed-form expressions for the
// number of blocks in the first and second layers. Then the
// subsequent layer counts can be obtained in sequence by starting
// with from l2 with a delta of (l2-l1) and adding 8 to the delta
// every time.

// I also came up with a general closed-form formula for the nth layer
// but it seems faster to use the method described above. The file
// 'backup.cc' contains all the code from those initial experiments.

// Runs in about 5s in my Linux VM.

#include <iostream>

using namespace std;

constexpr uint layer1_count(uint a) {
  return 4*a + 2;
}

constexpr uint layer1_count(uint a, uint b) {
  return 2 * (a*b + a + b);
}

constexpr uint layer1_count(uint a, uint b, uint c) {
  return 2 * (a*b + a*c + b*c);
}

uint C(uint n) {
  uint count = 0;
  
  // Check all combinations of (a, b, c)-cuboids that could possibly
  // have a layer with length n.
  for (uint a = 1; layer1_count(a) <= n; a++) {
    for (uint b = 1; b <= a && layer1_count(a, b) <= n; b++) {
      for (uint c = 1; c <= b && layer1_count(a, b, c) <= n; c++) {
        // Compute the number of blocks in the first and second layers.
        auto l1_count = layer1_count(a, b, c);
        auto l2_count = l1_count + 4 * (a + b + c);
        
        // Add them to the running count if necessary.
        count += (l1_count == n) + (l2_count == n);

        // Subsequent layers can be found by stepping from here,
        // adding 8 each time to the step quantity.
        uint step = l2_count - l1_count + 8;
        uint k = l2_count + step;

        // Keep stepping until the layer block count exceeds or is
        // equal to the target value n.
        while (k < n) {
          step += 8;
          k += step;
        }

        // Add to the running count if exactly equal to n
        count += k == n;        
      }
    }
  }
  
  return count;
}

int main() {
  for (uint n = 0; ; n += 2) {
    auto c = C(n);
    if (c == 1000) {
      cout << n << endl;
      break;
    }
  }
}
