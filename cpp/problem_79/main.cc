// This is the dumbest brute force solution possible, pretty
// much. Enumerate all numbers with at most 8 digits and check for
// each one if all the 3-digit codes are subsequences of it. We are
// lucky enough that the answer has only 8 digits and not 10 or more,
// else this solution would be too slow.

// Mohammad Sarabi gives a pretty nice algorithm in the problem
// thread: We want a mapping from digits to the sets of digits that
// follow them (e.g., 2 -> {4, 5, 8} to mean that the 4, 5, and 8 must
// follow 2). We build the mapping by iterating over all the
// three-digit codes [abc] and unioning {b, c} to the "follows" set of
// 'a' and {c} to the follows set of 'b'. Afterward, simply print out
// the keys of this mapping in reverse order of the size of their
// follows sets (might need a bit of logic in the general case to
// ensure that the follows order is respected.

#include <iostream>
#include <string>
using namespace std;

static uint dat[] = { 319, 680, 180, 690, 129, 620, 762, 689, 762, 318,
  368, 710, 720, 710, 629, 168, 160, 689, 716, 731, 736, 729, 316, 729,
  729, 710, 769, 290, 719, 680, 318, 389, 162, 289, 162, 718, 729, 319,
  790, 680, 890, 362, 319, 760, 316, 729, 380, 319, 728, 716 };

// Is string 's1' a subsequence of string 's2'?
bool is_subsequence(const string& s1, const string& s2) {
  uint i = 0;
  for (const char c : s2) {
    if (i == s1.length()) {
      break;
    } else if (s1[i] == c) {
      i++;
    }
  }
  return i == s1.length();
}

int main() {
  static string strings[50];
  for (uint i = 0; i < 50; i++) {
    strings[i] = to_string(dat[i]);
  }

  for (uint i = 0; i < 10000000000; i++) {
    const string si = to_string(i);
    for (const string& s : strings) {
      if (!is_subsequence(s, si)) {
        goto next;
      }
    }
    cout << i << endl;
    break;
    next: {}
  }
}
