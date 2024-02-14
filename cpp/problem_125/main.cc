// Straightforward brute force. Enumerate all sums of squares and add
// each one that is palindromic to the total. The only wrinkle (which
// had me stumped for a while) is that some numbers can be written as
// a sum of squares in more than one way, so we have to be careful not
// to count them more than once. We achieve this by just maintaining a
// 'seen' set and only adding a number to the total the first time we
// see it.

// Runs in about 23ms in my Linux VM.

#include <cmath>
#include <iostream>
#include <unordered_set>
#include <vector>

using namespace std;

constexpr uint N = pow(10, 8);

vector<uint> squares;

// Cute little way to compute squares that is slightly faster than the
// more obvious way.
void init_squares() {
  uint step = 1;
  for (uint n = 1; n < N; n += step) {
    squares.push_back(n);
    step += 2;
  }  
}

bool is_palindrome(uint n) {
  string s = to_string(n);
  for (uint i = 0; i < s.length() / 2; i++) {
    if (s[i] != s[s.length() - 1 - i]) {
      return false;
    }
  }
  return true;
}

int main() {
  init_squares();
  
  uint total = 0;
  unordered_set<uint> seen;
  
  for (uint i = 0; i < squares.size() - 1; i++) {
    uint sum = squares[i];
    for (uint j = i + 1; j < squares.size(); j++) {
      sum += squares[j];
      if (sum >= N) {
        break;
      }
      if (is_palindrome(sum) && !seen.contains(sum)) {
        seen.insert(sum);
        total += sum;
      }
    }
  }

  cout << total << endl;
}
