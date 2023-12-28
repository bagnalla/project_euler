// This solution is a little dumb but it works.

#include <algorithm>
#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;

bool isPrime(uint n) {
  if (n == 2) {
    return true;
  }
  for (uint i = 3; i*i <= n; i += 2) {
    if (n % i == 0) {
      return false;
    }
  }
  return true;
}

vector<vector<uint>> comb(int n, int k) {
  vector<vector<uint>> result;
  string bitmask(k, 1); // k leading 1's
  bitmask.resize(n, 0); // n-k trailing 0's
 
  do { // Print integers and permute bitmask.
    vector<uint> ixs;
    for (int i = 0; i < n; ++i) { // [0..n-1] integers.
      if (bitmask[i]) {
        ixs.push_back(i);
      }
    }
    result.push_back(ixs);
  } while (prev_permutation(bitmask.begin(), bitmask.end()));
  
  return result;
}

template <typename T>
bool contains(vector<T> v, T x) {
  return find(v.cbegin(), v.cend(), x) != v.cend();
}

int main() {
  unordered_map<uint, bool> seen;
  for (uint n = 3; ; n += 2) {
    cout << "seen[n]: " << seen[n] << endl;
    if (isPrime(n) && !seen[n]) {
      // seen[n] = true;
      // cout << n << endl;
      string s = to_string(n);
      for (uint i = 1; i < s.length(); i++) {
        vector<vector<uint>> ixs = comb(s.length(), i);
        for (vector<uint> js : ixs) {
          uint count = 0;
          for (char d = '0'; d <= '9'; d++) {
            if (d == '0' && contains(js, (uint)0)) {
              continue;
            }
            string copy = s;
            for (uint j : js) {
              copy[j] = d;
            }
            uint m = stoi(copy);
            if (isPrime(m)) {
              count++;
              cout << copy << endl;
              seen[m] = true;
            }
            if (count == 8) {
              // cout << n << endl;
              exit(0);
            }
          }
        }
      }
    }
  }
}
