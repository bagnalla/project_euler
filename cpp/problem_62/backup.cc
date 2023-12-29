// This solution produces the correct result but takes over 7 minutes
// to run in the VM. Not quite good enough.

#include <cmath>
#include <iostream>
#include <unordered_set>
using namespace std;

bool isCube(unsigned long long int n) {
  static unordered_set<unsigned long long int> cache;
  static unsigned long long int k = 1;
  unsigned long long int cube = k*k*k;
  cache.emplace(cube);  
  if (n <= cube) {
    return cache.find(n) != cache.end();
  } else {
    while (cube < n) {
      k++;
      cube = k*k*k;
      cache.emplace(cube);
    }
    return cache.find(n) != cache.end();
  }
}

int main() {
  for (unsigned long long int i = 1;; i++) {
    const unsigned long long int n = i*i*i;
    string s = to_string(n);
    int count = 0;
    do {
      if (isCube(stoull(s))) {
        count++;
      }
    } while (next_permutation(s.begin(), s.end()));
    if (count > 2) {
      cout << n << endl;
      cout << count << endl;
    }
    if (count == 5) {
      exit(0);
    }
  }
}
