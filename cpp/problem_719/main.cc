#include <cmath>
#include <iostream>
#include <mutex>
#include <string>
#include <vector>
using namespace std;

// Lookup table for powers of 10.
std::vector<unsigned long long int> pows;
void init_pows() {
  for (size_t i = 0; i < 13; i++) {
    pows.push_back(pow(10, i));
  }
}

vector<unsigned long long int> sums(unsigned long long n) {
  vector<unsigned long long int> result = { n };
  if (n < 10) {
    return result;
  }
  auto logn = log10(n); // Apparently doing this saves several seconds.
  for (size_t i = 1; i < logn; i++) {
    unsigned long long int x = n / pows[i];
    auto ys = sums(n % pows[i]);
    for (size_t j = 0; j < ys.size(); j++) {
      ys[j] += x;
    }
    result.insert(result.end(), ys.begin(), ys.end());
 }
  return result;
}

// Is n^2 an S-number?
bool is_S_number(unsigned long long int n) {
  auto m = n*n;
  if (m < 10) {
    return false;
  } else {
    auto ns = sums(m);
    for (size_t i = 1; i < ns.size(); i++) {
      if (ns[i] == n) {
        return true;
      }
    }
    return false;
  }
}

int main() {
  init_pows();
  std::mutex sum_mutex;
  unsigned long long int sum = 0;
#pragma omp parallel for num_threads(16)
  for (unsigned long long int i = 1; i <= 1000000; i++) {
    // for (int i = 1; i <= 100; i++) {
    if (is_S_number(i)) {
      const lock_guard<mutex> lock(sum_mutex);
      sum += i*i;
    }
  }
  cout << sum << endl;
}
