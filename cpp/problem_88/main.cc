// Brute force BFS search. Runs in a little over a minute on my
// machine without parallelization, and around 13s when the outer loop
// is parallelized via openmp.

#include <iostream>
#include <mutex>
#include <optional>
#include <queue>
#include <unordered_set>
#include <vector>
using namespace std;

struct State {
  vector<uint> factors;
  uint sum;
};

uint product(const vector<uint>& v) {
  uint prod = 1;
  for (const auto x : v) {
    prod *= x;
  }
  return prod;
}

// https://stackoverflow.com/a/29855973
struct VectorHash {
  size_t operator()(const std::vector<uint>& v) const {
    std::hash<uint> hasher;
    size_t seed = 0;
    for (int i : v) {
      seed ^= hasher(i) + 0x9e3779b9 + (seed<<6) + (seed>>2);
    }
    return seed;
  }
};

optional<State> search(uint k) {
  queue<State> frontier;
  frontier.push(State{{2, 2}, k+2});

  unordered_set<vector<uint>, VectorHash> seen;
  
  while (!frontier.empty()) {
    State s = frontier.front();
    frontier.pop();
    uint prod = product(s.factors);
    
    if (prod == s.sum) {
      return {s};
    } else if (prod > s.sum) {
      continue;
    }

    for (uint i = 0; i < s.factors.size(); i++) {
      vector<uint> new_factors(s.factors);
      new_factors[i]++;
      sort(new_factors.begin(), new_factors.end());
      if (!seen.contains(new_factors)) {
	frontier.push(State{new_factors, s.sum+1});
	seen.insert(new_factors);
      }
    }
    
    if (s.factors.size() < k) {
      vector<uint> new_factors(s.factors);
      new_factors.push_back(2);
      sort(new_factors.begin(), new_factors.end());
      if (!seen.contains(new_factors)) {
	frontier.push(State{new_factors, s.sum+1});
	seen.insert(new_factors);
      }
    }
  }
  return {};
}

int main() {
  uint sum = 0;
  std::mutex sum_mutex;
  unordered_set<uint> seen;

#pragma omp parallel for num_threads(16) schedule(static, 100)
  for (uint k = 2; k <= 12000; k++) {
    auto s = search(k);
    if (s) {
      if (!seen.contains(s->sum)) {
	const lock_guard<mutex> lock(sum_mutex);
	sum += s->sum;
	seen.insert(s->sum);
      }
    } else {
      cout << "no solution for " << k << "." << endl;
    }
  }

  cout << sum << endl;
}
