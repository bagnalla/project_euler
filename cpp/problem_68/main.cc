// This solution (using a backtracking search approach) is a bit
// complicated but performs well and generalizes up to 10-gon (N=20)
// before starting to slow down (11-gon with N=22 takes around 30 seconds).

#include <bitset>
#include <cmath>
#include <iostream>
#include <optional>
#include <vector>
using namespace std;

constexpr uint N = 22;

template <typename T>
inline vector<T> concat(vector<T> a, const vector<T>& b) {
  a.insert(a.end(), b.cbegin(), b.cend());
  return a;
}

// Recursively find solutions.
vector<vector<uint>> solve(uint target_sum,
                           uint first_i,
                           uint first_j,
                           uint j,
                           bitset<N> bits) {
  if (bits.count() < 3) {
    return {};
  }
  
  vector<vector<uint>> solutions;
  
  for (uint i = 1; i <= N; i++) {
    if (i <= first_i || i + j >= target_sum) {
      continue;
    }
    const uint k = target_sum - (i + j);
    if (i != k && bits[i-1] && bits[k-1]) {
      bits[i-1] = false;
      bits[k-1] = false;

      if (bits.count() == 1) {
        // Handle last triplet as a special case.
        const auto i2 = (uint)log2(bits.to_ulong()) + 1;
        const auto j2 = k;
        const auto k2 = first_j;
        if (i2 >= first_i && i2 + j2 + k2 == target_sum) {
          // Produce and return single solution vector with last and
          // second-to-last triplets.
          return {vector<uint>{i, j, k, i2, j2, k2}};
        } else {
          bits[i-1] = true;
          bits[k-1] = true;
          continue;
        }
      } else {
        auto sols = solve(target_sum, first_i, first_j, k, bits);
        for (vector<uint>& sol : sols) {
          sol.insert(sol.begin(), k);
          sol.insert(sol.begin(), j);
          sol.insert(sol.begin(), i);
        }
        solutions = concat(solutions, sols);
      }
        
      bits[i-1] = true;
      bits[k-1] = true;
    }
  }
  
  return solutions;
}

int main() {
  vector<vector<uint>> solutions;
  
  bitset<N> bits;
  bits.set();
  
  for (uint i = 1; i <= N; i++) {
    bits[i-1] = false;
    for (uint j = 1; j <= N; j++) {
      if (j != i) {
        bits[j-1] = false;
        for (uint k = 1; k <= N; k++) {
          if (k != i && k != j) {
            auto target_sum = i + j + k;
            bits[k-1] = false;
            auto sols = solve(target_sum, i, j, k, bits);
            for (vector<uint>& sol : sols) {
              sol.insert(sol.begin(), k);
              sol.insert(sol.begin(), j);
              sol.insert(sol.begin(), i);
            }
            solutions = concat(solutions, sols);
            bits[k-1] = true;
          }
        }
        bits[j-1] = true;
      }
    }
    bits[i-1] = true;
  }

  // for (const auto sol : solutions) {
  //   for (const auto x : sol) {
  //     cout << x << " ";
  //   }
  //   cout << endl;
  // }

  if (solutions.size()) {
    for (const auto x : solutions[solutions.size()-1]) {
      cout << x << "";
    }
    cout << endl;
  }
}
