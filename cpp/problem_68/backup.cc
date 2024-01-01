#include <bitset>
#include <cmath>
#include <iostream>
#include <optional>
#include <vector>
using namespace std;

constexpr uint N = 10;

template <typename T>
inline vector<T> concat(vector<T> a, const vector<T>& b) {
  a.insert(a.end(), b.cbegin(), b.cend());
  return a;
}

// optional<uint> sum_of_bottom_three(bitset<N> bits) {
//   uint count = 0;
//   uint sum = 0;
//   for (uint i = 0; i < N; i++) {
//     if (bits[i]) {
//       sum += i + 1;
//       if (++count == 3) {
//         return {sum};
//       }
//     }
//   }
//   return {};
// }

// optional<uint> sum_of_top_three(bitset<N> bits) {
//   uint count = 0;
//   uint sum = 0;
//   for (uint i = N-1; i >= 0; i--) {
//     if (bits[i]) {
//       sum += i + 1;
//       if (++count == 3) {
//         return {sum};
//       }
//     }
//   }
//   return {};
// }

// uint sum_of_bottom_three(bitset<N> bits) {
//   uint count = 0;
//   uint sum = 0;
//   for (uint i = 1; i <= N; i++) {
//     if (bits[i-1]) {
//       sum += i;
//       if (++count == 3) {
//         return sum;
//       }
//     }
//   }
//   return N*N;
// }

// uint sum_of_top_three(bitset<N> bits) {
//   uint count = 0;
//   uint sum = 0;
//   for (uint i = N; i > 0; i--) {
//     if (bits[i-1]) {
//       sum += i;
//       if (++count == 3) {
//         return sum;
//       }
//     }
//   }
//   return 0;
// }

uint sum_of_bottom_two(bitset<N> bits) {
  uint count = 0;
  uint sum = 0;
  for (uint i = 1; i <= N; i++) {
    if (bits[i-1]) {
      sum += i;
      if (++count == 2) {
        return sum;
      }
    }
  }
  return N*N;
}

uint sum_of_top_two(bitset<N> bits) {
  uint count = 0;
  uint sum = 0;
  for (uint i = N; i > 0; i--) {
    if (bits[i-1]) {
      sum += i;
      if (++count == 2) {
        return sum;
      }
    }
  }
  return 0;
}

// vector<vector<uint>> solve(uint target_sum,
//                            uint first_mid,
//                            uint prev_last,
//                            bitset<N> bits) {
//   // cout << bits.count() << endl;
//   if (bits.count() < 3) {
//     return {};
//   }
  
//   vector<vector<uint>> solutions;
//   const uint j = prev_last;
  
//   for (uint i = 0; i < N; i++) {
//     // cout << "i: " << i+1 << endl;
//     // cout << "j: " << j+1 << endl;
//     if (i + j + 2 >= target_sum) {
//       continue;
//     }
//     const uint k = target_sum - (i + j + 2) - 1;
//     // cout << "k: " << k+1 << endl;
//     if (bits[i] && bits[k]) {
//       bits[i] = false;
//       bits[k] = false;

//       // cout << "i: " << i+1 << endl;
//       // cout << "k: " << k+1 << endl;
//       // cout << bits.count() << endl;

//       if (bits.count() == 1) {
//         // TODO: handle last triplet as a special case.
//         // cout << prev_last+1 + first_mid+1 + log2(bits.to_ulong()) << endl;
//         const auto i2 = (uint)log2(bits.to_ulong());
//         const auto j2 = k;
//         const auto k2 = first_mid;
//         // cout << "i2: " << i2+1 << endl;
//         // cout << "j2: " << j2+1 << endl;
//         // cout << "k2: " << k2+1 << endl;
//         if (i2+1 + j2+1 + k2+1 == target_sum) {
//           // TODO: produce and return single solution vector with last
//           // and second-to-last triplets.
//           return {vector<uint>{i+1, j+1, k+1, i2+1, j2+1, k2+1}};
//         } else {
//           bits[i] = true;
//           bits[k] = true;
//           continue;
//         }
//       } else {
//         auto min_sum = sum_of_bottom_three(bits);
//         auto max_sum = sum_of_top_three(bits);
//         if (!(min_sum > target_sum || max_sum < target_sum)) {
//           auto sols = solve(target_sum, first_mid, k, bits);
//           for (auto sol : sols) {
//             sol.insert(sol.begin(), k);
//             sol.insert(sol.begin(), prev_last);
//             sol.insert(sol.begin(), i);
//           }
//           solutions = concat(solutions, sols);
//         }
//       }
        
//       bits[i] = true;
//       bits[k] = true;
//     }
//   }
  
//   return solutions;
// }

vector<vector<uint>> solve(uint target_sum,
                           uint first_i,
                           uint first_j,
                           uint j,
                           bitset<N> bits) {
  // cout << bits.count() << endl;
  if (bits.count() < 3) {
    return {};
  }
  
  vector<vector<uint>> solutions;
  
  for (uint i = 1; i <= N; i++) {
    // cout << "i: " << i << endl;
    // cout << "j: " << j << endl;
    if (i <= first_i || i + j >= target_sum) {
      continue;
    }
    const uint k = target_sum - (i + j);
    // cout << "k: " << k << endl;
    if (i != k && bits[i-1] && bits[k-1]) {
      bits[i-1] = false;
      bits[k-1] = false;

      // cout << "i: " << i << endl;
      // cout << "j: " << j << endl;
      // cout << "k: " << k << endl;
      // cout << bits.count() << endl;
      // if (bits.count() <= 3) {
      //   cout << bits.count() << endl;
      // }

      if (bits.count() == 1) {
        // Handle last triplet as a special case.
        // cout << prev_last+1 + first_mid+1 + log2(bits.to_ulong()) << endl;
        const auto i2 = (uint)log2(bits.to_ulong()) + 1;
        const auto j2 = k;
        const auto k2 = first_j;
        // cout << "i2: " << i2+1 << endl;
        // cout << "j2: " << j2+1 << endl;
        // cout << "k2: " << k2+1 << endl;
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
        auto min_sum = sum_of_bottom_two(bits) + k;
        auto max_sum = sum_of_top_two(bits) + k;
        // cout << "target_sum: " << target_sum << endl;
        // cout << "min_sum: " << min_sum << endl;
        // cout << "max_sum: " << max_sum << endl;
        if (!(min_sum > target_sum || max_sum < target_sum)) {
          auto sols = solve(target_sum, first_i, first_j, k, bits);
          for (vector<uint>& sol : sols) {
            sol.insert(sol.begin(), k);
            sol.insert(sol.begin(), j);
            sol.insert(sol.begin(), i);
          }
          solutions = concat(solutions, sols);
        }
      }
        
      bits[i-1] = true;
      bits[k-1] = true;
    }
  }
  
  return solutions;
}

int main() {
  vector<vector<uint>> solutions;

  uint count = 0;
  
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
            auto min_sum = sum_of_bottom_two(bits) + k;
            auto max_sum = sum_of_top_two(bits) + k;
            if (min_sum > target_sum || max_sum < target_sum) {
              continue;
            }
            // cout << i << " " << j << " " << k << endl;
            count++;
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

  // uint i = 3;
  // uint j = 7;
  // uint k = 6;
  // bits[i-1] = false;
  // bits[j-1] = false;
  // bits[k-1] = false;
  // auto sols = solve(i+j+k, i, j, k, bits);
  // for (const auto sol : sols) {
  //   cout << i << " " << j << " " << k << " ";
  //   for (const auto x : sol) {
  //     cout << x << " ";
  //   }
  //   cout << endl;
  // }

  // bitset<4> bs{"1000"};
  // cout << log2(bs.to_ulong()) << endl;
}

// vector<vector<uint>> solve(uint target_sum,
//                            uint first_mid,
//                            uint prev_mid,
//                            bitset<10> bits,
//                            uint depth) {
//   for (uint i = 1; i <= 10; i++) {
//     if (bits[i-1] && bits[prev_mid-1 + i-1]) {
      
//       bits[i-1] = false;
      
//       bits[i-1] = true;
//     }
//   }
//   return {}; // TODO
// }

// int main() {
//   vector<vector<uint>> solutions;

//   uint count = 0;
  
//   bitset<10> bits{"1111111111"};
//   for (uint i = 1; i <= 10; i++) {
//     bits[i-1] = false;
//     for (uint j = 1; j <= 10; j++) {
//       if (j != i) {
//         bits[j-1] = false;
//         for (uint k = 1; k <= 10; k++) {
//           if (k != i && k != j) {
//             auto target_sum = i + j + k;
//             auto min_sum = sum_of_bottom_three(bits);
//             auto max_sum = sum_of_top_three(bits);
//             if (min_sum > target_sum || max_sum < target_sum) {
//               continue;
//             }
//             cout << i << " " << j << " " << k << endl;
//             count++;
//             bits[k-1] = false;
//             solutions = concat(solutions, solve(target_sum, j, j, bits, 4));
//             bits[k-1] = true;
//           }
//         }
//         bits[j-1] = true;
//       }
//     }
//     bits[i-1] = true;
//   }

//   cout << count << endl;
// }
