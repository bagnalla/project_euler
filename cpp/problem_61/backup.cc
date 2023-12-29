#include <bitset>
#include <cmath>
#include <functional>
#include <optional>
#include <iostream>
#include <vector>
using namespace std;

// Forward mapping from natural numbers to triangle numbers.
double triangle_f(int n) {
  return n * (n + 1) / 2;
}

// Reverse mapping from triangle numbers to natural numbers.
double triangle_g(int p) {
  return sqrt((1 + 8*p) / 4.0) - 0.5;
}

// Forward mapping from natural numbers to square numbers.
double square_f(int n) {
  return n * n;
}

// Reverse mapping from square numbers to natural numbers.
double square_g(int p) {
  return sqrt(p);
}

// Forward mapping from natural numbers to pentagon numbers.
double pentagon_f(int n) {
  return n * (3*n - 1) / 2.0;
}

// Reverse mapping from pentagon numbers to natural numbers.
double pentagon_g(int p) {
  return (1 + sqrt(1 + 24*p)) / 6;
}

// Forward mapping from natural numbers to hexagon numbers.
double hexagon_f(int n) {
  return n * (2*n - 1);
}

// Reverse mapping from hexagon numbers to natural numbers.
double hexagon_g(int p) {
  return (1 + sqrt(1 + 8*p)) / 4;
}

// Forward mapping from natural numbers to heptagon numbers.
double heptagon_f(int n) {
  return n * (5*n - 3) / 2.0;
}

// Reverse mapping from heptagon numbers to natural numbers.
double heptagon_g(int p) {
  return (1.5 + sqrt(9/4.0 + 10*p)) / 5;
}

// Forward mapping from natural numbers to octagon numbers.
double octagon_f(int n) {
  return n * (3*n - 2);
}

// Reverse mapping from octagon numbers to natural numbers.
double octagon_g(int p) {
  return (2 + sqrt(4 + 12*p)) / 6;
}

void tests() {
  for (int i = 1; i < 5000; i++) {
    if (triangle_g(triangle_f(i)) != i) {
      cout << "BAD: " << i << endl;
      cout << "triangle_f(i): " << triangle_f(i) << endl;
      cout << "triangle_g(triangle_f(i)): " << triangle_g(triangle_f(i)) << endl;
    }
    if (square_g(square_f(i)) != i) {
      cout << "BAD: " << i << endl;
      cout << "square_f(i): " << square_f(i) << endl;
      cout << "square_g(square_f(i)): " << square_g(square_f(i)) << endl;
    }
    if (pentagon_g(pentagon_f(i)) != i) {
      cout << "BAD: " << i << endl;
      cout << "pentagon_f(i): " << pentagon_f(i) << endl;
      cout << "pentagon_g(pentagon_f(i)): " << pentagon_g(pentagon_f(i)) << endl;
    }
    if (hexagon_g(hexagon_f(i)) != i) {
      cout << "BAD: " << i << endl;
      cout << "hexagon_f(i): " << hexagon_f(i) << endl;
      cout << "hexagon_g(hexagon_f(i)): " << hexagon_g(hexagon_f(i)) << endl;
    }
    if (heptagon_g(heptagon_f(i)) != i) {
      cout << "BAD: " << i << endl;
      cout << "heptagon_f(i): " << heptagon_f(i) << endl;
      cout << "heptagon_g(heptagon_f(i)): " << heptagon_g(heptagon_f(i)) << endl;
    }
    if (octagon_g(octagon_f(i)) != i) {
      cout << "BAD: " << i << endl;
      cout << "octagon_f(i): " << octagon_f(i) << endl;
      cout << "octagon_g(octagon_f(i)): " << octagon_g(octagon_f(i)) << endl;
    }
  }
}

// template <typename T>
// optional<T> any(vector<function<void(optional<T>)>> fs) {
//   for (auto f : fs) {
//     auto o = f();
//     if (o.has_value()) {
//       return o.value();
//     }
//   }
//   return {};
// }

optional<vector<int>> run(int final_prefix, int cur_prefix, bitset<5> flags) {
  if (flags.all()) {
    if (final_prefix * 100 == cur_prefix) {
      return {{}};
    } else {
      return {};
    }
  }

  vector<pair<function<double(int)>, function<double(int)>>> fs = {
    {square_f, square_g},
    {pentagon_f, pentagon_g},
    {hexagon_f, hexagon_g},
    {heptagon_f, heptagon_g},
    {octagon_f, octagon_g}
  };

  // cout << "cur_prefix: " << cur_prefix << endl;

  for (int k = 0; k < fs.size(); k++) {
    if (!flags[k]) {
      for (int i = ceil(fs[k].second(cur_prefix)); fs[k].first(i) < cur_prefix + 100; i++) {
        int n = fs[k].first(i);
        // cout << "n: " << n << endl;
        int cur_prefix = n % 100 * 100;
        if (cur_prefix < 1000) {
          continue;
        }
        flags[k] = true;
        auto result = run(final_prefix, cur_prefix, flags);
        flags[k] = false;
        if (result.has_value()) {
          auto v = result.value();
          v.insert(v.begin(), n);
          return {v};
        }
      }
    }
  }
  
  // // Try squares.
  // if (!flags[0]) {
  //   for (int i = ceil(square_g(cur_prefix)); square_f(i) < cur_prefix + 100; i++) {
  //     int n = square_f(i);
  //     int cur_prefix = n % 100 * 100;
  //     if (cur_prefix < 1000) {
  //       continue;
  //     }
  //     // cout << "square: " << n << endl;
  //     flags[0] = true;
  //     auto result = run(final_prefix, cur_prefix, flags);
  //     flags[0] = false;
  //     if (result.has_value()) {
  //       auto v = result.value();
  //       v.insert(v.begin(), n);
  //       // cout << "square: " << n << endl;
  //       // cout << "flags: " << flags << endl;
  //       return {v};
  //     }
  //   }
  // }

  // // Try pentagons.
  // if (!flags[1]) {
  //   for (int i = ceil(pentagon_g(cur_prefix)); pentagon_f(i) < cur_prefix + 100; i++) {
  //     int n = pentagon_f(i);
  //     int cur_prefix = n % 100 * 100;
  //     if (cur_prefix < 1000) {
  //       continue;
  //     }
  //     // cout << "pentagon: " << n << endl;
  //     flags[1] = true;
  //     auto result = run(final_prefix, cur_prefix, flags);
  //     flags[1] = false;
  //     if (result.has_value()) {
  //       auto v = result.value();
  //       v.insert(v.begin(), n);
  //       // cout << "pentagon: " << n << endl;
  //       // cout << "flags: " << flags << endl;
  //       return {v};
  //     }
  //   }
  // }
  
  // // Try hexagons.
  // if (!flags[2]) {
  //   for (int i = ceil(hexagon_g(cur_prefix)); hexagon_f(i) < cur_prefix + 100; i++) {
  //     int n = hexagon_f(i);
  //     int cur_prefix = n % 100 * 100;
  //     if (cur_prefix < 1000) {
  //       continue;
  //     }
  //     // cout << "hexagon: " << n << endl;
  //     flags[2] = true;
  //     auto result = run(final_prefix, cur_prefix, flags);
  //     flags[2] = false;
  //     if (result.has_value()) {
  //       auto v = result.value();
  //       v.insert(v.begin(), n);
  //       // cout << "hexagon: " << n << endl;
  //       // cout << "flags: " << flags << endl;
  //       return {v};
  //     }
  //   }
  // }

  // // Try heptagons.
  // if (!flags[3]) {
  //   for (int i = ceil(heptagon_g(cur_prefix)); heptagon_f(i) < cur_prefix + 100; i++) {
  //     int n = heptagon_f(i);
  //     int cur_prefix = n % 100 * 100;
  //     if (cur_prefix < 1000) {
  //       continue;
  //     }
  //     // cout << "heptagon: " << n << endl;
  //     flags[3] = true;
  //     auto result = run(final_prefix, cur_prefix, flags);
  //     flags[3] = false;
  //     if (result.has_value()) {
  //       auto v = result.value();
  //       v.insert(v.begin(), n);
  //       // cout << "heptagon: " << n << endl;
  //       // cout << "flags: " << flags << endl;
  //       return {v};
  //     }
  //   }
  // }
  
  // // Try octagons.
  // if (!flags[4]) {
  //   for (int i = ceil(octagon_g(cur_prefix)); octagon_f(i) < cur_prefix + 100; i++) {
  //     int n = octagon_f(i);
  //     int cur_prefix = n % 100 * 100;
  //     if (cur_prefix < 1000) {
  //       continue;
  //     }
  //     // cout << "octagon: " << n << endl;
  //     flags[4] = true;
  //     auto result = run(final_prefix, cur_prefix, flags);
  //     flags[4] = false;
  //     if (result.has_value()) {
  //       auto v = result.value();
  //       v.insert(v.begin(), n);
  //       // cout << "octagon: " << n << endl;
  //       // cout << "flags: " << flags << endl;
  //       return {v};
  //     }
  //   }
  // }
  
  return {};
}

int main() {
  // tests();

  optional<vector<int>> result;

  for (int i = ceil(triangle_g(1000)); triangle_f(i) < 10000; i++) {
    int n = triangle_f(i);
    // cout << "triangle: " << n << endl;

    int final_prefix = n / 100;
    int cur_prefix = n % 100 * 100;
    if (cur_prefix < 1000) {
      continue;
    }

    result = run(final_prefix, cur_prefix, bitset<5>{"00000"});
    if (result.has_value()) {
      auto v = result.value();
      v.insert(v.begin(), n);
      result = {v};
      break;
    }
  }
  
  if (result.has_value()) {
    int sum = 0;
    for (auto x : result.value()) {
      cout << x << endl;
      sum += x;
    }
    cout << "sum: " << sum << endl;
  } else {
    cout << "no solution" << endl;
  }

  // cout << ((int)triangle_g(8256) == triangle_g(8256)) << endl;
  // cout << ((int)square_g(5625) == square_g(5625)) << endl;
  // cout << ((int)heptagon_g(2512) == heptagon_g(2512)) << endl;
  // cout << ((int)octagon_g(1281) == octagon_g(1281)) << endl;
  // cout << ((int)hexagon_g(8128) == hexagon_g(8128)) << endl;
  // cout << ((int)pentagon_g(2882) == pentagon_g(2882)) << endl;
}
