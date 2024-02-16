#include <iostream>
#include <unordered_set>

using namespace std;

struct Point {
  int x;
  int y;
  int z;
  auto operator<=>(const Point&) const = default;
};

// inline void hash_combine(std::size_t& seed) { }

// template <typename T, typename... Rest>
// inline void hash_combine(std::size_t& seed, const T& v, Rest... rest) {
//   std::hash<T> hasher;
//   seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
//   hash_combine(seed, rest...);
// }

// #define MAKE_HASHABLE(type, ...)                        \
//   namespace std {                                       \
//     template<> struct hash<type> {                      \
//       std::size_t operator()(const type &t) const {     \
//         std::size_t ret = 0;                            \
//         hash_combine(ret, __VA_ARGS__);                 \
//         return ret;                                     \
//       }                                                 \
//     };                                                  \
//   }

// MAKE_HASHABLE(Point, t.x, t.y, t.z)

template <class T>
constexpr void hash_combine(std::size_t& seed, const T& v)
{
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

namespace std {
  template <> struct hash<Point> {
    size_t operator()(const Point &p) const {
      std::size_t h = 0;
      hash_combine(h, p.x);
      hash_combine(h, p.y);
      hash_combine(h, p.z);
      return h;
    }
  };
}

constexpr uint layer1_count(uint a) {
  return 4*a + 2;
}

constexpr uint layer1_count(uint a, uint b) {
  // return 2*a*b + 2*a + 2*b;
  return 2 * (a*b + a + b);
}

constexpr uint layer1_count(uint a, uint b, uint c) {
  // return 2*a*b + 2*a*c + 2*b*c;
  return 2 * (a*b + a*c + b*c);
}

// constexpr uint layer2_count(uint a, uint b, uint c) {
//   return 2 * (layer1_count(a, b, 1) - a*b)
//     + 2 * (layer1_count(a, 1, c) - a*c)
//     + 2 * (layer1_count(1, b, c) - b*c)
//     - 4 * (a + b + c);
// }

constexpr uint layer2_count(uint a, uint b, uint c) {
  return 2 * (a*b + a*c + b*c) + 4 * (a + b + c);
}

constexpr uint layer3_count(uint a, uint b, uint c) {
  // return layer2_count(a, b, c) + 4 * (a + b + c);
  // const auto l1 = layer1_count(a, b, c);
  // const auto l2 = l1 + 4 * (a + b + c);
  // return 2 * l2 - l1 + 8;

  // return 2 * (l1 + 4 * (a + b + c)) - l1 + 8;
  // return l1 + 8 * (a + b + c) + 8;
  // return l1 + 8 * (a + b + c + 1);
  // return l1 + 8 * (a + b + c + 1);
  
  return 2 * (a*b + a*c + b*c) + 8 * (a + b + c) + 8;
}

constexpr uint layer4_count(uint a, uint b, uint c) {
  // return layer2_count(a, b, c) + 4 * (a + b + c);
  // const auto l1 = 2 * (a*b + a*c + b*c);
  // const auto l2 = l1 + 4 * (a + b + c);
  // const auto l3 = l2 + (l2 - l1) + 8;
  // // const auto l4 = l3 + (l3 - l2) + 8;
  // const auto l4 = 2 * l3 - l2 + 8;
  // return l4;

  // return 2 * (l2 + (l2 - l1) + 8) - l2 + 8;
  // return 2 * (2*l2 - l1 + 8) - l2 + 8;
  // return 4*l2 - 2*l1 + 2*8 - l2 + 8;
  // return 3*l2 - 2*l1 + 3*8;
  // return 3*(l1 + 4 * (a + b + c)) - 2*l1 + 3*8;
  // return 3*l1 + 12 * (a + b + c) - 2*l1 + 24;
  // return l1 + 12 * (a + b + c) + 24;
  return 2 * (a*b + a*c + b*c) + 12 * (a + b + c) + 24;
}

constexpr uint layern_count(uint a, uint b, uint c, uint n) {
  return 2 * (a*b + a*c + b*c) + 4*(n-1)*(a + b + c) + 16*(max(0, (int)n-3)) + 8;
}

// constexpr uint layern_count(uint a, uint b, uint c, uint n) {
//   if (n == 1) {
//     return layer1_count(a, b, c);
//   }
//   if (n == 2) {
//     return layer2_count(a, b, c);
//   }
//   const auto l1 = layern_count(a, b, c, n-2);
//   const auto l2 = layern_count(a, b, c, n-1);
//   return 2 * l2 - l1 + 8;
// }

// constexpr uint layern_count(uint a, uint b, uint c, uint n) {
//   const auto l1 = layer1_count(a, b, c);
//   const auto l2 = l1 + 4 * (a + b + c);
//   return l2 + (n-2) * (l2 - l1) + (n-2) * 8;
// }

unordered_set<Point> cuboid(uint a, uint b, uint c) {
  unordered_set<Point> points;
  for (int i = 0; i < a; i++) {
    for (int j = 0; j < b; j++) {
      for (int k = 0; k < c; k++) {
        points.insert(Point{i, j, k});
      }
    }
  }
  return points;
}

unordered_set<Point> next_layer(const unordered_set<Point> &points,
                                const unordered_set<Point> &prev_layer) {
  unordered_set<Point> layer;
  for (const auto p : prev_layer) {
    if (!points.contains(Point{p.x-1, p.y, p.z})) {
      layer.insert(Point{p.x-1, p.y, p.z});
    }
    if (!points.contains(Point{p.x+1, p.y, p.z})) {
      layer.insert(Point{p.x+1, p.y, p.z});
    }
    if (!points.contains(Point{p.x, p.y-1, p.z})) {
      layer.insert(Point{p.x, p.y-1, p.z});
    }
    if (!points.contains(Point{p.x, p.y+1, p.z})) {
      layer.insert(Point{p.x, p.y+1, p.z});
    }
    if (!points.contains(Point{p.x, p.y, p.z-1})) {
      layer.insert(Point{p.x, p.y, p.z-1});
    }
    if (!points.contains(Point{p.x, p.y, p.z+1})) {
      layer.insert(Point{p.x, p.y, p.z+1});
    }
  }
  return layer;
}

uint C(uint n) {
  uint count = 0;
  for (uint a = 1; layer1_count(a) <= n; a++) {
    for (uint b = 1; b <= a && layer1_count(a, b) <= n; b++) {
      for (uint c = 1; c <= b && layer1_count(a, b, c) <= n; c++) {
        unordered_set<Point> points = cuboid(a, b, c);
        auto layer = next_layer(points, points);
        points.insert(layer.begin(), layer.end());
        while (layer.size() < n) {
          layer = next_layer(points, layer);
          points.insert(layer.begin(), layer.end());
        }
        count += layer.size() == n;
      }
    }
  }
  return count;
}

uint C2(uint n) {
  uint count = 0;
  for (uint a = 1; layer1_count(a) <= n; a++) {
    for (uint b = 1; b <= a && layer1_count(a, b) <= n; b++) {
      for (uint c = 1; c <= b && layer1_count(a, b, c) <= n; c++) {

        // unordered_set<Point> points = cuboid(a, b, c);
        // auto layer1 = next_layer(points, points);
        // points.insert(layer1.begin(), layer1.end());
        // auto layer2 = next_layer(points, layer1);

        // points.insert(layer2.begin(), layer2.end());
        // auto layer3 = next_layer(points, layer2);
        // count += (layer1.size() == n) + (layer2.size() == n) + (layer3.size() == n);
        // uint step = layer3.size() - layer2.size() + 8;
        // uint k = layer3.size() + step;

        // count += (layer1.size() == n) + (layer2.size() == n);
        // uint step = layer2.size() - layer1.size() + 8;
        // uint k = layer2.size() + step;

        auto l1_count = 2 * (a*b + a*c + b*c);
        auto l2_count = l1_count + 4 * (a + b + c);
        count += (l1_count == n) + (l2_count == n);
        uint step = l2_count - l1_count + 8;
        uint k = l2_count + step;
        while (k < n) {
          step += 8;
          k += step;
        }
        count += k == n;

        // count += (2*(a*b + a*c + b*c) - 40 - n - 4*(a + b + c)) % (-4*(a + b + c + 4)) == 0;
        
      }
    }
  }
  return count;
}

int main() {
  
  // unordered_set<Point> l1 = cuboid(3, 2, 1);
  // unordered_set<Point> points(l1);
  // cout << l1.size() << endl;
  // auto l2 = next_layer(points, l1);
  // points.insert(l2.begin(), l2.end());
  // cout << l2.size() << endl;
  // auto l3 = next_layer(points, l2);
  // points.insert(l3.begin(), l3.end());
  // cout << l3.size() << endl;
  // auto l4 = next_layer(points, l3);
  // points.insert(l4.begin(), l4.end());
  // cout << l4.size() << endl;
  // auto l5 = next_layer(points, l4);
  // points.insert(l5.begin(), l5.end());
  // cout << l5.size() << endl;

  for (uint n = 0; ; n += 2) {
    // auto c = C(n);
    auto c = C2(n);
    // cout << n << " " << c << endl;
    if (c == 10) {
      cout << n << endl;
      break;
    }
  }

  // uint sum = 0;
  // for (uint n = 0; n < 50000; n += 2) {
  //   // auto c = C(n);
  //   auto c = C2(n);
  //   sum += c;
  //   // cout << n << " " << c << endl;
  // }
  // cout << sum << endl;

  // cout << C(118) << endl;

  // for (uint a = 1; a <= 5; a++) {
  //   for (uint b = 1; b <= a; b++) {
  //     for (uint c = 1; c <= b; c++) {
  //       cout << a << " " << b << " " << c << ": ";
  //       unordered_set<Point> points = cuboid(a, b, c);
  //       auto layer = next_layer(points, points);
  //       // cout << layer.size() << " ";
  //       // cout << layer.size() - points.size() << " ";
  //       points.insert(layer.begin(), layer.end());
  //       auto prev_layer_size = layer.size();
  //       for (uint i = 0; i < 3; i++) {
  //         cout << layer.size() << " ";
  //         layer = next_layer(points, layer);
  //         // cout << layer.size() - prev_layer_size << " ";
  //         prev_layer_size = layer.size();
  //         points.insert(layer.begin(), layer.end());
  //       }
  //       cout << layer.size();
  //       cout
  //         // << ", " << layer1_count(a, b, c)
  //         // << " " << layer2_count(a, b, c)
  //         // << " " << layer3_count(a, b, c)
  //         // << " " << layer4_count(a, b, c)
  //         << ", " << layern_count(a, b, c, 1)
  //         << " " << layern_count(a, b, c, 2)
  //         << " " << layern_count(a, b, c, 3)
  //         << " " << layern_count(a, b, c, 4)
  //         << endl;
  //       // cout << endl;
  //     }
  //   }
  // }

}
