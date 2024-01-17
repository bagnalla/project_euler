// First started trying to come up with a clever method for counting
// but then realized that it could easily be brute forced by just
// running through all the possible triangles and checking if they
// have a right angle.

#include <cmath>
#include <iostream>
using namespace std;

struct vec2 {
  int x;
  int y;
  constexpr double len() const {
    return sqrt(this->x*this->x + this->y*this->y);
  }
  constexpr int dot(const vec2 &b) const {
    return this->x * b.x + this->y * b.y;
  }
  constexpr vec2 minus(const vec2 &b) const {
    return {this->x - b.x, this->y - b.y};
  }
  friend ostream& operator<<(ostream& os, const vec2& v) {
    os << "<" << v.x << ", " << v.y << ">";
    return os;
  }
};

double angle(const vec2& a, const vec2& b) {
  return acos(a.dot(b) / (a.len() * b.len()));
}

bool eq(double x, double y) {
  return abs(x-y) <= 0.0001;
}

bool is_right_triangle(const vec2& a, const vec2& b, const vec2& c) {
  return eq(angle(b.minus(a), c.minus(a)), numbers::pi/2) ||
    eq(angle(a.minus(b), c.minus(b)), numbers::pi/2) ||
    eq(angle(a.minus(c), b.minus(c)), numbers::pi/2);
}

constexpr uint N = 50;

int main() {
  uint count = 0;

  vec2 a{0,0};
  for (int y1 = 1; y1 <= N; y1++) {
    for (int x1 = 0; x1 <= N; x1++) {
      vec2 b{x1, y1};
      for (int y2 = 0; y2 <= y1; y2++) {
	for (int x2 = max(1, x1); x2 <= N; x2++) {
	  if (x1 == x2 && y1 == y2) {
	    continue;
	  }
	  vec2 c{x2, y2};
	  count += is_right_triangle(a, b, c);
	}
      }
    }
  }

  cout << count << endl;
}
