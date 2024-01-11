// Surprisingly simple solution given how difficult the problem is
// supposed to be. We observe that the length of the shortest path is
// just the length of the hypotenuse of the triangle with sides equal
// to the largest of the cuboid side lengths and the sum of the two
// smaller side lengths (6 and 5+3=8 in the problem example). So, to
// answer whether the length of the shortest path for a cuboid with
// side lengths a, b, and c (where a>=b>=c) is an integer we can
// simply ask if a²+(b+c)² is a square. Thus to find the final answer
// we enumerate all a, b, c such that a>=b>=c and count how many
// satisfy the above property, stopping when the count exceeds one
// million. Runs in about 2.3s on my machine.

#include <cmath>
#include <iostream>
using namespace std;

bool is_square(uint n) {
  double root = sqrt(n);
  return floor(root) == root;
}

int main() {

  uint count = 0;

  for (uint a = 1;; a++) {
    for (uint b = 1; b <= a; b++) {
      for (uint c = 1; c <= b; c++) {
	if (is_square(a*a + (b+c)*(b+c))) {
	  count++;
	}
      }
    }
    if (count >= 1000000) {
      cout << "M = " << a << endl;
      break;
    }
  }

  cout << count << " cuboids" << endl;
}
