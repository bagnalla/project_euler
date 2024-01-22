// Simple but naive brute force that takes around four minutes to run
// in my Linux VM environment. I had deduced that the ratio of n/b was
// converging to sqrt(2) in the limit, and that that suggested a
// possible application of Diophantine/Pell's equations (which we can
// generate solutions for via continued fractions using code from
// previous problems), but I couldn't quite figure out how to
// transform [b/n * (b-1)/(n-1) = 1/2] into a suitable Pell equation
// that would lead to a solution. After finding this answer in this
// dumb way and consulting the problem thread on the Project Euler
// forum, I finally managed to deduce the Pell equation. See the
// corresponding (better) solution to this problem in
// ../ocaml/problem_100 -- it finds the answer very quickly.

#include <cmath>
#include <iostream>

using namespace std;

int main() {
  for (ulong b = (ulong)(1000000000000 / (long double)sqrt(2));; b++) {
    ulong n = (ulong)sqrt((long double)2.0 * b * b);
    if (2 * b * (b - 1) == n * (n - 1)) {
      cout << b << " " << n << endl;
      break;
    }
  }
}
