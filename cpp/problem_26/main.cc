#include <iostream>
#include <vector>
using namespace std;

int main() {
  uint max_count = 0;
  uint max_d = 0;
  for (uint d = 2; d < 1000; d++) {
    uint numerator = 1;
    uint count = 0;
    vector<bool> seen(d + 1, false);
    while (!seen[numerator]) {
      seen[numerator] = true;
      count++;
      double expansion = numerator / (float)d * 10;
      numerator = numerator * 10 - (uint)expansion * d;      
      if (numerator == 0) {
        goto next;
      }
    }
    if (count > max_count) {
      max_count = count;
      max_d = d;
    }
  next: {}
  }
  cout << "max_count: " << max_count << endl;
  cout << "max_d: " << max_d << endl;
}
