#include <iostream>
#include <string>
#include <vector>
using namespace std;

// vector<vector<string>> splittings(string s) {
//   vector<vector<string>> result = { { s } };
//   if (s.length() <= 1) {
//     return result;
//   }
//   for (size_t i = 1; i < s.length(); i++) {
//     string s1 = s.substr(0, i);
//     auto splits = splittings(s.substr(i));
//     // transform(splits.cbegin(), splits.cend(), splits.begin(),
//     //           [s1](vector<string> split) {
//     //             split.insert(split.begin(), s1);
//     //             return split;
//     //           });
//     for (size_t j = 0; j < splits.size(); j++) {
//       splits[j].push_back(s1);
//     }
//     result.insert(result.end(), splits.begin(), splits.end());
//   }
//   return result;
// }

vector<unsigned long long int> sums(string s) {
  vector<unsigned long long int> result = { { stoull(s) } };
  if (s.length() <= 1) {
    return result;
  }
  for (size_t i = 1; i < s.length(); i++) {
    unsigned long long int n = stoull(s.substr(0, i));
    auto ms = sums(s.substr(i));
    // transform(splits.cbegin(), splits.cend(), splits.begin(),
    //           [s1](vector<string> split) {
    //             split.insert(split.begin(), s1);
    //             return split;
    //           });
    for (size_t j = 0; j < ms.size(); j++) {
      ms[j] += n;
    }
    result.insert(result.end(), ms.begin(), ms.end());
  }
  return result;
}

// Is n^2 an S-number?
bool is_S_number(unsigned long long int n) {
  string s = to_string(n*n);
  // cout << s << endl;
  if (s.length() <= 1) {
    return false;
  } else {
    // for (size_t i = 1; i < s.length(); i++) {
    //   string s1 = s.substr(0, i);
    //   string s2 = s.substr(i);
    //   // cout << s1 << ", " << s2 << endl;
    //   if (stol(s1) + stol(s2) == n) {
    //     return true;
    //   }
    // }
    
    // auto splits = splittings(s);
    
    // splits.erase(splits.begin());
    // for (auto split : splits) {
    //   unsigned long long int sum = 0;
    //   for (auto s : split) {
    //     // cout << s << " ";
    //     sum += stol(s);
    //   }
    //   // cout << endl;
    //   if (sum == n) {
    //     return true;
    //   }
    // }
    
    // for (size_t i = 1; i < splits.size(); i++) {
    //   unsigned long long int sum = 0;
    //   for (auto s : split) {
    //     sum += stol(s);
    //   }
    //   if (sum == n) {
    //     return true;
    //   }
    // }

    auto ns = sums(s);
    for (size_t i = 1; i < ns.size(); i++) {
      if (ns[i] == n) {
        return true;
      }
    }
    
    return false;
  }
}

int main() {
  
  unsigned long long int sum = 0;
  for (unsigned long long int i = 1; i <= 1000000; i++) {
  // for (int i = 1; i*i <= 10*10*10*10; i++) {
    if (is_S_number(i)) {
      sum += i*i;
    }
  }
  cout << sum << endl;

  // auto splits = splittings("bakowabunga");
  // for (auto split : splits) {
  //   for (auto s : split) {
  //     cout << s << " ";
  //   }
  //   cout << endl;
  // }
}
