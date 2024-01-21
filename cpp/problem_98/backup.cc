#include <cmath>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <unordered_map>
#include <vector>

using namespace std;

using permutation = vector<uint>;

vector<string> read_lines(const string &path) {
  vector<string> lines;
  ifstream fs(path);
  for (string line; getline(fs, line);) {
    lines.push_back(line);
  }
  return lines;
}

// Split string by single character delimiter.
// https://stackoverflow.com/a/46931770
vector<string> split(const string &s, char delim) {
  vector<string> result;
  stringstream ss (s);
  string item;
  while (getline (ss, item, delim)) {
    result.push_back (item);
  }
  return result;
}

constexpr uint num_digits(ulong n) {
  return ceil(log10(n+1));
}

unordered_map<uint, vector<ulong>> squares;

void init_squares(ulong upper_bound) {
  for (ulong i = 0; i*i < upper_bound; i++) {
    squares[num_digits(i*i)].push_back(i*i);
  }
}

bool is_anagram(const string &a, const string &b) {
  return false;
}

// Compute permutation mapping (permuting a into b), succeeding only
// if b is actually a permutation of a.
optional<permutation> perm(const string &a, const string &b) {
  if (a.empty()) {
    if (b.empty()) {
      return {{}};
    } else {
      return {};
    }
  }
  
  for (uint i = 0; i < b.size(); i++) {
    if (a[0] == b[i]) {
      string b_prime(b);
      b_prime.erase(i, 1);
      auto p_opt = perm(a.substr(1, a.length()-1), b_prime);
      if (p_opt.has_value()) {
        auto p = p_opt.value();
        for (uint j = 0; j < p.size(); j++) {
          if (p[j] >= i) {
            p[j]++;
          }
        }
        p.insert(p.begin(), i);
        return {p};
      } else {
        return {};
      }
    }
  }
  
  return {};
}

// Apply permutation [p] to string [s].
string apply_perm(const permutation &p, const string &s) {
  string result = "";
  for (const uint i : p) {
    result.push_back(s[i]);
  }
  return result;
}

unordered_map<char, vector<uint>> char_ixs(const string &s) {
  unordered_map<char, vector<uint>> m;
  for (uint i = 0; i < s.length(); i++) {
    m[s[i]].push_back(i);
  }
  return m;
}

bool is_square(ulong n) {
  const auto d = num_digits(n);
  return binary_search(squares[d].cbegin(), squares[d].cend(), n);
}

// bool is_square(ulong n) {
//   double root = sqrt(n);
//   return floor(root) == root;
// }

template <typename T>
constexpr bool vector_contains(const vector<T> &v, const T &x) {
  return find(v.begin(), v.end(), x) != v.end();
}

// Look for square anagrams that correspond to a word anagram pair.
vector<pair<ulong, ulong>> find_square_anagrams(const permutation &p,
                                                const unordered_map<char, vector<uint>> &ixs) {
  // cout << "searching for squares with " << p.size() << " digits" << endl;
  vector<pair<ulong, ulong>> square_anagrams;
  
  for (const ulong n : squares[p.size()]) {
    const string s = to_string(n);

    // First we must check that n satisfies the constraints imposed by
    // ixs. That is, the digits at indices corresponding to a
    // particular character in the original string must all be equal
    // to one another and not equal to the digits at other indices.
    for (const auto pair : ixs) {
      // Get indices corresponding to current character.
      const vector<uint> c_ixs = pair.second;

      // Scan through the string of digits.
      for (uint i = 0; i < s.length(); i++) {
        // If this digit should correspond to current character, check
        // that it's equal to the first such digit (goto next if not equal).
        if (vector_contains(c_ixs, i)) {
          if (s[i] != s[c_ixs[0]]) {
            goto next;
          }
        }
        // Else if this digit should *not* correspond to current
        // character, check that it's not equal to the first such
        // digit (goto next if equal).
        else {
          if (s[i] == s[c_ixs[0]]) {
            goto next;
          }
        }
      }
    }
    
    {
      const string s_prime = apply_perm(p, s);
      const ulong n_prime = stoull(s_prime);
      if (num_digits(n_prime) == s_prime.length() && is_square(n_prime)) {
        square_anagrams.push_back({n, n_prime});
      }
    }

  next:{}
  }
  
  return square_anagrams;
}

int main() {
  auto lines = read_lines("words.txt");

  auto words = split(lines[0], ',');
  unordered_map<uint, vector<string>> words_by_length;

  for (uint i = 0; i < words.size(); i++) {
    words[i] = words[i].substr(1, words[i].length() - 2);
  }

  for (const string& word : words) {
    words_by_length[word.length()].push_back(word);
  }

  uint longest = 0;
  for (const auto wbl : words_by_length) {
    if (wbl.first > longest) {
      longest = wbl.first;
    }
  }
  
  init_squares(pow(10, longest));

  vector<tuple<string, string, permutation>> anagrams;
  for (const auto wbl : words_by_length) {
    auto words = wbl.second;
    for (uint i = 0; i < words.size(); i++) {
      for (uint j = i+1; j < words.size(); j++) {
        auto p = perm(words[i], words[j]);
        if (p.has_value()) {
          anagrams.push_back({words[i], words[j], p.value()});
        }
      }
    }
  }

  ulong largest = 0;
  
  for (const auto t : anagrams) {
    string w1 = get<0>(t);
    string w2 = get<1>(t);
    permutation p = get<2>(t);

    auto ixs = char_ixs(w2);
    auto square_anagrams = find_square_anagrams(p, ixs);
    for (const auto p : square_anagrams) {
      if (p.first > largest) {
        largest = p.first;
      }
      if (p.second > largest) {
        largest = p.second;
      }
    }
  }

  cout << largest << endl;
}
