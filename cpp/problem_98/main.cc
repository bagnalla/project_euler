// This one is a bit complicated but *fast*. First, we find all pairs
// of word anagrams and for each pair construct a permutation mapping
// that transforms one word into the other. Then, for each such
// anagram pair we scan the square numbers with same number of digits
// as characters in the words and 1) check that there's a one-to-one
// correspondence between characters and digits, and 2) apply the
// permutation to the square and check that the result is also a
// square.

#include <cmath>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <unordered_map>
#include <vector>

using namespace std;

// Split string by single character delimiter.
// https://stackoverflow.com/a/46931770
vector<string> split(const string &s, char delim) {
  vector<string> result;
  stringstream ss(s);
  string item;
  while (getline(ss, item, delim)) {
    result.push_back(item);
  }
  return result;
}

constexpr uint num_digits(ulong n) {
  return ceil(log10(n+1));
}

// A permutation mapping p between strings a and b has p[i] = j iff
// a[i] = b[j]. It encodes a way to transform b into a (see apply_perm).
using permutation = vector<uint>;

// Apply permutation [p] to string [s].
string apply_perm(const permutation &p, const string &s) {
  string result = "";
  for (const uint i : p) {
    result.push_back(s[i]);
  }
  return result;
}

// Construct permutation mapping between strings a and b by induction
// on a, succeeding only if b is actually a permutation of a.
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

// Build mapping from characters to indices at which they appear in
// string [s].
unordered_map<char, vector<uint>> char_ixs(const string &s) {
  unordered_map<char, vector<uint>> m;
  for (uint i = 0; i < s.length(); i++) {
    m[s[i]].push_back(i);
  }
  return m;
}

bool is_square(ulong n) {
  double root = sqrt(n);
  return floor(root) == root;
}

template <typename T>
constexpr bool vector_contains(const vector<T> &v, const T &x) {
  return find(v.begin(), v.end(), x) != v.end();
}

// Look for square anagrams that correspond to a word anagram pair.
vector<pair<ulong, ulong>>
find_square_anagrams(const permutation &p,
                     const unordered_map<char, vector<uint>> &ixs) {
  vector<pair<ulong, ulong>> square_anagrams;

  ulong lb = (ulong)ceil(sqrt(pow(10, p.size()-1)));
  ulong ub = (ulong)sqrt(pow(10, p.size()));

  // For all squares [n = k*k] with [p.size()] digits.
  for (uint k = lb; k <= ub; k++) {
    const ulong n = k*k;
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

    // If we've made it this far, we know that the digits in [n] can
    // be put in one-to-one correspondence with characters in one of
    // the words in the anagram pair, so it remains only to apply the
    // permutation to [n] and check if the result is also a square
    // (and has the same number of digits as [n], i.e., no leading zeros).
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
  // Read in words from file.
  ifstream fs("words.txt");
  string line;
  getline(fs, line);
  auto words = split(line, ',');

  // Trim off quotation marks.
  for (uint i = 0; i < words.size(); i++) {
    words[i] = words[i].substr(1, words[i].length() - 2);
  }

  // Organize the words by number of characters.
  unordered_map<uint, vector<string>> words_by_length;
  for (const string& word : words) {
    words_by_length[word.length()].push_back(word);
  }

  // Search for anagram pairs.
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

  // For each anagram pair, look for corresponding square anagram
  // pairs and remember the largest square encountered.
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
