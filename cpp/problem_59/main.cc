#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
using namespace std;

// Split string by single character delimiter.
// https://stackoverflow.com/a/46931770
vector<string> split (const string& s, char delim) {
  vector<string> result;
  stringstream ss (s);
  string item;
  while (getline (ss, item, delim)) {
    result.push_back (item);
  }
  return result;
}

string decode(const string& text, const string& key) {
  string result(text);
  for (size_t i = 0; i < result.length(); i++) {
    result[i] ^= key[i % key.length()];
  }
  return result;
}

int main() {
  ifstream t("0059_cipher.txt");
  stringstream buf;
  buf << t.rdbuf();
  string text = "";
  for (string s : split(buf.str(), ',')) {
    text += (char)stoi(s);
  }

  for (char a = 'a'; a <= 'z'; a++) {
    for (char b = 'a'; b <= 'z'; b++) {
      for (char c = 'a'; c <= 'z'; c++) {
        string key = {a, b, c};
        string decoded = decode(text, key);
        if (decoded.find("approximately") != string::npos) {
          cout << "key: " << key << endl;
          cout << "decoded text: " << decoded << endl;
          int sum = 0;
          for (char c : decoded) {
            sum += (int)c;
          }
          cout << "sum: " << sum << endl;
        }
      }
    }
  }
}
