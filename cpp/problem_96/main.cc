// Straightforward backtracking search using std::bitset for
// efficiency. Could be optimized by doing a pure deduction phase
// first (filling in digits for which there's only one option,
// repeating until fixed point) which would simplify the backtracking
// search or in some cases eliminate it entirely, but this is already
// fast enough (solves all 50 puzzles in about 0.3s in my Linux VM).

#include <bitset>
#include <fstream>
#include <iostream>
#include <optional>
#include <vector>

using namespace std;

using board = vector<vector<uint>>;

vector<string> read_lines(const string& path) {
  vector<string> lines;
  ifstream fs(path);
  for (string line; getline(fs, line);) {
    lines.push_back(line);
  }
  return lines;
}

// Indices of next element to solve for. Moves left-to-right and wraps
// around to the next row.
constexpr pair<uint, uint> next_ixs(uint x, uint y) {
  return y < 8 ? pair{ x, y+1 } : pair{ x+1, (uint)0 };
}

// Attempt to solve board b starting from row x and column y (moving
// right and down, assuming everything to the left and above is
// already solved).
optional<board> solve(const board& b, uint x, uint y) {
  if (x >= 9) {
    return b;
  }

  auto next_xy = next_ixs(x, y);

  if (b[x][y] != 0) {
    return solve(b, next_xy.first, next_xy.second);
  }
  
  board new_b(b);
  
  // Initialize set of available digits to {1, 2, 3, 4, 5, 6, 7, 8, 9}.
  bitset<10> digits(0b1111111110);

  // Scan current row, removing encountered digits from the set.
  for (uint j = 0; j < 9; j++) {
    digits[b[x][j]] = false;
  }

  // Scan current column, removing encountered digits from the set.
  for (uint i = 0; i < 9; i++) {
    digits[b[i][y]] = false;
  }

  // Scan current 9x9 block, removing encountered digits from the set.
  for (uint i = 0; i < 3; i++) {
    for (uint j = 0; j < 3; j++) {
      digits[b[x/3*3 + i][y/3*3 + j]] = false;
    }
  }

  // The remaining digits are solution candidates.
  for (uint i = 1; i < 10; i++) {
    if (digits[i]) {
      new_b[x][y] = i;
      auto solution = solve(new_b, next_xy.first, next_xy.second);
      if (solution.has_value()) {
        return solution;
      }
    }
  }
  
  return {};
}

int main() {
  // Read file to vector of strings.
  vector<string> lines = read_lines("sudoku.txt");

  // Build boards from input strings.
  vector<board> boards;
  for (uint i = 0; i < lines.size() / 10; i++) {
    board b;
    for (uint j = 0; j < 9; j++) {
      vector<uint> row;
      for (uint k = 0; k < 9; k++) {
        row.push_back(lines[i*10+1+j][k] - '0');
      }
      b.push_back(row);
    }
    boards.push_back(b);
  }

  uint sum = 0;
  
  for (const auto b : boards) {
    auto sol = solve(b, 0, 0);
    if (sol.has_value()) {
      auto b = sol.value();
      sum += b[0][0] * 100 + b[0][1] * 10 + b[0][2];
    } else {
      cout << "NO SOLUTION!" << endl;
    }
  }
  
  cout << sum << endl;
}
