// Monte Carlo simulation (technically MCMC I guess but not
// Metropolis-Hastings or Gibbs).

#include <chrono>
#include <iostream>
#include <random>
// #include <utility>
using namespace std;

// Number of rounds to simulate.
constexpr uint num_turns = 10000000;

// Number of sides of dice.
constexpr uint N = 6;

constexpr uint GO = 0;
constexpr uint JAIL = 10;
constexpr uint FP = 20;
constexpr uint G2J = 30;

// Community chest squares.
constexpr uint CC1 = 2;
constexpr uint CC2 = 17;
constexpr uint CC3 = 33;

// Chance squares.
constexpr uint CH1 = 7;
constexpr uint CH2 = 22;
constexpr uint CH3 = 36;

constexpr uint R1 = 5;
constexpr uint R2 = 5;
constexpr uint R3 = 5;
constexpr uint C1 = 11;
constexpr uint E3 = 24;
constexpr uint H2 = 39;
constexpr uint U1 = 12;
constexpr uint U2 = 28;

constexpr uint next_railway(uint pos) {
  switch (pos) {
  case CH1:
    return R2;
  case CH2:
    return R3;
  case CH3:
    return R1;
  default:
    cerr << "next_railway error" << endl;
    exit(-1);
  }
}

constexpr uint next_utility(uint pos) {
  switch (pos) {
  case CH1:
  case CH3:
    return U1;
  case CH2:
    return U2;
  default:
    cerr << "next_railway error" << endl;
    exit(-1);
  }
}

template <typename T, uint n> uint argmax(T arr[n]) {
  static_assert(n > 0, "argmax: n must be a positive integer.");
  uint max_i = 0;
  for (uint i = 0; i < n; i++) {
    if (arr[i] > arr[max_i]) {
      max_i = i;
    }
  }
  return max_i;
}

template <typename T, uint n>
void shuffle(T arr[n]) {
  mt19937 rng(chrono::system_clock::now().time_since_epoch().count());
  for (uint i = 0; i < n-1; i++) {
    swap(arr[i], arr[uniform_int_distribution<uint>(0, n-1-i)(rng)]);
  }
}

uint CC_cards[16] = {};
uint CH_cards[16] = {};

void init_cards() {
  for (uint i = 0; i < 16; i++) {
    CC_cards[i] = i;
    CH_cards[i] = i;
  }
}

int main() {
  init_cards();
  mt19937 rng(chrono::system_clock::now().time_since_epoch().count());
  uniform_int_distribution<uint> dieN(1, N);
  uniform_int_distribution<uint> die16(0, 15);
  
  // Array of counts for each board square, all initialized to zero.
  uint counts[40] = {};

  // Current board position of the player.
  uint pos = 0;

  // Current number of consecutive double rolls.
  uint doubles_count = 0;

  for (uint i = 0; i < num_turns; i++) {
    // Simulate a single step.
    uint roll1 = dieN(rng);
    uint roll2 = dieN(rng);

    if (roll1 == roll2) {
      if (++doubles_count == 3) {
        pos = JAIL; // Go to JAIL.
        doubles_count = 0;
      }
    }
    else {
      doubles_count = 0;
      pos = (pos + roll1 + roll2) % 40;

      if (pos == G2J) {
        pos = JAIL; // Go to JAIL.
      } else if (pos == CC1 || pos == CC2 || pos == CC3) {
        uint n = die16(rng);
        if (n < 2) {
          // Go to either GO or JAIL with equal probability.
          pos = n < 1 ? GO : JAIL;
        }
      } else if (pos == CH1 || pos == CH2 || pos == CH3) {
        uint n = die16(rng);
        if (n < 6) {
          static uint arr[] = { GO, JAIL, C1, E3, H2, R1 };
          pos = arr[n];
        } else if (n < 8) {
          pos = next_railway(pos);
        } else if (n < 9) {
          pos = next_utility(pos);
        } else if (n < 10) {
          pos = (pos + 37) % 40; // Move back three squares.
        }
      }
    }
    
    counts[pos]++;
  }

  for (uint i = 0; i < 40; i++) {
    cout << i << ": " << counts[i] / (float)num_turns << endl;
  }

  // uint first = argmax<uint, 40>(counts);
  // counts[first] = 0;
  // uint second = argmax<uint, 40>(counts);
  // counts[second] = 0;
  // uint third = argmax<uint, 40>(counts);
  // cout << first << " " << second << " " << third << endl;

  cout << endl;
  for (uint i = 0; i < 10; i++) {
    uint k = argmax<uint, 40>(counts);
    cout << k << " ";
    counts[k] = 0;
  }
  cout << endl;

  // uint arr[] = { 1, 2, 3, 4, 5 };
  // for (uint i = 0; i < 5; i++) {
  //   cout << arr[i] << " ";
  // }
  // cout << endl;
  // for (uint i = 0; i < 1000000; i++) {
  //   shuffle<uint, 5>(arr);
  // }
  // for (uint i = 0; i < 5; i++) {
  //   cout << arr[i] << " ";
  // }
  // cout << endl;
  
}
