// Monte Carlo simulation (technically MCMC I guess but not
// Metropolis-Hastings or Gibbs). We model the card drawing mechanism
// accurately (reshuffling every 10000 turns) although I don't think
// it is necessary since it doesn't change the result vs. just using
// i.i.d. random draws with replacement but it does run a bit faster
// and consumes less entropy.

// Alvaro describes a really nice solution: "I computed a matrix
// M[j][k] with the probability of going from state j to state
// k. There are 120 states (40 squares x 3 "number of doubles"
// values). The eigenvector of the largest eigenvalue, normalized to
// add up to 1, gives you the probability of each state. To compute
// that, start with (1,1,1,...,1), multiply by M, normalize, rinse,
// repeat. This method converges quickly to very precise values of the
// probabilities."

#include <chrono>
#include <iostream>
#include <random>
using namespace std;

// Number of rounds to simulate.
constexpr uint num_shuffles = 10000;
constexpr uint turns_per_shuffle = 10000;

// Number of sides of dice.
constexpr uint N = 4;

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
constexpr uint R2 = 15;
constexpr uint R3 = 25;
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

static uint CC_cards[16] = {};
static uint CH_cards[16] = {};
static uint CC_index = 0;
static uint CH_index = 0;

void init_cards() {
  for (uint i = 0; i < 16; i++) {
    CC_cards[i] = i;
    CH_cards[i] = i;
  }
}

uint draw_CC() {
  uint card = CC_cards[CC_index];
  CC_index = (CC_index + 1) % 16;
  return card;
}

uint draw_CH() {
  uint card = CH_cards[CH_index];
  CH_index = (CH_index + 1) % 16;
  return card;
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
  
  for (uint i = 0; i < num_shuffles; i++) {
    shuffle<uint, 16>(CC_cards);
    shuffle<uint, 16>(CH_cards);
    
    for (uint j = 0; j < turns_per_shuffle; j++) {
      // Simulate a single step.
      uint roll1 = dieN(rng);
      uint roll2 = dieN(rng);
      // cout << roll1 << " " << roll2 << endl;
      
      if (roll1 == roll2) {
        if (++doubles_count == 3) {
	  // cout << "GO TO JAIL!\n";
          pos = JAIL;
          doubles_count = 0;
          goto next;
        }
      }
      else {
        doubles_count = 0;
      }
      
      pos = (pos + roll1 + roll2) % 40;
      
      if (pos == G2J) {
        pos = JAIL;
      } else if (pos == CC1 || pos == CC2 || pos == CC3) {
        uint card = draw_CC();
        if (card < 2) {
          // Go to either GO or JAIL with equal probability.
          pos = card < 1 ? GO : JAIL;
        }
      } else if (pos == CH1 || pos == CH2 || pos == CH3) {
        uint card = draw_CH();
        if (card < 6) {
          static uint arr[] = { GO, JAIL, C1, E3, H2, R1 };
          pos = arr[card];
        } else if (card < 8) {
          pos = next_railway(pos);
        } else if (card < 9) {
          pos = next_utility(pos);
        } else if (card < 10) {
          pos = (pos + 37) % 40; // Move back three squares.
        }
      }
      
    next:
      counts[pos]++;
    }
  }

  for (uint i = 0; i < 40; i++) {
    cout << i << ": " << counts[i] / (num_shuffles * (double)turns_per_shuffle) << endl;
  }
  cout << endl;
  for (uint i = 0; i < 3; i++) {
    uint k = argmax<uint, 40>(counts);
    cout << k;
    counts[k] = 0;
  }
  cout << endl;  
}
