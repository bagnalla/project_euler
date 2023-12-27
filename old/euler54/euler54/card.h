#include <string>

class card
{
public:
	int value, suit;

	card() {}
	card(string str)
	{
		switch (str[0])
		{
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			value = (int)str[0]-48;
			break;
		case  'T':
			value = 10;
			break;
		case 'J':
			value = JACK;
			break;
		case 'Q':
			value = QUEEN;
			break;
		case 'K':
			value = KING;
			break;
		case 'A':
			value = ACE;
			break;
		default:
			value = 0;
		}

		switch (str[1])
		{
		case 'D':
			suit = DIAMONDS;
			break;
		case 'H':
			suit = HEARTS;
			break;
		case 'C':
			suit = CLUBS;
			break;
		case 'S':
			suit = SPADES;
			break;
		default:
			suit = 0;
		}
	}
	string string()
	{
		return toString(value) + " of " + toString(suit);
	}
	bool equals(card c)
	{
		return (this->value == c.value && this->suit == c.suit);
	}
	int compareTo(card c)
	{
		if (equals(c))
			return 0;
		if (this->value > c.value)
			return 1;
		return -1;
	}
};