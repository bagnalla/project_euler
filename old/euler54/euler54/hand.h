#include <vector>

class hand
{
public:
	vector<card> cards;
	vector<vector<card>> matches;
	int handValue, lowPair, highPair, highSize;

	hand(string str)
	{
		for (int i = 0; i < 10; i+=2)
		{
			card temp(str.substr(i, 2));
			cards.push_back(temp);
		}
		sort();
		evaluate();
	}

	string string()
	{
		std::string result = "[";
		for (int i = 0; i < cards.size(); i++)
		{
			result += cards[i].string();
			if (i != cards.size()-1)
				result += ", ";
		}
		result += "]";
		return result;
	}

	void sort()
	{
		int lowest;
		for (int i = 0; i < cards.size(); i++)
		{
			lowest = i;
			for (int s = i; s < cards.size(); s++)
			{
				if (cards[s].value < cards[lowest].value)
					lowest = s;
			}
			swap(cards[i], cards[lowest]);
		}
	}

	void erase(card c)
	{
		for (int i = 0; i < cards.size(); i++)
		{
			if (c.equals(cards[i]))
				cards.erase(cards.begin()+i);
		}
	}

	void evaluate()
	{
		bool suited = false;
		if (cards[0].suit == cards[1].suit && cards[0].suit == cards[2].suit && 
			cards[0].suit == cards[3].suit && cards[0].suit == cards[4].suit)
			suited = true;
		
		int temp;
		for (int i = 0; i < cards.size(); i++)
		{
			temp = cards[i].value;
			vector<card> count;
			for (int s = 0; s < cards.size(); s++)
			{
				if (cards[s].value == temp)
					count.push_back(cards[s]);
			}
			if (count.size() >= 2)
			{
				matches.push_back(count);
				for (int s = 0; s < count.size(); s++)
					erase(count[s]);
				i = 0;
			}
		}

		lowPair = highPair = 0;
		highSize = 0;
		if (matches.size())
		{
			for (int i = 0 ; i < matches.size(); i++)
			{
				if (matches[i][0].value < lowPair && matches[i].size() <= 2)
					lowPair = matches[i][0].value;
				if (matches[i][0].value > highPair && matches[i].size() > highSize)
				{
					highPair = matches[i][0].value;
					highSize = matches[i].size();
				}
			}
		}

		if (!matches.size() && cards[0].value == 10 && cards[1].value == 11 && 
			cards[2].value == 12 && cards[3].value == 13 && cards[4].value == 14 && suited)
		{
			handValue = ROYALFLUSH;
		}
		else if (!matches.size() && cards[0].value+1 == cards[1].value && cards[0].value+2 == cards[2].value
			&& cards[0].value+3 == cards[3].value && cards[0].value+4 == cards[4].value && suited)
		{
			handValue = STRAIGHTFLUSH;
		}
		else if (matches.size() && matches[0].size() == 4)
		{
			handValue = FOUROFAKIND;
		}
		else if (matches.size() == 2 && (matches[0].size() == 3 || matches[1].size() == 3))
		{
			handValue = FULLHOUSE;
		}
		else if (suited)
		{
			handValue = FLUSH;
		}
		else if (!matches.size() && cards[0].value+1 == cards[1].value && cards[0].value+2 == cards[2].value
			&& cards[0].value+3 == cards[3].value && cards[0].value+4 == cards[4].value)
		{
			handValue = STRAIGHT;
		}
		else if (matches.size() && matches[0].size() == 3)
		{
			handValue = THREEOFAKIND;
		}
		else if (matches.size() == 2)
		{
			handValue = TWOPAIRS;
		}
		else if (matches.size() == 1)
		{
			handValue = ONEPAIR;
		}
		else
			handValue = HIGHCARD;
	}

	int compareTo(hand h)
	{
		if (handValue > h.handValue)
			return 1;
		if (handValue < h.handValue)
			return -1;

		if (handValue == ONEPAIR)
		{
			if (highPair > h.highPair)
				return 1;
			if (highPair < h.highPair)
				return -1;
		}
		else if (handValue == TWOPAIRS)
		{
			if (highPair > h.highPair)
				return 1;
			if (highPair < h.highPair)
				return -1;
			if (lowPair > h.lowPair)
				return 1;
			if (lowPair < h.lowPair)
				return -1;
		}
		else if (handValue == THREEOFAKIND)
		{
			if (highPair > h.highPair)
				return 1;
			if (highPair < h.highPair)
				return -1;
		}
		else if (handValue == FULLHOUSE)
		{
			cout << h.highPair << endl;
			if (highPair > h.highPair)
				return 1;
			if (highPair < h.highPair)
				return -1;
			if (lowPair > h.lowPair)
				return 1;
			if (lowPair < h.lowPair)
				return -1;
		}
		else if (handValue == FOUROFAKIND)
		{
			if (highPair > h.highPair)
				return 1;
			if (highPair < h.highPair)
				return -1;
		}


		while (cards.size() && h.cards.size())
		{
			if (cards[cards.size()-1].value > h.cards[h.cards.size()-1].value)
				return 1;
			if (cards[cards.size()-1].value < h.cards[h.cards.size()-1].value)
				return -1;
			cards.pop_back();
			h.cards.pop_back();
		}
	}
};