using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace euler_31
{
    class Program
    {
        const int MAX_MONEY = 200;

        static void Main(string[] args)
        {
            System.Console.WriteLine(two_euro(0));
            System.Console.ReadLine();
        }

        static int two_euro(int current_money)
        {
            int combinations = 0;

            for (int i = 0; i <= MAX_MONEY - current_money; i += 200)
            {
                int temp_combos = one_euro(current_money + i);
                if (temp_combos > 0)
                    combinations += temp_combos;
                else
                    combinations++;
            }

            return combinations;
        }

        static int one_euro(int current_money)
        {
            int combinations = 0;

            for (int i = 0; i <= MAX_MONEY - current_money; i += 100)
            {
                int temp_combos = fifty_cent(current_money + i);
                if (temp_combos > 0)
                    combinations += temp_combos;
                else
                    combinations++;
            }

            return combinations;
        }

        static int fifty_cent(int current_money)
        {
            int combinations = 0;

            for (int i = 0; i <= MAX_MONEY - current_money; i += 50)
            {
                int temp_combos = twenty_cent(current_money + i);
                if (temp_combos > 0)
                    combinations += temp_combos;
                else
                    combinations++;
            }

            return combinations;
        }

        static int twenty_cent(int current_money)
        {
            int combinations = 0;

            for (int i = 0; i <= MAX_MONEY - current_money; i += 20)
            {
                int temp_combos = ten_cent(current_money + i);
                if (temp_combos > 0)
                    combinations += temp_combos;
                else
                    combinations++;
            }

            return combinations;
        }

        static int ten_cent(int current_money)
        {
            int combinations = 0;

            for (int i = 0; i <= MAX_MONEY - current_money; i += 10)
            {
                int temp_combos = five_cent(current_money + i);
                if (temp_combos > 0)
                    combinations += temp_combos;
                else
                    combinations++;
            }

            return combinations;
        }

        static int five_cent(int current_money)
        {
            int combinations = 0;

            for (int i = 0; i <= MAX_MONEY - current_money; i += 5)
            {
                int temp_combos = two_cent(current_money + i);
                if (temp_combos > 0)
                    combinations += temp_combos;
                else
                    combinations++;
            }

            return combinations;
        }

        static int two_cent(int current_money)
        {
            int combinations = 0;

            for (int i = 0; i <= MAX_MONEY - current_money; i += 2)
            {
                int temp_combos = one_cent(current_money + i);
                if (temp_combos > 0)
                    combinations += temp_combos;
                else
                    combinations++;
            }

            return combinations;
        }

        static int one_cent(int current_money)
        {
            int combinations = 0;

            if (current_money < MAX_MONEY)
                combinations++;

            return combinations;
        }
    }
}
