using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace euler_26
{
    class Program
    {
        static void Main(string[] args)
        {
            int longestRecurringCycle = 0;

            for (int i = 11; i < 1000; i++)
            {
                int recurringCycle = findRecurringCycleLength(i);
                if (recurringCycle > longestRecurringCycle)
                    longestRecurringCycle = recurringCycle;
            }

            Console.WriteLine(longestRecurringCycle.ToString());
            Console.ReadLine();
        }

        static int findRecurringCycleLength(int number)
        {
            //string numString = Decimal.Round((1 / (decimal)number), 28).ToString();
            string numString = (1 / (decimal)number).ToString();

            System.Numerics.Complex numerator = new System.Numerics.Complex(1, 1);
            System.Numerics.Complex denominator = new System.Numerics.Complex(number, number);

            System.Numerics.Complex muh = numerator / denominator;
            return 0;
        }
    }
}
