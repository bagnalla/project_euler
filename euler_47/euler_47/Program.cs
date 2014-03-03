using System;
using System.Collections.Generic;
using System.Numerics;
using System.Diagnostics;

namespace euler_47
{
    class Program
    {
        static List<BigInteger> numbers = new List<BigInteger>();
        static List<int> primes = new List<int>();
        static Stopwatch stopWatch = new Stopwatch();

        static void Main(string[] args)
        {
            stopWatch.Start();
            generatePrimes(125);

            for (int a = 0; a < primes.Count; a++)
            {
                for (int b = a + 1; b < primes.Count; b++)
                {
                    for (int c = b + 1; c < primes.Count; c++)
                    {
                        for (int d = c + 1; d < primes.Count; d++)
                        {
                            addProduct(primes[a], primes[b], primes[c], primes[d]);
                            addProduct(BigInteger.Pow(primes[a], 2), primes[b], primes[c], primes[d]);
                            addProduct(primes[a], BigInteger.Pow(primes[b], 2), primes[c], primes[d]);
                            addProduct(primes[a], primes[b], BigInteger.Pow(primes[c], 2), primes[d]);
                            addProduct(primes[a], primes[b], primes[c], BigInteger.Pow(primes[d], 2));
                        }
                    }
                }
            }

            numbers.Sort();

            for (int i = 0; i < numbers.Count - 3; i++)
            {
                if (numbers[i + 1] == numbers[i] + 1 &&
                    numbers[i + 2] == numbers[i] + 2 &&
                    numbers[i + 3] == numbers[i] + 3)
                {
                    System.Console.WriteLine(numbers[i] + ", " + numbers[i + 1] + ", " + numbers[i + 2] + ", " + numbers[i + 3]);
                }
            }

            System.Console.WriteLine(stopWatch.ElapsedMilliseconds / 1000f + " seconds");
            System.Console.ReadLine();
        }

        static void generatePrimes(int n)
        {
            if (n > 0)
                primes.Add(2);
            if (n == 1)
                return;

            int count = 1;
            for (int i = 3; ; i += 2)
            {
                if (isPrime(i))
                {
                    primes.Add(i);
                    if (++count >= n)
                        return;
                }
            }
        }

        static bool isPrime(int n)
        {
            for (int i = 2; i * i <= n; i++)
                if (n % i == 0)
                    return false;
            return true;
        }

        static void addProduct(BigInteger a, BigInteger b, BigInteger c, BigInteger d)
        {
            BigInteger n = a * b * c * d;

            if (n < 150000)
                numbers.Add(n);
        }
    }
}
