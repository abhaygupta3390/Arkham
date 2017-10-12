package codePractice;

import java.util.Scanner;

/**
 * Created by test on 12/10/17.
 */
public class printEgyptian {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        long num = in.nextLong();
        long den = in.nextLong();

        printEgyptian(num, den);
    }

    private static void printEgyptian(long num, long den) {
        if (num  ==  0 || den == 0 || num >= den) {
            return;
        }
        if (den % num == 0) {
            System.out.print(den/num);
            return;
        }

        long n = den / num + 1;

        long newNum = num * n - den;
        long newDen = den * n;


        System.out.print(n + " ");
        printEgyptian(num * n - den, den * n);
    }
}
