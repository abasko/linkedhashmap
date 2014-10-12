import java.util.LinkedHashMap;

public class Tests {
    static final int N = 100000;

    interface Test {
        void test();
    }

    public static void test1() {
        LinkedHashMap m = new LinkedHashMap();
        for (int i = 0; i < N; i++) {
            m.put(N-i, i);
        }
    }

    public static long[] run(int times, Test t) {
        long[] results = new long[times];

        for (int n = 0; n < times; n++) {
            long start = System.nanoTime();
            t.test();
            results[n] = System.nanoTime() - start;
        }

        return results;
    }

    public static double mean(long[] xs) {
        double mean = 0;
        for (int i = 0; i < xs.length; i++) {
            mean += xs[i];
        }
        mean /= xs.length;
        return mean / 1_000_000;
    }

    public static double stdvar(long[] xs) {
        long m1 = 0;
        long m2 = 0;
        for (int i = 0; i < xs.length; i++) {
            m1 += xs[i];
            m2 += xs[i] * xs[i];
        }
        m1 /= xs.length;
        m2 /= xs.length;

        return Math.sqrt(m2 - m1*m1) / 1_000_000;
    }

    public static void main(String[] args) {
        long[] result = run(1000, new Test() { public void test() { test1(); }});
        for (int i = 0; i < result.length; i++) {
            System.out.println(((double)result[i]) / 1_000_000 + " ms");
        }
        System.out.println("mean=" + mean(result) + " ms");
        System.out.println("std var=" + stdvar(result) + " ms");
    }    
}
