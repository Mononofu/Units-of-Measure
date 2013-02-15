import org.jscience.physics.amount.Amount;
import javax.measure.quantity.Length;
import javax.measure.quantity.Area;
import javax.measure.unit.SI;
import java.lang.instrument.Instrumentation;

public class Bench {
  static final int BATCH_SIZE = 5000000;
  static final int BATCH_WARMUP = 3;
  static final int BATCH_NUM = 20;
  private long time = -1;

  private long time(String name) {
    long elapsed = stop();
    System.out.print(String.format("%30s took: \t", name));
    time = (new java.util.Date()).getTime();
    return elapsed;
  }

  private long stop() {
    long elapsed = (new java.util.Date()).getTime() - time;
    if(time != -1) {
      System.out.println(String.format("%d ms, %.2f ns each", elapsed, elapsed * 1e6 / BATCH_SIZE));
    }
    time = -1;
    return elapsed;
  }

  public static void main(String args[]) {
    Bench b = new Bench();
    b.run();
  }

  public void run() {
    System.out.println("Starting benchmarks ...");

    initMeasures();
    initPrimitives();
    initDoublePrimitives();

    for(int i = 0; i < BATCH_WARMUP; i++) {
      benchMeasures();
      benchPrimitives();
      initDoublePrimitives();
    }
    measure_add = 0;
    measure_mult = 0;
    int_add = 0;
    int_mult = 0;
    double_add = 0;
    double_mult = 0;


    for(int i = 0; i < BATCH_NUM; i++) {
      benchMeasures();
      benchPrimitives();
      initDoublePrimitives();
      System.out.println();
    }

    System.out.println(String.format("int: %f add, %f mult",
      int_add * 1e6 / BATCH_NUM / BATCH_SIZE, int_mult * 1e6 / BATCH_NUM / BATCH_SIZE));
    System.out.println(String.format("double: %f add, %f mult",
      double_add * 1e6 / BATCH_NUM / BATCH_SIZE, double_mult * 1e6 / BATCH_NUM / BATCH_SIZE));
    System.out.println(String.format("mes: %f add, %f mult",
      measure_add * 1e6 / BATCH_NUM / BATCH_SIZE, measure_mult * 1e6 / BATCH_NUM / BATCH_SIZE));
  }





  Amount<Length>[] a_m = (Amount<Length>[]) new Amount[BATCH_SIZE];
  Amount<Length>[] b_m = (Amount<Length>[]) new Amount[BATCH_SIZE];
  Amount<Length>[] c_m = (Amount<Length>[]) new Amount[BATCH_SIZE];
  Amount<Area>[] d_m = (Amount<Area>[]) new Amount[BATCH_SIZE];

  private void initMeasures() {
    for(int i = 0; i < BATCH_SIZE; ++i) {
      a_m[i] = Amount.valueOf((double) i, SI.METRE);
      b_m[i] = Amount.valueOf((double) i, SI.METRE);
    }
  }

  long measure_add = 0;
  long measure_mult = 0;
  private void benchMeasures() {
    time("addition");
    for(int i = 0; i < BATCH_SIZE; ++i) {
      c_m[i] = a_m[i].plus(b_m[i]);
    }

    measure_add += time("multiplication");
    for(int i = 0; i < BATCH_SIZE; ++i) {
      d_m[i] = (Amount<Area>) a_m[i].times(b_m[i]);
    }

    measure_mult += stop();
  }





  int[] a = new int[BATCH_SIZE];
  int[] b = new int[BATCH_SIZE];
  int[] c = new int[BATCH_SIZE];
  int[] d = new int[BATCH_SIZE];
  private void initPrimitives() {
    for(int i = 0; i < BATCH_SIZE; ++i) {
      a[i] = i;
      b[i] = i;
    }
  }

  long int_add = 0;
  long int_mult = 0;
  private void benchPrimitives() {
    time("primitives - addition");
    for(int i = 0; i < BATCH_SIZE; ++i) {
      c[i] = a[i] + b[i];
    }

    int_add += time("primitives - multiplication");
    for(int i = 0; i < BATCH_SIZE; ++i) {
      d[i] =  a[i] * b[i];
    }

    int_mult += stop();
  }





  double[] a_d = new double[BATCH_SIZE];
  double[] b_d = new double[BATCH_SIZE];
  double[] c_d = new double[BATCH_SIZE];
  double[] d_d = new double[BATCH_SIZE];
  private void initDoublePrimitives() {
    for(int i = 0; i < BATCH_SIZE; ++i) {
      a_d[i] = i;
      b_d[i] = i;
    }
  }

  long double_add = 0;
  long double_mult = 0;
  private void benchDoublePrimitives() {
    time("primitives - addition");
    for(int i = 0; i < BATCH_SIZE; ++i) {
      c_d[i] = a_d[i] + b_d[i];
    }

    double_add += time("primitives - multiplication");
    for(int i = 0; i < BATCH_SIZE; ++i) {
      d_d[i] =  a_d[i] * b_d[i];
    }

    double_mult += stop();
  }
}
