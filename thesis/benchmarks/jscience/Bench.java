import org.jscience.physics.amount.Amount;
import javax.measure.quantity.Length;
import javax.measure.quantity.Area;
import javax.measure.unit.SI;
import java.lang.instrument.Instrumentation;

public class Bench {
  static final int BATCH_SIZE = 5000000;
  private long time = -1;

  private void time(String name) {
    stop();
    System.out.print(String.format("%30s took: \t", name));
    time = (new java.util.Date()).getTime();;
  }

  private void stop() {
    if(time != -1) {
      long elapsed = (new java.util.Date()).getTime() - time;
      System.out.println(String.format("%d ms, %.2f ns each", elapsed, elapsed * 1e6 / BATCH_SIZE));
    }
    time = -1;
  }

  public static void main(String args[]) {
    Bench b = new Bench();
    b.run();
  }

  public void run() {
    System.out.println("Starting benchmarks ...");

    initMeasures();
    initPrimitives();

    for(int i = 0; i < 5; i++) {
      benchMeasures();
      benchPrimitives();
      System.out.println();
    }
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

  private void benchMeasures() {
    time("addition");
    for(int i = 0; i < BATCH_SIZE; ++i) {
      c_m[i] = a_m[i].plus(b_m[i]);
    }

    time("multiplication");
    for(int i = 0; i < BATCH_SIZE; ++i) {
      d_m[i] = (Amount<Area>) a_m[i].times(b_m[i]);
    }

    stop();
  }

  double[] a = new double[BATCH_SIZE];
  double[] b = new double[BATCH_SIZE];
  double[] c = new double[BATCH_SIZE];
  double[] d = new double[BATCH_SIZE];
  private void initPrimitives() {
    for(int i = 0; i < BATCH_SIZE; ++i) {
      a[i] = i;
      b[i] = i;
    }
  }

  private void benchPrimitives() {
    time("primitives - addition");
    for(int i = 0; i < BATCH_SIZE; ++i) {
      c[i] = a[i] + b[i];
    }

    time("primitives - multiplication");
    for(int i = 0; i < BATCH_SIZE; ++i) {
      d[i] =  a[i] * b[i];
    }

    stop();
  }
}
