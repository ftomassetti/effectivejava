package effectivejava.test.samples;

public class ClassThatDeclaresToStringWithParams {

    private final int a;

    public ClassThatDeclaresToStringWithParams(int a) {
        this.a = a;
    }

    public int getA() {
        return a;
    }

    public String toString(int b) {
        return Integer.toString(b);
    }

}
