package effectivejava.test.samples;

public class ClassThatDoesNotOverrideToString {

    private final int a;

    public ClassThatDoesNotOverrideToString(int a) {
        this.a = a;
    }

    public int getA() {
        return a;
    }

}
