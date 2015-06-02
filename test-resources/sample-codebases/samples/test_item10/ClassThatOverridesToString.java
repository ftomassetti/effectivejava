package app.test.samples;

public class ClassThatOverridesToString {

    private final int a;

    public ClassThatOverridesToString(int a) {
        this.a = a;
    }

    public int getA() {
        return a;
    }

    @Override
    public String toString() {
        return "app.test.samples.ClassThatOverridesToString{" +
                "a=" + a +
                '}';
    }

}
