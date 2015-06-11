package app.test.samples;

public class ClassWhoseParentOverridesToString extends ParentClassThatOverridesToString {

    private final int a;

    public ClassWhoseParentOverridesToString (int a, int b) {
        super(b);
        this.a = a;
    }

    public int getA() {
        return a;
    }

}
