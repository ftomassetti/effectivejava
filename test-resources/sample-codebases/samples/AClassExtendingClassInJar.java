import com.github.javaparser.ast.expr.AnnotationExpr;

class AClassExtendingClassInJar extends AnnotationExpr {

	public void aMethod(){
		// name refers to the field from AnnotaionExpr: protected NameExpr name;
		System.out.println(name);
	}

}
