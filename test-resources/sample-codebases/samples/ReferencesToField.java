package sample

class AClassResolvingToLocalVarClassType  {
	
	class A {
		
	}
	
	class B {

	}

	public void method1(){
		A i;
		i = 2;
	}

	public void method2(){
		A i;
		if (true){
			B i;
			i = 2;
		}
	}	

}
