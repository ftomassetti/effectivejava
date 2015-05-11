package sample

class AClassResolvingToLocalVar  {

	public void method1(){
		int i;
		i = 2;
	}

	public void method2(){
		int i;
		if (true){
			long i;
			i = 2;
		}
	}	

}
