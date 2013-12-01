class A {
	int x;
	int y;
	A (int a) { }
	void f (int a, boolean c) {
		if (c)
		System.out.print(a);
		else
		System.out.print(34);
	}
}

public class Exemple {
	public static void main (String argv[]){
		A a1 = new A(255);
		a1.f(2,true);
	}
}