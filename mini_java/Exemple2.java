https://github.com/fenix01/mini_java.gitclass A {
int x;
int y;
A () { }
void f () {
System.out.print (1);
}
void g (int x) { }
}

class B extends A {
int x;
int z;
B () { }
void f () {
System.out.print (2);
}
void g (int x, boolean b) { }
int h (int x) { return 42; }
}

public class Exemple2 {
	public static void main (String argv[]){
		A a1 = new A();
		A a2 = new B();
		a1.f();
		a2.f();

	}
}