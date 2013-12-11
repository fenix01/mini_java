class A {
  int x;
  int y;
  A () { }
  void f () {
    System.out.print ("Je suis A\n");
  }
  void g (int x) { }
}

class B extends A {
  int x;
  int z;
  B () { }
  void f () {
    System.out.print ("Je suis B\n");
  }
  void g (int x, boolean b) { }
  int h (int x) { return 42;}
}

public class exemple {
 public static void main (String s []) {
          A a1 = new A();
          A a2 = new B();
	  a2.f();
          a1.f();

}
}
