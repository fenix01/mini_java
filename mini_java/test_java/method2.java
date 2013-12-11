class B extends A {

  B () {}
  void f () { System.out.print ("f de B\n"); }
  void g () { System.out.print ("g de B\n"); }
  void i () { System.out.print ("i de B\n"); }
  void j () { System.out.print ("j de B\n"); }
}

class A {

  A () {}
  void f () { System.out.print ("f de A\n"); }
  void g () { System.out.print ("g de A\n"); }
  void h () { System.out.print ("h de A\n"); }
  void i () { System.out.print ("i de A\n"); }
}


public class method2 {

  public static void main(String _[])
    {
      A a1 = new A ();
      A a2 = new B ();
      B b  = new B ();

      a1.f();
      a1.g();
      a1.h();
      a1.i();
      a2.f();
      a2.g();
      a2.h();
      a2.i();
      b.f();
      b.g();
      b.h();
      b.i();
      b.j();
    }
}
