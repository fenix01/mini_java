class A {

  void f () {
    System.out.print("Je suis A");
  }
}

class B  extends A {
  B () { }
  void f () {
    System.out.print ("Je suis B");
  }
}

public class method {
  public static void main(String _ [])
    {
      A b = new B();
      b.f();
    }
}
