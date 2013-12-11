class Callable {

  void call(Object o) { return; }

}

class Print extends Callable {
  Print () { }
  void call(Object o) {
    if (o instanceof String) {
      String s = (String) o;
      System.out.print(s + "\n");
    }
  }
}

class List {
  Object data;
  boolean is_empty() { return false; }
  List next () { return null; }
  int length() { return 1 + this.next().length(); }
  Object data () { return data; }
  List cons(Object data) { return new Node (data, this); }
  void iter(Callable c) {

    if (!is_empty()) {
      c.call(data ());
      next().iter(c);
    };

  }
}
class Node extends List {
  List next;
  Node (Object d, List n) {
    next = n;
    data = d;
  }
  List next() { return next; }
}
class Nil extends List {
  Nil () {}
  int length () { return 0; }
  boolean is_empty () { return true; }
}

public class list {
  public static void main (String _ []) {
    List n = (new Nil()).cons("123").cons("456").cons("789");
    n.iter(new Print());
  }

}
