class fibonacci {

  fibonacci () {}
  int call (int n)
    {
      if (n <= 1) return n;
      else
       return  call(n-1) + call (n-2);
    }
}

public class fib {

  public static void main(String _ [])
    {
      fibonacci f = new fibonacci();
      int x = f.call(21);
      System.out.print(x);
      System.out.print("\n");
    }
}
