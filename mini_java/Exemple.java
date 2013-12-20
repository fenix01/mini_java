class A{
	A(){
		
	}
	void move(int n, int startPole, int endPole) {
	    if (n== 0){
	      return; 
	    }
	    int intermediatePole = 6 - startPole - endPole;
	    move(n-1, startPole, intermediatePole);
	    System.out.print("Move ");
	    System.out.print(n);
	    System.out.print(" from ");
	    System.out.print(startPole);
	    System.out.print(" to ");
	    System.out.print(endPole);
	    System.out.print("\n");
	    move(n-1, intermediatePole, endPole);
	  }
}

public class Exemple {

  public static void main(String _ [])
    {
	  A a = new A();
      a.move(100, 1, 3);
    }
}
