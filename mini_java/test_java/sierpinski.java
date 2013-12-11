class Triangle {

  int maxi;
  int maxj;

  Triangle (int i, int j) { maxi = i; maxj = j; }
  boolean common_bits(int i, int j) {
    for (;i !=0 || j !=0;) {

      if ((i % 2) + (j % 2) ==2) return true;
      i = i / 2;
      j = j / 2;
    };
    return false;
  }

  void draw () {
    int i;
    int j;
    for(j = 0; j < maxj; j++) {
      for(i = 0; i < maxi ; i++){
        if (common_bits(i ,j )) System.out.print(" ");
        else System.out.print("#");
      }
      System.out.print("\n");
    }

  }
}

public class sierpinski {

  public static void main(String _ []) {
    Triangle t = new Triangle (32, 32);
    t.draw ();
  }

}
