

class Rational {
      int numerator;
   // int numerator = 1; C'est vrai en Java mais illegal en ce projet pcq on ne definit pas la grammaire!! comment faire?
    int denominator;
    void setNumerator(int a ){
        int c = f(Math.abs(a), denominator);
        numerator = a/c;
        denominator = denominator / c;
        if(numerator <0 && denominator <0){
            numerator = -numerator;
            denominator = -denominator;
        }
    }
    
    void setDenominator(int b) {
        int c = f(numerator, Math.abs(b));
        numerator = numerator /c ;
        denominator = b/c ;
        if(numerator < 0 && denominator < 0){
            numerator = -numerator;
            denominator = -denominator;
        }
    }
    
    int getNumerator(){
        return numerator;
    }
    
    int getDenominator(){
        return denominator;
    }
    
    int f(int a, int b){
        if (a == 0) return 1;
        if (a < b){
            int c = a;
            a = b;
            b = c;
        }
        int r = a % b;
        for(r;r != 0;r++){
            a = b;
            b = r;
            r = a % b ;
        }
        return b;
    }
        
        Rational add( Rational r){
            int a = r.getNumerator();
            int b = r.getDenominator();
            int newNumerator = numerator * b + denominator * a;
            int newDenominator = denominator * b;
            Rational result = new Rational();
            result.setNumerator(newNumerator);
            result.setDenominator(newDenominator);
            return result;
        }
        
        
        Rational sub (Rational r ){
            int a = r.getNumerator();
            int b = r.getDenominator();
            int newNumerator = numerator * b - denominator * a;
            int newDenominator = denominator * b;
            Rational result = new Rational();
            result.setNumerator(newNumerator);
            result.setDenominator(newDenominator);
            return result;
        }
        
        Rational muti(Rational r ){
            int a = r.getNumerator();
            int b = r.getDenominator();
            int newNumerator = numerator * a ;
            int newDenominator = denominator * b;
            Rational result = new Rational();
            result.setNumerator(newNumerator);
            result.setDenominator(newDenominator);
            return result;
        }
    
        Rational div(Rational r ){
            int a = r.getNumerator();
            int b = r.getDenominator();
            int newNumerator = numerator * a ;
            int newDenominator = denominator * b;
	    a = (Integer)b;
            Rational result = new Rational();
            result.setNumerator(newNumerator);
            result.setDenominator(newDenominator);
            return result;
        }

}


public class Exemple_Rational {
    public static void main (String args[]){
        Rational r1 = new Rational();
        r1.setNumerator(1);
        r1.setDenominator(5);
        
        Rational r2 = new Rational();
        r2.setNumerator(3);
        r2.setDenominator(2);
        
        Rational result = r1.add(r2);
        int a = result.getNumerator();
        int b = result.getDenominator();
        System.out.println("1/5 + 3/2 = " + a + "/" + b);
    }
}
