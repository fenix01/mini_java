public class test_Bloc {
    public static void main (String args[]){
    	int n = 0;
    	int sum = 1 ;
    	int i ;
    	if (n > 0){
    	for ( i = 1; i <=n ; i++){
			sum = (int)sum * i;
			}
    	
        System.out.print ("factorielle " + n + " = " + sum + "\\"
        		+ "\n" );
    	}
    	else 
    	System.out.println("Error!");
    }
    
}


