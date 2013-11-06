class People extends Peoples{
	int leg;
	int hand;
	int age;
	String name;
	People(String s){
		name = s;
		this.init();
	}
	
	int setAge(int a){
		this.age = a ;
		return age;
	}
	
	int setAge(int a, int b){
		this.age = a ;
		return age;
	}
	
	void init(){
	leg = 2;
	hand = 2;
	System.out.println(name + " has " + hand + " hands " + leg +" legs ");
	}
}

class Peoples{

//classe en cours de développement
/*
date:01/10/2013
*/
}

public class Peoples{
	public static void main (String args[]){
		People Jayden = new People("Jayden");
		Jayden.init();
		Jayden.setAge(10);
		System.out.println("Jayden is "+Jayden.age +" years old \n");
	}
    }
