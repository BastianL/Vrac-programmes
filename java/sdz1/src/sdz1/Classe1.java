package sdz1;

import java.util.Scanner;

public class Classe1 {

public static void main(String[] args) {
	String str1 = new String("coucou"), str2 = new String("toutou");
	if(!str1.equals(str2))
		System.out.println("les deux chaines sont identiques !");
	else
		System.out.println("Les deux chaines sont différentes");
}
public static double arrondi(double A, int B) {
	return (double) ( (int) (A * Math.pow(10, B) + .5)) / Math.pow(10, B);
}

}