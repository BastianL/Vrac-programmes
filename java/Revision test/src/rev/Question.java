package rev;

import java.io.Serializable;
import java.util.Scanner;

public class Question implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -2058774677163540452L;
	private String enoncé;
	private String reponse;
	public Question (String enoncé, String reponse) {
		this.enoncé = enoncé;
		this.reponse = reponse;
	}
	public boolean poserQuestion () {
		System.out.println (enoncé);
		Scanner sc = new Scanner(System.in);
		String c = sc.next();
		if (c.equalsIgnoreCase(reponse)) {
			System.out.println ("Bravo! Bonne réponse");
			return true;
		}
		else { System.out.println ("Mauvaise réponse");
		System.out.println("voici la bonne réponse :");
		System.out.println(reponse);
		return false;}
	}
	public void MontrerQuestion() {
		System.out.println(enoncé);
		System.out.println("la reponse est");
		System.out.println(reponse);
	}
	
}
