package rev;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Scanner;

public class Test implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -1079584523150027512L;
	public ArrayList<Question> ListeQuestion;
	public String nom;
	public void lancertest (){
		int Réponsecorrecte = 0;
		for (int j=1; j <= ListeQuestion.size(); j++ ) {
			System.out.println ("Voici la question");
			boolean temp = ListeQuestion.get(j-1).poserQuestion();
			if (temp == true) Réponsecorrecte++;}
		System.out.println("Vous avez");
		System.out.println(Réponsecorrecte);
		System.out.println("sur");
		System.out.println(ListeQuestion.size());
	}
	public Test(ArrayList<Question> ListeQuestion, String nom) {
		this.ListeQuestion = ListeQuestion;
		this.nom = nom;
	}
	public String Appelnom(){
		return nom;
	}
	public void Modifiertest() {
		// TODO Auto-generated method stub
		System.out.println("voici les questions :");
		for(int j = 1; j <= ListeQuestion.size(); j++) {
			System.out.println(j);
			ListeQuestion.get(j-1).MontrerQuestion();
		}		
		int continuer = 1;
		while (continuer == 1) {
		System.out.println("entrez la question que vous voulez modifier");
		Scanner sc = new Scanner (System.in);
		int i=sc.nextInt();
		if (i < ListeQuestion.size()+1){
			System.out.println("Entrez le nouvel énoncé");
			String enoncé = sc.next();
			System.out.println("Entrez la nouvelle réponse");
			String reponse = sc.next();
			ListeQuestion.set(i-1, (new Question(enoncé, reponse)));
			System.out.println("souhaitez vous modifier d'autre question ? Entrez 1 pour oui");
			continuer = sc.nextInt();
		}
		else {
			System.out.println("Entrez un nombre valide");
		}		
		}
	}		
}
