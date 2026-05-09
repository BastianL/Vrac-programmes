package rev;
import java.util.Scanner;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.io.Serializable;
public class Rev implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -1394118289365752698L;
	public static void main(String[] args) 
	{
		ArrayList<Test> ListeTest= new ArrayList<Test>();
		Scanner sc = new Scanner(System.in);
		try {
			FileInputStream fileIn = new FileInputStream("tests");
		    ObjectInputStream ois = new ObjectInputStream(fileIn);
		    ListeTest = (ArrayList<Test>) ois.readObject();
		    ois.close();
		    fileIn.close();
		} catch (FileNotFoundException e) {
		    e.printStackTrace();
		} catch (IOException e) {
		    e.printStackTrace();
		} catch (ClassNotFoundException e) {
		    e.printStackTrace();
		}
		while (true) {
			System.out.println ("Que voulez vous faire ?");
			System.out.println ("Tappez 1 pour lancer un test");
			System.out.println ("tappez 2 pour modifier les tests");
			System.out.println("Tappez 3 pour fermer le programme");
			int i = 0;
			i = sc.nextInt();
			switch (i) 
			{
				case 1 : System.out.println ("Choisissez un test");
					for(int j = 1; j <= ListeTest.size(); j++) 
					{
						System.out.println(j);
						System.out.println(":");
						System.out.println(ListeTest.get(j-1).Appelnom());
					}
					System.out.println("entrez le numéro de votre test");
					i = sc.nextInt();
					if (i <= ListeTest.size()) {
						ListeTest.get(i-1).lancertest();
						break;}
					else {
						System.out.println("Entrez un nombre valable !");
						break;
					}
				case 2 : System.out.println ("Voulez vous 1- Créer un test, 2-Modifier un test ?, 3-Supprimer un test ?");
					i = sc.nextInt();
					switch(i) 
					{
						case 1 : ListeTest.add(creertest());
						System.out.println("test ajouté !");
						break;
						case 2 :System.out.println ("Choisissez un test");
						for(int j = 1; j <= ListeTest.size(); j++) 
						{
							System.out.println(j);
							System.out.println(":");
							System.out.println(ListeTest.get(j-1).Appelnom());
						}
						System.out.println("entrez le numéro de votre test");
						i = sc.nextInt();				
						if (i <= ListeTest.size())
							ListeTest.get(i-1).Modifiertest();
						break;
						case 3 :System.out.println("Entrez le test à supprimer");
						for(int j = 1; j <= ListeTest.size(); j++) 
						{
							System.out.println(j);
							System.out.println(":");
							System.out.println(ListeTest.get(j-1).Appelnom());
						}
						i=sc.nextInt();
						if (i <= ListeTest.size())
							ListeTest.remove(i-1);
						System.out.println("test supprimé");
						break;
					}
					break;
				case 3 : System.exit(0);
				default : System.out.println("entrez un nombre valable");
			}
			try 
			{
				FileOutputStream fileOut = new FileOutputStream("Tests");
			    ObjectOutputStream out = new ObjectOutputStream(fileOut);
			    out.writeObject(ListeTest);
			    out.close();
			    fileOut.close();
			    System.out.println("\nSerialisation terminée avec succès...\n");
			} 
			catch (FileNotFoundException e) 
			{
				e.printStackTrace();
			} 
			catch (IOException e) 
			{
				e.printStackTrace();
			}
		}
	}			
	public static Test creertest () 
	{
		int j = 0;
		ArrayList<Question> Tableau = new ArrayList<Question>();
		System.out.println ("Quel est le nom du test ?");
		Scanner sc = new Scanner(System.in);
		String nom = sc.next();
		sc.nextLine();
		do 
		{
			System.out.println("Entrez un enoncé");
			String c = sc.next();
			sc.nextLine();
			System.out.println("Entrez une réponse");
			String reponse = sc.next();
			sc.nextLine();
			Tableau.add(new Question(c, reponse));
			System.out.println("Voulez vous ajouter une nouvelle question ? 1 pour oui");
			try {
			j = sc.nextInt();
			}
			catch (java.util.InputMismatchException e) {
				e.printStackTrace();
				j = 1;
			}
			sc.nextLine();
		}
		while (j == 1);
		return (new Test (Tableau, nom));
	}
}