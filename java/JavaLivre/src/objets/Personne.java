package objets;

public class Personne {
	private final String pasDeSociete = "?";
	private String nom;
	private String societe;
	private String valideSociete(String sNom) {
		if (sNom.length() > 32 || sNom.equals("?")) {
			System.out.println("classe Personne, société incorrecte : " + sNom);
			System.exit(2);
		}
		return sNom;
	}
	private static String nomduFichier;
	public Personne (String leNom) {
		nom = leNom.toUpperCase();
		societe = new String("?");
	}
	public Personne (String leNom, String uneEntreprise) {
		nom = leNom.toUpperCase();
		societe = valideSociete(uneEntreprise).toUpperCase();
	}
	public String sonNom() { return nom; }
	public String saSociete() {
		if ( societe.equals(pasDeSociete)) {
			return new String ("societe inconnue");
		}
		else return societe;
	}
	public boolean estSalarie() {
		return !societe.equals(pasDeSociete);
	}
	public void quitteSaSociete() {
		if ( societe.equals(pasDeSociete) ) {
			presenteToi();
			System.out.println("impossible de quitter la société");
			System.exit(2);
		}
		societe = pasDeSociete;
	}
	public void vaDansSociete(String entreprise) {
		if ( ! societe.equals(pasDeSociete)) {
			presenteToi();
			System.out.println("erreur : 1 - quitteSaSociete, 2 - vaDansSociete ");
			System.exit(3);
		}
		societe = valideSociete(entreprise).toUpperCase();
	}	
	public void presenteToi() {
		System.out.println("Je m'appelle" + nom);
		System.out.println("Je travaille à" + societe);
	}

	public static void choisirFichier() {
		
	}
}
