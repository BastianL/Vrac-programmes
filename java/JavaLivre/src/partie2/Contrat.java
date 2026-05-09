package partie2;

public class Contrat {
	private float solde;
	private float tauxPrelevement = 0.02f;
	public void credite (int versement) {
		float frais;
		frais = versement * tauxPrelevement;
		solde = solde + versement - frais;
	}
	public void afficheToi() {
		System.out.println(
		"Contrat(" + solde + " , " + tauxPrelevement + ")");
	}
	public void afficheToi(byte index) {
		System.out.println("commande:");
	}
	public static void neDoublepas(float x) { x = 2 * x; }
	public static void rembourse (Contrat expire) {
		expire.solde = 0;
	}
	public static void main (String args[ ]) {
		Contrat assurance = new Contrat();
		int montantDuCheque = 1000;
		assurance.credite(montantDuCheque);
		Contrat.neDoublepas(assurance.solde);
		rembourse(assurance);
	}
}
