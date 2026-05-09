package partie2;

public class Commande {
	public String produit;
	public short quantite;
	public float prixHT;
	public char codeTVA;
	private static float tableTVA[ ] = { 0.055f, 0.206f, 0.25f };
	public boolean estReduite() {
		return (quantite * prixHT) > 10000;
	}
	private int indexTVA() {
		int index;
		if (codeTVA < 'A') erreur("code TVA invalide");
		if (codeTVA > 'C') erreur("code TVA invalide");
		index = codeTVA - 'A';
		return index;
	}
	public void afficheToi() {
		System.out.println("commande:" + produit + "," + quantite + "," + tableTVA[indexTVA()] *100 + "%");
	}
	private void erreur(String msgErreur) {
		System.out.println(msgErreur);
		System.exit(1);
	}
}
