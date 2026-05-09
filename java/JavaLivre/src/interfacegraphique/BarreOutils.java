package interfacegraphique;
import java.awt.*;
public class BarreOutils extends Panel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public BarreOutils(ZoneGraphique graphique) {
		Bouton boutonDefaire, boutonEffacer, boutonQuitter;
		Choice listeCouleur;
		String libelleCouleurs[] = 
			{"Bleu", "Rouge", "Jaune", "Vert"};
		setBackground(Color.lightGray);
		listeCouleur = new Choice();
		for (int i = 0; i < libelleCouleurs.length; i++)
			listeCouleur.addItem(libelleCouleurs [ i]);
		listeCouleur.select(0);
		add(listeCouleur);
		add(boutonDefaire = new Bouton("Defaire"));
		add(boutonEffacer = new Bouton("Effacer tout"));
		add(boutonQuitter = new Bouton("Quitter"));
		boutonDefaire.addActionListener(
			new GereGraphique(GereGraphique.DEFAIRE, graphique));
		boutonEffacer.addActionListener(
			new GereGraphique(GereGraphique.NOUVEAU, graphique));
		boutonQuitter.addActionListener(
			new GereGraphique(GereGraphique.QUITTER, graphique));
			
	}
	public BarreOutils() {
		// TODO Auto-generated constructor stub
	}

}
