package interfacegraphique;
import java.awt.*;
public class MenuEditeur extends MenuBar {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public MenuItem menuQuitter, menuNouveau, menuAPropos;
	public MenuEditeur(ZoneGraphique zone) {
		Menu menuFichier = new Menu("Fichier");
		menuNouveau = new MenuItem("Nouveau");
		menuQuitter = new MenuItem("Quitter");
		menuFichier.add(menuNouveau);
		menuFichier.addSeparator();
		menuFichier.add(menuQuitter);
		Menu menuAide = new Menu("Aide");
		menuAPropos = new MenuItem("APropos");
		menuAide.add(menuAPropos);
		add(menuFichier);
		setHelpMenu(menuAide);
		menuQuitter.addActionListener(
			new GereGraphique(GereGraphique.QUITTER,zone));
		menuNouveau.addActionListener(
			new GereGraphique(GereGraphique.NOUVEAU,zone));
		menuAPropos.addActionListener(
			new GereGraphique(GereGraphique.APROPOS,zone));
	}
}
