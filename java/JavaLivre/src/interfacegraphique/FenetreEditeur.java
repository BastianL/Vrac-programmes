package interfacegraphique;
import java.awt.*;
public class FenetreEditeur extends Frame {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	final static int HTAILLE = 750;
	final static int VTAILLE = 450;
	public FenetreEditeur() {
		BarreEtat etat;
		ZoneGraphique graphique;
		setTitle("Editeur graphique");
		setSize(HTAILLE, VTAILLE);
		setBackground(Color.darkGray);
		setLayout(new BorderLayout (2,2));
		add(etat = new BarreEtat(),"South");
		add(graphique = new ZoneGraphique(etat),"Center");
		add(new BarreOutils(graphique),"North");
		setMenuBar(new MenuEditeur(graphique));
		setVisible(true);
		addWindowListener(new GestionnaireFenetre());
	}
	public Insets getInsets() {
		Insets normal = super.getInsets();
		return new Insets(normal.top+2, normal.left, normal.bottom, normal.right);
	}
}
